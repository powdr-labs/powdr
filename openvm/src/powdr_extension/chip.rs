// Mostly taken from [this openvm extension](https://github.com/openvm-org/openvm/blob/1b76fd5a900a7d69850ee9173969f70ef79c4c76/extensions/rv32im/circuit/src/auipc/core.rs#L1)

use std::{
    collections::BTreeMap,
    sync::{Arc, Mutex},
};

use crate::{
    extraction_utils::OriginalAirs, powdr_extension::executor::PowdrPeripheryInstances,
    ExtendedVmConfig, Instr,
};

use super::{executor::PowdrExecutor, opcode::PowdrOpcode, PowdrPrecompile};
use itertools::Itertools;
use openvm_circuit::system::memory::MemoryController;
use openvm_circuit::{
    arch::{ExecutionState, InstructionExecutor, Result as ExecutionResult},
    system::memory::OfflineMemory,
};
use openvm_instructions::{instruction::Instruction, LocalOpcode};
use openvm_stark_backend::{
    p3_air::{Air, BaseAir},
    rap::ColumnsAir,
};

use openvm_stark_backend::{
    config::{StarkGenericConfig, Val},
    interaction::InteractionBuilder,
    p3_field::PrimeField32,
    p3_matrix::Matrix,
    prover::types::AirProofInput,
    rap::{AnyRap, BaseAirWithPublicValues, PartitionedBaseAir},
    Chip, ChipUsageGetter,
};
use powdr_autoprecompiles::{
    expression::{AlgebraicEvaluator, AlgebraicReference, WitnessEvaluator},
    Apc,
};

pub struct PowdrChip<F: PrimeField32> {
    pub name: String,
    pub opcode: PowdrOpcode,
    /// An "executor" for this chip, based on the original instructions in the basic block
    pub executor: PowdrExecutor<F>,
    pub air: Arc<PowdrAir<F>>,
}

impl<F: PrimeField32> PowdrChip<F> {
    pub(crate) fn new(
        precompile: PowdrPrecompile<F>,
        original_airs: OriginalAirs<F>,
        memory: Arc<Mutex<OfflineMemory<F>>>,
        base_config: ExtendedVmConfig,
        periphery: PowdrPeripheryInstances,
    ) -> Self {
        let PowdrPrecompile {
            name, opcode, apc, ..
        } = precompile;
        let air = Arc::new(PowdrAir::new(apc.clone()));
        let executor = PowdrExecutor::new(original_airs, memory, base_config, periphery, apc);

        Self {
            name,
            opcode,
            executor,
            air,
        }
    }
}

impl<F: PrimeField32> InstructionExecutor<F> for PowdrChip<F> {
    fn execute(
        &mut self,
        memory: &mut MemoryController<F>,
        instruction: &Instruction<F>,
        from_state: ExecutionState<u32>,
    ) -> ExecutionResult<ExecutionState<u32>> {
        let &Instruction { opcode, .. } = instruction;
        assert_eq!(opcode.as_usize(), self.opcode.global_opcode().as_usize());

        let execution_state = self.executor.execute(memory, from_state)?;

        Ok(execution_state)
    }

    fn get_opcode_name(&self, _: usize) -> String {
        self.name.clone()
    }
}

impl<F: PrimeField32> ChipUsageGetter for PowdrChip<F> {
    fn air_name(&self) -> String {
        format!("powdr_air_for_opcode_{}", self.opcode.global_opcode()).to_string()
    }
    fn current_trace_height(&self) -> usize {
        self.executor.number_of_calls()
    }

    fn trace_width(&self) -> usize {
        <PowdrAir<_> as BaseAir<_>>::width(&self.air)
    }
}

impl<SC: StarkGenericConfig> Chip<SC> for PowdrChip<Val<SC>>
where
    Val<SC>: PrimeField32,
{
    fn air(&self) -> Arc<dyn AnyRap<SC>> {
        self.air.clone()
    }

    fn generate_air_proof_input(self) -> AirProofInput<SC> {
        tracing::trace!("Generating air proof input for PowdrChip {}", self.name);

        let width = self.trace_width();
        let labels = [("apc_opcode", self.opcode.global_opcode().to_string())];
        metrics::counter!("num_calls", &labels).absolute(self.executor.number_of_calls() as u64);
        let trace = self
            .executor
            .generate_witness::<SC>(&self.air.column_index_by_poly_id);

        assert_eq!(trace.width(), width);

        AirProofInput::simple(trace, vec![])
    }
}

pub struct PowdrAir<F> {
    /// The columns in arbitrary order
    columns: Vec<AlgebraicReference>,
    /// The mapping from poly_id id to the index in the list of columns.
    /// The values are always unique and contiguous
    column_index_by_poly_id: BTreeMap<u64, usize>,
    apc: Arc<Apc<F, Instr<F>>>,
}

impl<F: PrimeField32> ColumnsAir<F> for PowdrAir<F> {
    fn columns(&self) -> Option<Vec<String>> {
        Some(self.columns.iter().map(|c| (*c.name).clone()).collect())
    }
}

impl<F: PrimeField32> PowdrAir<F> {
    pub fn new(apc: Arc<Apc<F, Instr<F>>>) -> Self {
        let (column_index_by_poly_id, columns): (BTreeMap<_, _>, Vec<_>) = apc
            .machine()
            .main_columns()
            .enumerate()
            .map(|(index, c)| ((c.id, index), c.clone()))
            .unzip();

        Self {
            columns,
            column_index_by_poly_id,
            apc,
        }
    }
}

impl<F: PrimeField32> BaseAir<F> for PowdrAir<F> {
    fn width(&self) -> usize {
        let res = self.columns.len();
        assert!(res > 0);
        res
    }
}

// No public values, but the trait is implemented
impl<F: PrimeField32> BaseAirWithPublicValues<F> for PowdrAir<F> {}

impl<AB: InteractionBuilder> Air<AB> for PowdrAir<AB::F>
where
    AB::F: PrimeField32,
{
    fn eval(&self, builder: &mut AB) {
        let main = builder.main();
        let witnesses = main.row_slice(0);
        // TODO: cache?
        let witness_values: BTreeMap<u64, AB::Var> = self
            .columns
            .iter()
            .map(|c| c.id)
            .zip_eq(witnesses.iter().cloned())
            .collect();

        let witness_evaluator = WitnessEvaluator::<AB::Var, AB::F, AB::Expr>::new(&witness_values);

        for constraint in &self.apc.machine().constraints {
            let e = witness_evaluator.eval_expr(&constraint.expr);
            builder.assert_zero(e);
        }

        for interaction in &self.apc.machine().bus_interactions {
            let powdr_autoprecompiles::SymbolicBusInteraction { id, mult, args, .. } = interaction;

            let mult = witness_evaluator.eval_expr(mult);
            let args = args
                .iter()
                .map(|arg| witness_evaluator.eval_expr(arg))
                .collect_vec();
            // TODO: is this correct?
            let count_weight = 1;

            builder.push_interaction(*id as u16, args, mult, count_weight);
        }
    }
}

impl<F: PrimeField32> PartitionedBaseAir<F> for PowdrAir<F> {}
