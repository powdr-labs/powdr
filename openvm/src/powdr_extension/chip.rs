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
use openvm_circuit::arch::ExecutionState;
use openvm_circuit::{
    arch::{ExecutionError, MatrixRecordArena, PreflightExecutor},
    system::memory::{online::TracingMemory, MemoryController},
};
use openvm_instructions::{instruction::Instruction, LocalOpcode};
use openvm_stark_backend::{
    p3_air::{Air, BaseAir}, p3_matrix::dense::DenseMatrix, prover::{hal::ProverBackend, types::AirProvingContext}, ChipUsageGetter
};

use openvm_stark_backend::{
    config::{StarkGenericConfig, Val},
    interaction::InteractionBuilder,
    p3_field::PrimeField32,
    p3_matrix::Matrix,
    rap::{AnyRap, BaseAirWithPublicValues, PartitionedBaseAir},
    Chip, 
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
        memory: Arc<Mutex<TracingMemory>>,
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

impl<F: PrimeField32> PreflightExecutor<F> for PowdrChip<F> {
    fn execute(
        &self,
        state: openvm_circuit::arch::VmStateMut<F, TracingMemory, MatrixRecordArena<F>>,
        instruction: &Instruction<F>,
    ) -> Result<(), ExecutionError> {
        let &Instruction { opcode, .. } = instruction;
        assert_eq!(opcode.as_usize(), self.opcode.global_opcode().as_usize());

        self.executor.execute(state)
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

impl<PB: ProverBackend<Matrix = DenseMatrix<Val<SC>>>, SC: StarkGenericConfig> Chip<SC, PB> for PowdrChip<Val<SC>>
where
    Val<SC>: PrimeField32,
{
    fn generate_proving_ctx(&self, records: SC) -> AirProvingContext<PB> {
        tracing::trace!("Generating air proof input for PowdrChip {}", self.name);

        let width = self.trace_width();
        let labels = [("apc_opcode", self.opcode.global_opcode().to_string())];
        metrics::counter!("num_calls", &labels).absolute(self.executor.number_of_calls() as u64);
        let trace = self.executor.generate_witness::<SC>();

        assert_eq!(trace.width(), width);

        AirProvingContext::simple(trace, vec![])
    }
}

pub struct PowdrAir<F> {
    /// The columns in arbitrary order
    columns: Vec<AlgebraicReference>,
    apc: Arc<Apc<F, Instr<F>>>,
}

// impl<F: PrimeField32> ColumnsAir<F> for PowdrAir<F> {
//     fn columns(&self) -> Option<Vec<String>> {
//         Some(self.columns.iter().map(|c| (*c.name).clone()).collect())
//     }
// }

impl<F: PrimeField32> PowdrAir<F> {
    pub fn new(apc: Arc<Apc<F, Instr<F>>>) -> Self {
        Self {
            columns: apc.machine().main_columns().collect(),
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

        let witness_evaluator = WitnessEvaluator::new(&witness_values);

        for constraint in &self.apc.machine().constraints {
            let constraint = witness_evaluator.eval_constraint(constraint);
            builder.assert_zero(constraint.expr);
        }

        for interaction in &self.apc.machine().bus_interactions {
            let interaction = witness_evaluator.eval_bus_interaction(interaction);
            // TODO: is this correct?
            let count_weight = 1;

            builder.push_interaction(
                interaction.id as u16,
                interaction.args,
                interaction.mult,
                count_weight,
            );
        }
    }
}

impl<F: PrimeField32> PartitionedBaseAir<F> for PowdrAir<F> {}
