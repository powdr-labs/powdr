// Mostly taken from [this openvm extension](https://github.com/openvm-org/openvm/blob/1b76fd5a900a7d69850ee9173969f70ef79c4c76/extensions/rv32im/circuit/src/auipc/core.rs#L1)

use std::{
    collections::BTreeMap,
    sync::{Arc, Mutex},
};

use crate::{
    extraction_utils::OriginalAirs, powdr_extension::executor::PowdrPeripheryInstances,
    utils::algebraic_to_symbolic,
};

use super::{executor::PowdrExecutor, opcode::PowdrOpcode, PowdrPrecompile};
use itertools::Itertools;
use openvm_circuit::system::memory::MemoryController;
use openvm_circuit::{
    arch::{ExecutionState, InstructionExecutor, Result as ExecutionResult},
    system::memory::OfflineMemory,
};
use openvm_instructions::{instruction::Instruction, LocalOpcode};
use openvm_sdk::config::SdkVmConfig;
use openvm_stark_backend::{
    air_builders::symbolic::{
        symbolic_expression::{SymbolicEvaluator, SymbolicExpression},
        symbolic_variable::{Entry, SymbolicVariable},
    },
    interaction::BusIndex,
    p3_air::{Air, BaseAir},
    rap::ColumnsAir,
};

use openvm_stark_backend::{
    config::{StarkGenericConfig, Val},
    interaction::InteractionBuilder,
    p3_field::{Field, PrimeField32},
    p3_matrix::Matrix,
    prover::types::AirProofInput,
    rap::{AnyRap, BaseAirWithPublicValues, PartitionedBaseAir},
    Chip, ChipUsageGetter,
};
use powdr_autoprecompiles::expression::{AlgebraicExpression, AlgebraicReference};
use serde::{Deserialize, Serialize};

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
        base_config: SdkVmConfig,
        periphery: PowdrPeripheryInstances,
    ) -> Self {
        let PowdrPrecompile {
            machine,
            original_instructions,
            is_valid_column,
            name,
            opcode,
            ..
        } = precompile;
        let air = PowdrAir::new(machine);
        let executor = PowdrExecutor::new(
            original_instructions,
            original_airs,
            is_valid_column,
            memory,
            base_config,
            periphery,
        );

        Self {
            name,
            opcode,
            air: Arc::new(air),
            executor,
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
        <PowdrAir<_> as BaseAir<_>>::width(self.air.as_ref())
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
        let trace = self.executor.generate_witness::<SC>(
            &self.air.column_index_by_poly_id,
            &self.air.machine.bus_interactions,
        );

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
    machine: powdr_autoprecompiles::SymbolicMachine<F>,
}

impl<F: PrimeField32> ColumnsAir<F> for PowdrAir<F> {
    fn columns(&self) -> Option<Vec<String>> {
        Some(self.columns.iter().map(|c| (*c.name).clone()).collect())
    }
}

pub struct RowEvaluator<'a, F: PrimeField32> {
    pub row: &'a [F],
    pub witness_id_to_index: Option<&'a BTreeMap<u64, usize>>,
}

impl<'a, F: PrimeField32> RowEvaluator<'a, F> {
    pub fn new(row: &'a [F], witness_id_to_index: Option<&'a BTreeMap<u64, usize>>) -> Self {
        Self {
            row,
            witness_id_to_index,
        }
    }
}

impl<F: PrimeField32> SymbolicEvaluator<F, F> for RowEvaluator<'_, F> {
    fn eval_const(&self, c: F) -> F {
        c
    }

    fn eval_var(&self, symbolic_var: SymbolicVariable<F>) -> F {
        match symbolic_var.entry {
            Entry::Main {
                part_index: 0,
                offset: 0,
            } => {
                let index = if let Some(witness_id_to_index) = self.witness_id_to_index {
                    witness_id_to_index[&(symbolic_var.index as u64)]
                } else {
                    symbolic_var.index
                };
                self.row[index]
            }
            // currently only the current rotation of the main is supported
            // next rotation is not supported because this is a single row evaluator
            _ => unreachable!(),
        }
    }
    fn eval_is_first_row(&self) -> F {
        unreachable!()
    }
    fn eval_is_last_row(&self) -> F {
        unreachable!()
    }
    fn eval_is_transition(&self) -> F {
        unreachable!()
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(bound = "F: Field")]
pub struct SymbolicMachine<F> {
    columns: Vec<AlgebraicReference>,
    constraints: Vec<SymbolicConstraint<F>>,
    pub bus_interactions: Vec<SymbolicBusInteraction<F>>,
}

impl<F: PrimeField32> From<powdr_autoprecompiles::SymbolicMachine<F>> for SymbolicMachine<F> {
    fn from(machine: powdr_autoprecompiles::SymbolicMachine<F>) -> Self {
        let columns = machine.main_columns().collect();

        let powdr_autoprecompiles::SymbolicMachine {
            constraints,
            bus_interactions,
        } = machine;
        Self {
            columns,
            constraints: constraints
                .into_iter()
                .map(SymbolicConstraint::from)
                .collect(),
            bus_interactions: bus_interactions
                .into_iter()
                .map(SymbolicBusInteraction::from)
                .collect(),
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(bound = "F: Field")]
struct SymbolicConstraint<F> {
    expr: SymbolicExpression<F>,
}

impl<F: PrimeField32> From<powdr_autoprecompiles::SymbolicConstraint<F>> for SymbolicConstraint<F> {
    fn from(constraint: powdr_autoprecompiles::SymbolicConstraint<F>) -> Self {
        let powdr_autoprecompiles::SymbolicConstraint { expr } = constraint;
        Self {
            expr: algebraic_to_symbolic(&expr),
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(bound = "F: Field")]
pub struct SymbolicBusInteraction<F> {
    pub id: BusIndex,
    pub mult: SymbolicExpression<F>,
    pub args: Vec<SymbolicExpression<F>>,
    pub count_weight: u32,
}

impl<F: PrimeField32> From<powdr_autoprecompiles::SymbolicBusInteraction<F>>
    for SymbolicBusInteraction<F>
{
    fn from(bus_interaction: powdr_autoprecompiles::SymbolicBusInteraction<F>) -> Self {
        let powdr_autoprecompiles::SymbolicBusInteraction { id, mult, args, .. } = bus_interaction;
        let mult = algebraic_to_symbolic(&mult);
        let args = args.iter().map(algebraic_to_symbolic).collect();
        Self {
            id: id as BusIndex,
            mult,
            args,
            // TODO: Is this correct?
            count_weight: 1,
        }
    }
}

pub struct RangeCheckerSend<F> {
    pub mult: SymbolicExpression<F>,
    pub value: SymbolicExpression<F>,
    pub max_bits: SymbolicExpression<F>,
}

impl<F: PrimeField32> TryFrom<&powdr_autoprecompiles::SymbolicBusInteraction<F>>
    for RangeCheckerSend<F>
{
    type Error = ();

    fn try_from(i: &powdr_autoprecompiles::SymbolicBusInteraction<F>) -> Result<Self, Self::Error> {
        if i.id == 3 {
            assert_eq!(i.args.len(), 2);
            let value = &i.args[0];
            let max_bits = &i.args[1];
            Ok(Self {
                mult: algebraic_to_symbolic(&i.mult),
                value: algebraic_to_symbolic(value),
                max_bits: algebraic_to_symbolic(max_bits),
            })
        } else {
            Err(())
        }
    }
}

impl<F: PrimeField32> PowdrAir<F> {
    pub fn new(machine: powdr_autoprecompiles::SymbolicMachine<F>) -> Self {
        let (column_index_by_poly_id, columns): (BTreeMap<_, _>, Vec<_>) = machine
            .main_columns()
            .enumerate()
            .map(|(index, c)| ((c.id, index), c.clone()))
            .unzip();

        Self {
            columns,
            column_index_by_poly_id,
            machine,
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

        let witness_evaluator = WitnessEvaluator::<AB>::new(&witness_values);

        let eval_expr = |expr: &AlgebraicExpression<_>| {
            let symbolic_expr = algebraic_to_symbolic(expr);
            witness_evaluator.eval_expr(&symbolic_expr)
        };

        for constraint in &self.machine.constraints {
            let e = eval_expr(&constraint.expr);
            builder.assert_zero(e);
        }

        for interaction in &self.machine.bus_interactions {
            let powdr_autoprecompiles::SymbolicBusInteraction { id, mult, args, .. } = interaction;

            let mult = eval_expr(mult);
            let args = args.iter().map(&eval_expr).collect_vec();
            // TODO: is this correct?
            let count_weight = 1;

            builder.push_interaction(*id as u16, args, mult, count_weight);
        }
    }
}

pub struct WitnessEvaluator<'a, AB: InteractionBuilder> {
    pub witness: &'a BTreeMap<u64, AB::Var>,
}

impl<'a, AB: InteractionBuilder> WitnessEvaluator<'a, AB> {
    pub fn new(witness: &'a BTreeMap<u64, AB::Var>) -> Self {
        Self { witness }
    }
}

impl<AB: InteractionBuilder> SymbolicEvaluator<AB::F, AB::Expr> for WitnessEvaluator<'_, AB> {
    fn eval_const(&self, c: AB::F) -> AB::Expr {
        c.into()
    }

    fn eval_var(&self, symbolic_var: SymbolicVariable<AB::F>) -> AB::Expr {
        match symbolic_var.entry {
            Entry::Main { part_index, offset } => {
                assert_eq!(part_index, 0);
                assert_eq!(offset, 0);
                (*self.witness.get(&(symbolic_var.index as u64)).unwrap()).into()
            }
            Entry::Public => unreachable!("Public variables are not supported"),
            Entry::Challenge => unreachable!("Challenges are not supported"),
            Entry::Exposed => unreachable!("Exposed values are not supported"),
            Entry::Preprocessed { .. } => {
                unimplemented!("Preprocessed values are not supported yet")
            }
            Entry::Permutation { .. } => unreachable!("Permutation values are not supported"),
        }
    }

    fn eval_is_first_row(&self) -> AB::Expr {
        unimplemented!()
    }

    fn eval_is_last_row(&self) -> AB::Expr {
        unimplemented!()
    }

    fn eval_is_transition(&self) -> AB::Expr {
        unimplemented!()
    }
}

impl<F: PrimeField32> PartitionedBaseAir<F> for PowdrAir<F> {}
