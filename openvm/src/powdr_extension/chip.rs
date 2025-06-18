// Mostly taken from [this openvm extension](https://github.com/openvm-org/openvm/blob/1b76fd5a900a7d69850ee9173969f70ef79c4c76/extensions/rv32im/circuit/src/auipc/core.rs#L1)

use std::{
    collections::BTreeMap,
    sync::{Arc, Mutex},
};

use crate::{
    powdr_extension::executor::PowdrPeripheryInstances, traits::OpenVmField,
    utils::algebraic_to_symbolic, IntoOpenVm,
};

use super::{executor::PowdrExecutor, PowdrStackedPrecompile};
use itertools::Itertools;
use openvm_circuit::system::memory::MemoryController;
use openvm_circuit::{
    arch::{ExecutionState, InstructionExecutor, Result as ExecutionResult},
    system::memory::OfflineMemory,
    utils::next_power_of_two_or_zero,
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
    p3_matrix::dense::RowMajorMatrix,
    rap::ColumnsAir,
};

use openvm_stark_backend::{
    config::{StarkGenericConfig, Val},
    interaction::InteractionBuilder,
    p3_field::{Field, FieldAlgebra, PrimeField32},
    p3_matrix::Matrix,
    prover::types::AirProofInput,
    rap::{AnyRap, BaseAirWithPublicValues, PartitionedBaseAir},
    Chip, ChipUsageGetter,
};
use powdr_autoprecompiles::{
    expression::{AlgebraicExpression, AlgebraicReference},
    powdr::UniqueReferences,
};
use serde::{Deserialize, Serialize};

pub struct PowdrChip<P: IntoOpenVm> {
    pub name: String,
    /// An "executor" for each precompile stacked in this chip, by opcode.
    pub executors: BTreeMap<usize, PowdrExecutor<P>>,
    pub air: Arc<PowdrAir<P>>,
}

impl<P: IntoOpenVm> PowdrChip<P> {
    pub(crate) fn new(
        precompile: PowdrStackedPrecompile<P>,
        original_airs: BTreeMap<usize, powdr_autoprecompiles::SymbolicMachine<P>>,
        memory: Arc<Mutex<OfflineMemory<OpenVmField<P>>>>,
        base_config: SdkVmConfig,
        periphery: PowdrPeripheryInstances,
    ) -> Self {
        let air = PowdrAir::new(precompile.machine);

        let name = if precompile.precompiles.len() == 1 {
            // single precompile, just use its name
            precompile.precompiles.values().next().unwrap().name.clone()
        } else {
            // TODO: this name can be quite big depending on the number of precompiles
            format!(
                "StackedPrecompile_{}",
                precompile
                    .precompiles
                    .keys()
                    .map(|o| o.global_opcode())
                    .join("_")
            )
        };

        let executors = precompile
            .precompiles
            .into_iter()
            .map(|(opcode, pcp)| {
                let executor = PowdrExecutor::new(
                    pcp.original_instructions,
                    original_airs.clone(),
                    pcp.is_valid_column,
                    memory.clone(),
                    base_config.clone(),
                    periphery.clone(),
                );
                (opcode.global_opcode().as_usize(), executor)
            })
            .collect();

        Self {
            name,
            air: Arc::new(air),
            executors,
        }
    }
}

impl<P: IntoOpenVm> InstructionExecutor<OpenVmField<P>> for PowdrChip<P> {
    fn execute(
        &mut self,
        memory: &mut MemoryController<OpenVmField<P>>,
        instruction: &Instruction<OpenVmField<P>>,
        from_state: ExecutionState<u32>,
    ) -> ExecutionResult<ExecutionState<u32>> {
        let &Instruction { opcode, .. } = instruction;

        let execution_state = self
            .executors
            .get_mut(&opcode.as_usize())
            .expect("invalid opcode for stacked chip")
            .execute(memory, from_state)?;

        Ok(execution_state)
    }

    fn get_opcode_name(&self, _: usize) -> String {
        self.name.clone()
    }
}

impl<P: IntoOpenVm> ChipUsageGetter for PowdrChip<P> {
    fn air_name(&self) -> String {
        // TODO: this name can be quite big depending on the number of stacked precompiles
        format!("powdr_air_for_opcodes_{}", self.executors.keys().join("_"))
    }

    fn current_trace_height(&self) -> usize {
        self.executors.values().map(|e| e.number_of_calls()).sum()
    }

    fn trace_width(&self) -> usize {
        <PowdrAir<_> as BaseAir<_>>::width(self.air.as_ref())
    }
}

impl<SC: StarkGenericConfig, P: IntoOpenVm<Field = Val<SC>>> Chip<SC> for PowdrChip<P>
where
    Val<SC>: PrimeField32,
{
    fn air(&self) -> Arc<dyn AnyRap<SC>> {
        self.air.clone()
    }

    fn generate_air_proof_input(self) -> AirProofInput<SC> {
        tracing::trace!("Generating air proof input for PowdrChip {}", self.name);

        let num_records = self.current_trace_height();
        let height = next_power_of_two_or_zero(num_records);
        let width = self.trace_width();
        let mut values = Val::<SC>::zero_vec(height * width);
        let mut values_curr_record = 0;
        // this is just for sanity checking later
        let all_is_valid_ids = self
            .executors
            .values()
            .map(|executor| executor.is_valid_poly_id)
            .collect::<Vec<_>>();

        for (_opcode, executor) in self.executors {
            let executor_is_valid_id = executor.is_valid_poly_id;
            let executor_calls = executor.number_of_calls();
            let mut trace = executor.generate_witness::<SC>(
                &self.air.column_index_by_poly_id,
                &self.air.machine.bus_interactions,
            );

            // copy to main trace
            values
                .chunks_mut(width)
                .skip(values_curr_record)
                .zip(trace.rows_mut().take(executor_calls))
                .for_each(|(values_row, trace_row)| {
                    assert!(values_row.len() >= trace_row.len());
                    // copy the trace row to the main trace row
                    values_row.copy_from_slice(trace_row);

                    // check that only the correct is valid is set to ONE
                    for id in all_is_valid_ids.iter() {
                        if *id == executor_is_valid_id {
                            assert_eq!(
                                values_row[self.air.column_index_by_poly_id[id]],
                                <Val<SC>>::ONE
                            );
                        } else {
                            assert_eq!(
                                values_row[self.air.column_index_by_poly_id[id]],
                                <Val<SC>>::ZERO
                            );
                        }
                    }
                });
            values_curr_record += executor_calls;
        }

        let trace = RowMajorMatrix::new(values, width);
        AirProofInput::simple(trace, vec![])
    }
}

pub struct PowdrAir<P> {
    /// The columns in arbitrary order
    columns: Vec<AlgebraicReference>,
    /// The mapping from poly_id id to the index in the list of columns.
    /// The values are always unique and contiguous
    column_index_by_poly_id: BTreeMap<u64, usize>,
    machine: powdr_autoprecompiles::SymbolicMachine<P>,
}

impl<P: IntoOpenVm> ColumnsAir<OpenVmField<P>> for PowdrAir<P> {
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

impl<P: IntoOpenVm> From<powdr_autoprecompiles::SymbolicMachine<P>>
    for SymbolicMachine<OpenVmField<P>>
{
    fn from(machine: powdr_autoprecompiles::SymbolicMachine<P>) -> Self {
        let columns = machine.unique_references().collect();

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

impl<P: IntoOpenVm> From<powdr_autoprecompiles::SymbolicConstraint<P>>
    for SymbolicConstraint<OpenVmField<P>>
{
    fn from(constraint: powdr_autoprecompiles::SymbolicConstraint<P>) -> Self {
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

impl<P: IntoOpenVm> From<powdr_autoprecompiles::SymbolicBusInteraction<P>>
    for SymbolicBusInteraction<OpenVmField<P>>
{
    fn from(bus_interaction: powdr_autoprecompiles::SymbolicBusInteraction<P>) -> Self {
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

impl<P: IntoOpenVm> TryFrom<&powdr_autoprecompiles::SymbolicBusInteraction<P>>
    for RangeCheckerSend<OpenVmField<P>>
{
    type Error = ();

    fn try_from(i: &powdr_autoprecompiles::SymbolicBusInteraction<P>) -> Result<Self, Self::Error> {
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

impl<P: IntoOpenVm> PowdrAir<P> {
    pub fn new(machine: powdr_autoprecompiles::SymbolicMachine<P>) -> Self {
        let (column_index_by_poly_id, columns): (BTreeMap<_, _>, Vec<_>) = machine
            .unique_references()
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

impl<P: IntoOpenVm> BaseAir<OpenVmField<P>> for PowdrAir<P> {
    fn width(&self) -> usize {
        let res = self.columns.len();
        assert!(res > 0);
        res
    }
}

// No public values, but the trait is implemented
impl<P: IntoOpenVm> BaseAirWithPublicValues<OpenVmField<P>> for PowdrAir<P> {}

impl<AB: InteractionBuilder, P: IntoOpenVm<Field = AB::F>> Air<AB> for PowdrAir<P> {
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

impl<P: IntoOpenVm> PartitionedBaseAir<OpenVmField<P>> for PowdrAir<P> {}
