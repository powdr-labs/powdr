// Mostly taken from [this openvm extension](https://github.com/openvm-org/openvm/blob/1b76fd5a900a7d69850ee9173969f70ef79c4c76/extensions/rv32im/circuit/src/auipc/core.rs#L1)

use std::{
    collections::{BTreeMap, HashMap},
    sync::{Arc, Mutex},
};

use crate::utils::algebraic_to_symbolic;

use super::{
    opcode::PowdrOpcode,
    vm::{OriginalInstruction, SdkVmInventory},
    PowdrPrecompile,
};
use itertools::Itertools;
use openvm_circuit::{arch::VmConfig, system::memory::MemoryController};
use openvm_circuit::{
    arch::{
        ExecutionState, InstructionExecutor, Result as ExecutionResult, VmChipComplex,
        VmInventoryError,
    },
    system::memory::OfflineMemory,
    utils::next_power_of_two_or_zero,
};
use openvm_circuit_primitives::{
    bitwise_op_lookup::SharedBitwiseOperationLookupChip, range_tuple::SharedRangeTupleCheckerChip,
    var_range::SharedVariableRangeCheckerChip,
};
use openvm_instructions::{instruction::Instruction, LocalOpcode};
use openvm_native_circuit::CastFExtension;
use openvm_sdk::config::{SdkVmConfig, SdkVmConfigExecutor, SdkVmConfigPeriphery};
use openvm_stark_backend::{
    air_builders::symbolic::{
        symbolic_expression::{SymbolicEvaluator, SymbolicExpression},
        symbolic_variable::{Entry, SymbolicVariable},
    },
    interaction::BusIndex,
    p3_air::{Air, BaseAir},
    p3_field::FieldAlgebra,
    p3_matrix::dense::RowMajorMatrix,
    p3_maybe_rayon::prelude::{
        IndexedParallelIterator, IntoParallelIterator, IntoParallelRefIterator, ParallelIterator,
        ParallelSliceMut,
    },
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
use powdr_autoprecompiles::powdr::{Column, UniqueColumns};
use serde::{Deserialize, Serialize};

pub struct PowdrChip<F: PrimeField32> {
    pub name: String,
    pub opcode: PowdrOpcode,
    /// An "executor" for this chip, based on the original instructions in the basic block
    pub executor: PowdrExecutor<F>,
    pub air: Arc<PowdrAir<F>>,
    pub periphery: SharedChips,
}

// Extracted from openvm, extended to create an inventory with the correct memory
fn create_chip_complex_with_memory<F: PrimeField32>(
    memory: Arc<Mutex<OfflineMemory<F>>>,
    range_checker: SharedVariableRangeCheckerChip,
    base_config: SdkVmConfig,
) -> std::result::Result<
    VmChipComplex<F, SdkVmConfigExecutor<F>, SdkVmConfigPeriphery<F>>,
    VmInventoryError,
> {
    use openvm_keccak256_circuit::Keccak256;
    use openvm_native_circuit::Native;
    use openvm_rv32im_circuit::{Rv32I, Rv32Io};
    use openvm_sha256_circuit::Sha256;

    let this = base_config;
    let mut complex = this.system.config.create_chip_complex()?.transmute();

    // CHANGE: inject the correct memory here to be passed to the chips, to be accessible in their get_proof_input
    complex.base.memory_controller.offline_memory = memory.clone();
    complex.base.range_checker_chip = range_checker;
    // END CHANGE

    if this.rv32i.is_some() {
        complex = complex.extend(&Rv32I)?;
    }
    if this.io.is_some() {
        complex = complex.extend(&Rv32Io)?;
    }
    if this.keccak.is_some() {
        complex = complex.extend(&Keccak256)?;
    }
    if this.sha256.is_some() {
        complex = complex.extend(&Sha256)?;
    }
    if this.native.is_some() {
        complex = complex.extend(&Native)?;
    }
    if this.castf.is_some() {
        complex = complex.extend(&CastFExtension)?;
    }

    if let Some(rv32m) = this.rv32m {
        let mut rv32m = rv32m;
        if let Some(ref bigint) = this.bigint {
            rv32m.range_tuple_checker_sizes[0] =
                rv32m.range_tuple_checker_sizes[0].max(bigint.range_tuple_checker_sizes[0]);
            rv32m.range_tuple_checker_sizes[1] =
                rv32m.range_tuple_checker_sizes[1].max(bigint.range_tuple_checker_sizes[1]);
        }
        complex = complex.extend(&rv32m)?;
    }
    if let Some(bigint) = this.bigint {
        let mut bigint = bigint;
        if let Some(ref rv32m) = this.rv32m {
            bigint.range_tuple_checker_sizes[0] =
                rv32m.range_tuple_checker_sizes[0].max(bigint.range_tuple_checker_sizes[0]);
            bigint.range_tuple_checker_sizes[1] =
                rv32m.range_tuple_checker_sizes[1].max(bigint.range_tuple_checker_sizes[1]);
        }
        complex = complex.extend(&bigint)?;
    }
    if let Some(ref modular) = this.modular {
        complex = complex.extend(modular)?;
    }
    if let Some(ref fp2) = this.fp2 {
        complex = complex.extend(fp2)?;
    }
    if let Some(ref pairing) = this.pairing {
        complex = complex.extend(pairing)?;
    }
    if let Some(ref ecc) = this.ecc {
        complex = complex.extend(ecc)?;
    }

    Ok(complex)
}

/// A struct which holds the state of the execution based on the original instructions in this block and a dummy inventory.
pub struct PowdrExecutor<F: PrimeField32> {
    instructions: Vec<OriginalInstruction<F>>,
    air_by_opcode_id: BTreeMap<usize, SymbolicMachine<F>>,
    is_valid_poly_id: u64,
    inventory: SdkVmInventory<F>,
    current_trace_height: usize,
}

impl<F: PrimeField32> PowdrExecutor<F> {
    fn new(
        instructions: Vec<OriginalInstruction<F>>,
        air_by_opcode_id: BTreeMap<usize, SymbolicMachine<F>>,
        is_valid_column: Column,
        memory: Arc<Mutex<OfflineMemory<F>>>,
        range_checker: &SharedVariableRangeCheckerChip,
        base_config: SdkVmConfig,
    ) -> Self {
        Self {
            instructions,
            air_by_opcode_id,
            is_valid_poly_id: is_valid_column.id.id,
            inventory: create_chip_complex_with_memory(
                memory,
                range_checker.clone(),
                base_config.clone(),
            )
            .unwrap()
            .inventory,
            current_trace_height: 0,
        }
    }

    fn execute(
        &mut self,
        memory: &mut MemoryController<F>,
        from_state: ExecutionState<u32>,
    ) -> ExecutionResult<ExecutionState<u32>> {
        // execute the original instructions one by one
        let res = self
            .instructions
            .iter()
            .try_fold(from_state, |execution_state, instruction| {
                let executor = self
                    .inventory
                    .get_mut_executor(&instruction.opcode())
                    .unwrap();
                executor.execute(memory, instruction.as_ref(), execution_state)
            });

        self.current_trace_height += 1;

        res
    }
}

/// The shared chips which can be used by the PowdrChip.
pub struct SharedChips {
    bitwise_lookup_8: SharedBitwiseOperationLookupChip<8>,
    range_checker: SharedVariableRangeCheckerChip,
    tuple_range_checker: Option<SharedRangeTupleCheckerChip<2>>,
}

impl SharedChips {
    pub fn new(
        bitwise_lookup_8: SharedBitwiseOperationLookupChip<8>,
        range_checker: SharedVariableRangeCheckerChip,
        tuple_range_checker: Option<SharedRangeTupleCheckerChip<2>>,
    ) -> Self {
        Self {
            bitwise_lookup_8,
            range_checker,
            tuple_range_checker,
        }
    }
}

impl SharedChips {
    /// Sends concrete values to the shared chips using a given bus id.
    /// Panics if the bus id doesn't match any of the chips' bus ids.
    fn apply(&self, bus_id: u16, mult: u32, args: &[u32]) {
        match bus_id {
            id if id == self.bitwise_lookup_8.bus().inner.index => {
                // bitwise operation lookup
                // interpret the arguments, see `Air<AB> for BitwiseOperationLookupAir<NUM_BITS>`
                let [x, y, x_xor_y, selector] = args.try_into().unwrap();

                for _ in 0..mult {
                    match selector {
                        0 => {
                            self.bitwise_lookup_8.request_range(x, y);
                        }
                        1 => {
                            let res = self.bitwise_lookup_8.request_xor(x, y);
                            debug_assert_eq!(res, x_xor_y);
                        }
                        _ => {
                            unreachable!("Invalid selector");
                        }
                    }
                }
            }
            id if id == self.range_checker.bus().index() => {
                // interpret the arguments, see `Air<AB> for VariableRangeCheckerAir`
                let [value, max_bits] = args.try_into().unwrap();

                for _ in 0..mult {
                    self.range_checker.add_count(value, max_bits as usize);
                }
            }
            id if Some(id)
                == self
                    .tuple_range_checker
                    .as_ref()
                    .map(|c| c.bus().inner.index) =>
            {
                // tuple range checker
                // We pass a slice. It is checked inside `add_count`.
                for _ in 0..mult {
                    self.tuple_range_checker.as_ref().unwrap().add_count(args);
                }
            }
            0..=2 => {
                // execution bridge, memory, pc lookup
                // do nothing
            }
            _ => {
                unreachable!("Bus interaction {} not implemented", bus_id);
            }
        }
    }
}

impl<F: PrimeField32> PowdrChip<F> {
    pub(crate) fn new(
        precompile: PowdrPrecompile<F>,
        memory: Arc<Mutex<OfflineMemory<F>>>,
        base_config: SdkVmConfig,
        periphery: SharedChips,
    ) -> Self {
        let air: PowdrAir<F> = PowdrAir::new(precompile.machine);
        let original_airs = precompile
            .original_airs
            .into_iter()
            .map(|(k, v)| (k, v.into()))
            .collect();
        let executor = PowdrExecutor::new(
            precompile.original_instructions,
            original_airs,
            precompile.is_valid_column,
            memory,
            &periphery.range_checker,
            base_config,
        );
        let name = precompile.name;
        let opcode = precompile.opcode;

        Self {
            name,
            opcode,
            air: Arc::new(air),
            executor,
            periphery,
        }
    }

    /// Returns the index of the is_valid of this air.
    fn get_is_valid_index(&self) -> usize {
        self.air.column_index_by_poly_id[&self.executor.is_valid_poly_id]
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
        self.executor.current_trace_height
    }

    fn trace_width(&self) -> usize {
        self.air.width()
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

        let is_valid_index = self.get_is_valid_index();
        let num_records = self.current_trace_height();
        let height = next_power_of_two_or_zero(num_records);
        let width = self.air.width();
        let mut values = Val::<SC>::zero_vec(height * width);

        // for each original opcode, the name of the dummy air it corresponds to
        let air_name_by_opcode = self
            .executor
            .instructions
            .iter()
            .map(|instruction| instruction.opcode())
            .unique()
            .map(|opcode| {
                (
                    opcode,
                    self.executor
                        .inventory
                        .get_executor(opcode)
                        .unwrap()
                        .air_name(),
                )
            })
            .collect::<HashMap<_, _>>();

        let dummy_trace_by_air_name: HashMap<_, _> = self
            .executor
            .inventory
            .executors
            .into_par_iter()
            .map(|executor| {
                (
                    executor.air_name(),
                    Chip::<SC>::generate_air_proof_input(executor)
                        .raw
                        .common_main
                        .unwrap(),
                )
            })
            .collect();

        let instruction_index_to_table_offset = self
            .executor
            .instructions
            .iter()
            .enumerate()
            .scan(
                HashMap::default(),
                |counts: &mut HashMap<&str, usize>, (index, instruction)| {
                    let air_name = air_name_by_opcode.get(&instruction.opcode()).unwrap();
                    let count = counts.entry(air_name).or_default();
                    let current_count = *count;
                    *count += 1;
                    Some((index, (air_name, current_count)))
                },
            )
            .collect::<HashMap<_, _>>();

        let occurrences_by_table_name: HashMap<&String, usize> = self
            .executor
            .instructions
            .iter()
            .map(|instruction| air_name_by_opcode.get(&instruction.opcode()).unwrap())
            .counts();

        // A vector of HashMap<dummy_trace_index, apc_trace_index> by instruction, empty HashMap if none maps to apc
        let dummy_trace_index_to_apc_index_by_instruction: Vec<HashMap<usize, usize>> = self
            .executor
            .instructions
            .par_iter()
            .map(|instruction| {
                // look up how many dummy‐cells this AIR produces:
                let air_width = dummy_trace_by_air_name
                    .get(air_name_by_opcode.get(&instruction.opcode()).unwrap())
                    .unwrap()
                    .width();

                // build a map only of the (dummy_index -> apc_index) pairs
                let mut map = HashMap::with_capacity(air_width);
                for dummy_trace_index in 0..air_width {
                    if let Ok(apc_index) = global_index(
                        dummy_trace_index,
                        instruction,
                        &self.air.column_index_by_poly_id,
                    ) {
                        if map.insert(dummy_trace_index, apc_index).is_some() {
                            panic!(
                                "duplicate dummy_trace_index {} for instruction opcode {:?}",
                                dummy_trace_index,
                                instruction.opcode()
                            );
                        }
                    }
                }
                map
            })
            .collect();

        assert_eq!(
            self.executor.instructions.len(),
            dummy_trace_index_to_apc_index_by_instruction.len()
        );

        let dummy_values = (0..num_records).into_par_iter().map(|record_index| {
            (0..self.executor.instructions.len())
                .map(|index| {
                    // get the air name and offset for this instruction (by index)
                    let (air_name, offset) = instruction_index_to_table_offset.get(&index).unwrap();
                    // get the table
                    let table = dummy_trace_by_air_name.get(*air_name).unwrap();
                    // get how many times this table is used per record
                    let occurrences_per_record = occurrences_by_table_name.get(air_name).unwrap();
                    // get the width of each occurrence
                    let width = table.width();
                    // start after the previous record ended, and offset by the correct offset
                    let start = (record_index * occurrences_per_record + offset) * width;
                    // end at the start + width
                    let end = start + width;
                    &table.values[start..end]
                })
                .collect_vec()
        });

        // go through the final table and fill in the values
        values
            // a record is `width` values
            .par_chunks_mut(width)
            .zip(dummy_values)
            .for_each(|(row_slice, dummy_values)| {
                // map the dummy rows to the autoprecompile row
                for (instruction_id, (instruction, dummy_row)) in self
                    .executor
                    .instructions
                    .iter()
                    .zip_eq(dummy_values)
                    .enumerate()
                {
                    let evaluator = RowEvaluator::new(dummy_row, None);

                    // first remove the side effects of this row on the main periphery
                    for range_checker_send in self
                        .executor
                        .air_by_opcode_id
                        .get(&instruction.as_ref().opcode.as_usize())
                        .unwrap()
                        .bus_interactions
                        .iter()
                        .filter(|i| i.id == 3)
                    {
                        let mult = evaluator
                            .eval_expr(&range_checker_send.mult)
                            .as_canonical_u32();
                        let args = range_checker_send
                            .args
                            .iter()
                            .map(|arg| evaluator.eval_expr(arg).as_canonical_u32())
                            .collect_vec();
                        let [value, max_bits] = args.try_into().unwrap();
                        for _ in 0..mult {
                            self.periphery
                                .range_checker
                                .remove_count(value, max_bits as usize);
                        }
                    }

                    write_dummy_to_autoprecompile_row(
                        row_slice,
                        dummy_row,
                        &dummy_trace_index_to_apc_index_by_instruction[instruction_id],
                    );
                }

                // Set the is_valid column to 1
                row_slice[is_valid_index] = <Val<SC>>::ONE;

                let evaluator =
                    RowEvaluator::new(row_slice, Some(&self.air.column_index_by_poly_id));

                // replay the side effects of this row on the main periphery
                for bus_interaction in self.air.machine.bus_interactions.iter() {
                    let mult = evaluator
                        .eval_expr(&bus_interaction.mult)
                        .as_canonical_u32();
                    let args = bus_interaction
                        .args
                        .iter()
                        .map(|arg| evaluator.eval_expr(arg).as_canonical_u32())
                        .collect_vec();

                    self.periphery.apply(bus_interaction.id, mult, &args);
                }
            });

        let trace = RowMajorMatrix::new(values, width);

        AirProofInput::simple(trace, vec![])
    }
}

fn write_dummy_to_autoprecompile_row<F: PrimeField32>(
    row_slice: &mut [F],
    dummy_row: &[F],
    dummy_trace_index_to_apc_index: &HashMap<usize, usize>,
) {
    for (dummy_trace_index, apc_index) in dummy_trace_index_to_apc_index {
        row_slice[*apc_index] = dummy_row[*dummy_trace_index];
    }
}

enum IndexError {
    NotInDummy,
    NotInAutoprecompile,
}

/// Maps the index of a column in the original AIR of a given instruction to the corresponding
/// index in the autoprecompile AIR.
fn global_index<F>(
    local_index: usize,
    instruction: &OriginalInstruction<F>,
    autoprecompile_index_by_poly_id: &BTreeMap<u64, usize>,
) -> Result<usize, IndexError> {
    // Map to the poly_id in the original instruction to the poly_id in the autoprecompile.
    let autoprecompile_poly_id = instruction
        .subs
        .get(local_index)
        .ok_or(IndexError::NotInDummy)?;
    // Map to the index in the autoprecompile.
    let variable_index = autoprecompile_index_by_poly_id
        .get(autoprecompile_poly_id)
        .ok_or(IndexError::NotInAutoprecompile)?;
    Ok(*variable_index)
}

pub struct PowdrAir<F> {
    /// The columns in arbitrary order
    columns: Vec<Column>,
    /// The mapping from poly_id id to the index in the list of columns.
    /// The values are always unique and contiguous
    column_index_by_poly_id: BTreeMap<u64, usize>,
    machine: SymbolicMachine<F>,
}

impl<F: PrimeField32> ColumnsAir<F> for PowdrAir<F> {
    fn columns(&self) -> Option<Vec<String>> {
        Some(self.columns.iter().map(|c| c.name.clone()).collect())
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
    columns: Vec<Column>,
    constraints: Vec<SymbolicConstraint<F>>,
    bus_interactions: Vec<SymbolicBusInteraction<F>>,
}

impl<F: PrimeField32> From<powdr_autoprecompiles::SymbolicMachine<F>> for SymbolicMachine<F> {
    fn from(machine: powdr_autoprecompiles::SymbolicMachine<F>) -> Self {
        let columns = machine.unique_columns().collect();

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
struct SymbolicBusInteraction<F> {
    id: BusIndex,
    mult: SymbolicExpression<F>,
    args: Vec<SymbolicExpression<F>>,
    count_weight: u32,
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

impl<F: PrimeField32> PowdrAir<F> {
    pub fn new(machine: powdr_autoprecompiles::SymbolicMachine<F>) -> Self {
        let (column_index_by_poly_id, columns): (BTreeMap<_, _>, Vec<_>) = machine
            .unique_columns()
            .enumerate()
            .map(|(index, c)| ((c.id.id, index), c.clone()))
            .unzip();

        Self {
            columns,
            column_index_by_poly_id,
            machine: machine.into(),
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
            .map(|c| c.id.id)
            .zip_eq(witnesses.iter().cloned())
            .collect();

        let witness_evaluator = WitnessEvaluator::<AB>::new(&witness_values);

        for constraint in &self.machine.constraints {
            let e = witness_evaluator.eval_expr(&constraint.expr);
            builder.assert_zero(e);
        }

        for interaction in &self.machine.bus_interactions {
            let SymbolicBusInteraction {
                id,
                mult,
                args,
                count_weight,
            } = interaction;

            let mult = witness_evaluator.eval_expr(mult);
            let args = args
                .iter()
                .map(|arg| witness_evaluator.eval_expr(arg))
                .collect_vec();

            builder.push_interaction(*id, args, mult, *count_weight);
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
