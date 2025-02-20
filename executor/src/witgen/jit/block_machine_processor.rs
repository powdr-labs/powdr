use std::collections::{BTreeMap, BTreeSet, HashSet};

use bit_vec::BitVec;
use itertools::Itertools;
use powdr_ast::analyzed::{ContainsNextRef, PolyID, PolynomialType};
use powdr_number::FieldElement;

use crate::witgen::{
    jit::{
        code_cleaner, identity_queue::QueueItem, processor::Processor,
        prover_function_heuristics::decode_prover_functions,
    },
    machines::MachineParts,
    FixedData,
};

use super::{
    effect::Effect,
    processor::ProcessorResult,
    prover_function_heuristics::ProverFunction,
    variable::{Cell, MachineCallVariable, Variable},
    witgen_inference::{CanProcessCall, FixedEvaluator, WitgenInference},
};

/// This is a tuning value. It is the maximum nesting depth of branches in the JIT code.
const BLOCK_MACHINE_MAX_BRANCH_DEPTH: usize = 6;

/// A processor for generating JIT code for a block machine.
pub struct BlockMachineProcessor<'a, T: FieldElement> {
    fixed_data: &'a FixedData<'a, T>,
    machine_parts: MachineParts<'a, T>,
    block_size: usize,
    latch_row: usize,
}

impl<'a, T: FieldElement> BlockMachineProcessor<'a, T> {
    pub fn new(
        fixed_data: &'a FixedData<'a, T>,
        machine_parts: MachineParts<'a, T>,
        block_size: usize,
        latch_row: usize,
    ) -> Self {
        BlockMachineProcessor {
            fixed_data,
            machine_parts,
            block_size,
            latch_row,
        }
    }

    /// Generates the JIT code for a given combination of connection and known arguments.
    /// Fails if it cannot solve for the outputs, or if any sub-machine calls cannot be completed.
    pub fn generate_code(
        &self,
        can_process: impl CanProcessCall<T>,
        identity_id: u64,
        known_args: &BitVec,
        known_concrete: Option<(usize, T)>,
    ) -> Result<(ProcessorResult<T>, Vec<ProverFunction<'a, T>>), String> {
        let connection = self.machine_parts.connections[&identity_id];
        assert_eq!(connection.right.expressions.len(), known_args.len());

        // Set up WitgenInference with known arguments.
        let known_variables = known_args
            .iter()
            .enumerate()
            .filter_map(|(i, is_input)| is_input.then_some(Variable::Param(i)))
            .collect::<HashSet<_>>();
        let witgen = WitgenInference::new(self.fixed_data, self, known_variables, []);

        let prover_functions = decode_prover_functions(&self.machine_parts, self.fixed_data)?;

        let mut queue_items = vec![];

        // In the latch row, set the RHS selector to 1.
        let selector = &connection.right.selector;
        queue_items.push(QueueItem::constant_assignment(
            selector,
            T::one(),
            self.latch_row as i32,
        ));

        if let Some((index, value)) = known_concrete {
            // Set the known argument to the concrete value.
            queue_items.push(QueueItem::constant_assignment(
                &connection.right.expressions[index],
                value,
                self.latch_row as i32,
            ));
        }

        // Set all other selectors to 0 in the latch row.
        for other_connection in self.machine_parts.connections.values() {
            let other_selector = &other_connection.right.selector;
            if other_selector != selector {
                queue_items.push(QueueItem::constant_assignment(
                    other_selector,
                    T::zero(),
                    self.latch_row as i32,
                ));
            }
        }

        // For each argument, connect the expression on the RHS with the formal parameter.
        for (index, expr) in connection.right.expressions.iter().enumerate() {
            queue_items.push(QueueItem::variable_assignment(
                expr,
                Variable::Param(index),
                self.latch_row as i32,
            ));
        }

        let intermediate_definitions = self.fixed_data.analyzed.intermediate_definitions();

        // Compute the identity-row-pairs we consider.
        let have_next_ref = self
            .machine_parts
            .identities
            .iter()
            .any(|id| id.contains_next_ref(&intermediate_definitions));
        let (start_row, end_row) = if !have_next_ref {
            // No identity contains a next reference - we do not need to consider row -1,
            // and the block has to be rectangular-shaped.
            (0, self.block_size as i32 - 1)
        } else {
            // A machine that might have a non-rectangular shape.
            // We iterate over all rows of the block +/- one row.
            (-1, self.block_size as i32)
        };
        let identities = (start_row..=end_row).flat_map(|row| {
            self.machine_parts
                .identities
                .iter()
                .map(|id| (*id, row))
                .collect_vec()
        });

        // Add the prover functions
        queue_items.extend(prover_functions.iter().flat_map(|f| {
            (0..self.block_size).map(move |row| QueueItem::ProverFunction(f.clone(), row as i32))
        }));

        let requested_known = known_args
            .iter()
            .enumerate()
            .filter_map(|(i, is_input)| (!is_input).then_some(Variable::Param(i)))
            .collect_vec();
        let mut result = Processor::new(
            self.fixed_data,
            identities,
            queue_items,
            requested_known.iter().cloned(),
            BLOCK_MACHINE_MAX_BRANCH_DEPTH,
        )
        .with_block_size(self.block_size)
        .with_requested_range_constraints((0..known_args.len()).map(Variable::Param))
        .generate_code(can_process, witgen)
        .map_err(|e| {
            let err_str = e.to_string_with_variable_formatter(|var| match var {
                Variable::Param(i) => format!("{} (connection param)", &connection.right.expressions[*i]),
                _ => var.to_string(),
            });
            log::trace!("\nCode generation failed for connection:\n  {connection}");
            let known_args_str = known_args
                .iter()
                .enumerate()
                .filter_map(|(i, b)| b.then_some(connection.right.expressions[i].to_string()))
                .join("\n  ");
            log::trace!("Known arguments:\n  {known_args_str}");
            log::trace!("Error:\n  {err_str}");
            let shortened_error = err_str
                .lines()
                .take(10)
                .format("\n  ");
            format!("Code generation failed: {shortened_error}\nRun with RUST_LOG=trace to see the code generated so far.")
        })?;

        result.code = self.try_ensure_block_shape(result.code, &requested_known)?;

        Ok((result, prover_functions))
    }

    /// Tries to ensure that each column and each bus send is stackable in the block.
    /// This means that if we have a cell write or a bus send in row `i`, we cannot
    /// have another one in row `i + block_size`.
    /// In some situations, it removes assignments to variables that are not required,
    /// but would conflict with other assignments.
    /// Returns the potentially modified code.
    fn try_ensure_block_shape(
        &self,
        code: Vec<Effect<T, Variable>>,
        requested_known: &[Variable],
    ) -> Result<Vec<Effect<T, Variable>>, String> {
        let optional_vars = code_cleaner::optional_vars(&code, &requested_known);

        // Determine conflicting variable assignments we can remove.
        let mut vars_to_remove = HashSet::new();
        for (column_id, row_offsets) in written_rows_per_column(&code) {
            for (outside, inside) in self.conflicting_row_offsets(&row_offsets) {
                let first_var = Variable::WitnessCell(Cell {
                    column_name: String::new(),
                    id: column_id,
                    row_offset: outside,
                });
                let second_var = Variable::WitnessCell(Cell {
                    column_name: String::new(),
                    id: column_id,
                    row_offset: inside,
                });

                if optional_vars.contains(&first_var) {
                    vars_to_remove.insert(first_var);
                } else if optional_vars.contains(&second_var) {
                    vars_to_remove.insert(second_var);
                } else {
                    // Both variables are non-optional, we have a conflict.
                    return Err(format!(
                        "Column {} is not stackable in a {}-row block, conflict in rows {inside} and {outside}.",
                        self.fixed_data.column_name(&PolyID {
                            id: column_id,
                            ptype: PolynomialType::Committed
                        }),
                        self.block_size,
                    ));
                }
            }
        }
        let code = code_cleaner::remove_variables(code, vars_to_remove);

        // Determine conflicting machine calls we can remove.
        let mut machine_calls_to_remove = HashSet::new();
        for (identity_id, row_offsets) in completed_rows_for_bus_send(&code) {
            for (outside, inside) in
                self.conflicting_row_offsets(&row_offsets.keys().copied().collect())
            {
                if row_offsets[&outside] {
                    machine_calls_to_remove.insert((identity_id, outside));
                } else if row_offsets[&inside] {
                    machine_calls_to_remove.insert((identity_id, inside));
                } else {
                    return Err(format!(
                    "Bus send for identity {} is not stackable in a {}-row block, conflict in rows {inside} and {outside}.",
                    identity_id,
                    self.block_size,
                ));
                }
            }
        }
        let code = code_cleaner::remove_machine_calls(code, &machine_calls_to_remove);

        Ok(code)
    }

    /// Returns a list of pairs of conflicting row offsets in `row_offsets`
    /// (i.e. equal modulo block size) where the first is always the one
    /// outside the "regular" block range.
    fn conflicting_row_offsets<'b>(
        &'b self,
        row_offsets: &'b BTreeSet<i32>,
    ) -> impl Iterator<Item = (i32, i32)> + 'b {
        row_offsets.iter().copied().flat_map(|offset| {
            let other_offset = offset + self.block_size as i32;
            row_offsets.contains(&other_offset).then_some({
                if offset >= 0 && offset < self.block_size as i32 {
                    (other_offset, offset)
                } else {
                    (offset, other_offset)
                }
            })
        })
    }
}

/// Returns, for each column ID, the collection of row offsets that have a cell write.
/// Combines writes from branches.
fn written_rows_per_column<T: FieldElement>(
    code: &[Effect<T, Variable>],
) -> BTreeMap<u64, BTreeSet<i32>> {
    code.iter()
        .flat_map(|e| e.written_vars())
        .filter_map(|(v, _)| match v {
            Variable::WitnessCell(cell) => Some((cell.id, cell.row_offset)),
            _ => None,
        })
        .fold(BTreeMap::new(), |mut map, (id, row)| {
            map.entry(id).or_default().insert(row);
            map
        })
}

/// Returns, for each bus send ID, the collection of row offsets that have a machine call
/// and if in all the calls or that row, all the arguments are known.
/// Combines calls from branches.
fn completed_rows_for_bus_send<T: FieldElement>(
    code: &[Effect<T, Variable>],
) -> BTreeMap<u64, BTreeMap<i32, bool>> {
    code.iter()
        .flat_map(machine_calls)
        .fold(BTreeMap::new(), |mut map, (id, row, call)| {
            let rows = map.entry(id).or_default();
            let entry = rows.entry(row).or_insert_with(|| true);
            *entry &= fully_known_call(call);
            map
        })
}

/// Returns true if the effect is a machine call where all arguments are known.
fn fully_known_call<T: FieldElement>(e: &Effect<T, Variable>) -> bool {
    match e {
        Effect::MachineCall(_, known, _) => known.iter().all(|v| v),
        _ => false,
    }
}

/// Returns all machine calls (bus identity and row offset) found in the effect.
/// Recurses into branches.
fn machine_calls<T: FieldElement>(
    e: &Effect<T, Variable>,
) -> Box<dyn Iterator<Item = (u64, i32, &Effect<T, Variable>)> + '_> {
    match e {
        Effect::MachineCall(id, _, arguments) => match &arguments[0] {
            Variable::MachineCallParam(MachineCallVariable {
                identity_id,
                row_offset,
                ..
            }) => {
                assert_eq!(*id, *identity_id);
                Box::new(std::iter::once((*identity_id, *row_offset, e)))
            }
            _ => panic!("Expected machine call variable."),
        },
        Effect::Branch(_, first, second) => {
            Box::new(first.iter().chain(second.iter()).flat_map(machine_calls))
        }
        _ => Box::new(std::iter::empty()),
    }
}

impl<T: FieldElement> FixedEvaluator<T> for &BlockMachineProcessor<'_, T> {
    fn evaluate(&self, fixed_cell: &Cell) -> Option<T> {
        let poly_id = PolyID {
            id: fixed_cell.id,
            ptype: PolynomialType::Constant,
        };
        let values = self.fixed_data.fixed_cols[&poly_id].values_max_size();

        // By assumption of the block machine, all fixed columns are cyclic with a period of <block_size>.
        // An exception might be the first and last row.
        assert!(fixed_cell.row_offset >= -1);
        assert!(self.block_size >= 1);
        // The row is guaranteed to be at least 1.
        let row = (2 * self.block_size as i32 + fixed_cell.row_offset) as usize;

        assert!(values.len() >= self.block_size * 4);

        // Fixed columns are assumed to be cyclic, except in the first and last row.
        // The code above should ensure that we never access the first or last row.
        assert!(row > 0);
        assert!(row < values.len() - 1);

        Some(values[row])
    }
}

#[cfg(test)]
mod test {
    use std::fs::read_to_string;

    use pretty_assertions::assert_eq;
    use test_log::test;

    use powdr_number::GoldilocksField;

    use crate::witgen::{
        data_structures::mutable_state::MutableState,
        global_constraints,
        jit::{effect::format_code, test_util::read_pil},
        machines::{machine_extractor::MachineExtractor, KnownMachine, Machine},
        range_constraints::RangeConstraint,
        FixedData,
    };

    use super::*;

    fn generate_for_block_machine(
        input_pil: &str,
        machine_name: &str,
        num_inputs: usize,
        num_outputs: usize,
    ) -> Result<ProcessorResult<GoldilocksField>, String> {
        let (analyzed, fixed_col_vals) = read_pil(input_pil);

        let fixed_data = FixedData::new(&analyzed, &fixed_col_vals, &[], Default::default(), 0);
        let fixed_data = global_constraints::set_global_constraints(fixed_data);
        let machines = MachineExtractor::new(&fixed_data).split_out_machines();
        let [KnownMachine::BlockMachine(machine)] = machines
            .iter()
            .filter(|m| m.name().contains(machine_name))
            .collect_vec()
            .as_slice()
        else {
            panic!("Expected exactly one matching block machine")
        };
        let (machine_parts, block_size, latch_row) = machine.machine_info();
        assert_eq!(machine_parts.connections.len(), 1);
        let connection_id = *machine_parts.connections.keys().next().unwrap();
        let processor = BlockMachineProcessor {
            fixed_data: &fixed_data,
            machine_parts: machine_parts.clone(),
            block_size,
            latch_row,
        };

        let mutable_state = MutableState::new(machines.into_iter(), &|_| {
            Err("Query not implemented".to_string())
        });

        let known_values = BitVec::from_iter(
            (0..num_inputs)
                .map(|_| true)
                .chain((0..num_outputs).map(|_| false)),
        );

        processor
            .generate_code(&mutable_state, connection_id, &known_values, None)
            .map(|(result, _)| result)
    }

    #[test]
    fn add() {
        let input = "
        namespace Main(256);
            col witness a, b, c;
            [a, b, c] is Add.sel $ [Add.a, Add.b, Add.c]; 
        namespace Add(256);
            col witness sel, a, b, c;
            c = a + b;
        ";
        let code = generate_for_block_machine(input, "Add", 2, 1).unwrap().code;
        assert_eq!(
            format_code(&code),
            "Add::sel[0] = 1;
Add::a[0] = params[0];
Add::b[0] = params[1];
Add::c[0] = (Add::a[0] + Add::b[0]);
params[2] = Add::c[0];"
        );
    }

    #[test]
    fn unconstrained_output() {
        let input = "
        namespace Main(256);
            col witness a, b, c;
            [a, b, c] is Unconstrained.sel $ [Unconstrained.a, Unconstrained.b, Unconstrained.c]; 
        namespace Unconstrained(256);
            col witness sel, a, b, c;
            a + b = 0;
        ";
        let err_str = generate_for_block_machine(input, "Unconstrained", 2, 1)
            .err()
            .unwrap();
        assert!(err_str
            .contains("The following variables or values are still missing: Unconstrained::c"));
    }

    #[test]
    fn not_stackable() {
        // Note: This used to require a panic, but now we are just not assigning to
        // a' any more. At some point, we need a better check for the block shape.
        let input = "
        namespace Main(256);
            col witness a, b, c;
            [a] is NotStackable.sel $ [NotStackable.a]; 
        namespace NotStackable(256);
            col witness sel, a;
            a = a';
        ";
        generate_for_block_machine(input, "NotStackable", 1, 0).unwrap();
    }

    #[test]
    fn binary() {
        let input = read_to_string("../test_data/pil/binary.pil").unwrap();
        let result = generate_for_block_machine(&input, "main_binary", 3, 1).unwrap();
        let [op_rc, a_rc, b_rc, c_rc] = &result.range_constraints.try_into().unwrap();
        assert_eq!(op_rc, &RangeConstraint::from_range(0.into(), 2.into()));
        assert_eq!(a_rc, &RangeConstraint::from_mask(0xffffffffu64));
        assert_eq!(b_rc, &RangeConstraint::from_mask(0xffffffffu64));
        assert_eq!(c_rc, &RangeConstraint::from_mask(0xffffffffu64));
        assert_eq!(
            format_code(&result.code),
            "main_binary::sel[0][3] = 1;
main_binary::operation_id[3] = params[0];
main_binary::A[3] = params[1];
main_binary::B[3] = params[2];
main_binary::operation_id[2] = main_binary::operation_id[3];
main_binary::operation_id[1] = main_binary::operation_id[2];
main_binary::operation_id[0] = main_binary::operation_id[1];
main_binary::operation_id_next[-1] = main_binary::operation_id[0];
call_var(9, -1, 0) = main_binary::operation_id_next[-1];
main_binary::operation_id_next[0] = main_binary::operation_id[1];
call_var(9, 0, 0) = main_binary::operation_id_next[0];
main_binary::operation_id_next[1] = main_binary::operation_id[2];
call_var(9, 1, 0) = main_binary::operation_id_next[1];
main_binary::A_byte[2] = ((main_binary::A[3] & 0xff000000) // 16777216);
main_binary::A[2] = (main_binary::A[3] & 0xffffff);
assert (main_binary::A[3] & 0xffffffff00000000) == 0;
call_var(9, 2, 1) = main_binary::A_byte[2];
main_binary::A_byte[1] = ((main_binary::A[2] & 0xff0000) // 65536);
main_binary::A[1] = (main_binary::A[2] & 0xffff);
assert (main_binary::A[2] & 0xffffffffff000000) == 0;
call_var(9, 1, 1) = main_binary::A_byte[1];
main_binary::A_byte[0] = ((main_binary::A[1] & 0xff00) // 256);
main_binary::A[0] = (main_binary::A[1] & 0xff);
assert (main_binary::A[1] & 0xffffffffffff0000) == 0;
call_var(9, 0, 1) = main_binary::A_byte[0];
main_binary::A_byte[-1] = main_binary::A[0];
call_var(9, -1, 1) = main_binary::A_byte[-1];
main_binary::B_byte[2] = ((main_binary::B[3] & 0xff000000) // 16777216);
main_binary::B[2] = (main_binary::B[3] & 0xffffff);
assert (main_binary::B[3] & 0xffffffff00000000) == 0;
call_var(9, 2, 2) = main_binary::B_byte[2];
main_binary::B_byte[1] = ((main_binary::B[2] & 0xff0000) // 65536);
main_binary::B[1] = (main_binary::B[2] & 0xffff);
assert (main_binary::B[2] & 0xffffffffff000000) == 0;
call_var(9, 1, 2) = main_binary::B_byte[1];
main_binary::B_byte[0] = ((main_binary::B[1] & 0xff00) // 256);
main_binary::B[0] = (main_binary::B[1] & 0xff);
assert (main_binary::B[1] & 0xffffffffffff0000) == 0;
call_var(9, 0, 2) = main_binary::B_byte[0];
main_binary::B_byte[-1] = main_binary::B[0];
call_var(9, -1, 2) = main_binary::B_byte[-1];
machine_call(9, [Known(call_var(9, -1, 0)), Known(call_var(9, -1, 1)), Known(call_var(9, -1, 2)), Unknown(call_var(9, -1, 3))]);
main_binary::C_byte[-1] = call_var(9, -1, 3);
main_binary::C[0] = main_binary::C_byte[-1];
machine_call(9, [Known(call_var(9, 0, 0)), Known(call_var(9, 0, 1)), Known(call_var(9, 0, 2)), Unknown(call_var(9, 0, 3))]);
main_binary::C_byte[0] = call_var(9, 0, 3);
main_binary::C[1] = (main_binary::C[0] + (main_binary::C_byte[0] * 256));
machine_call(9, [Known(call_var(9, 1, 0)), Known(call_var(9, 1, 1)), Known(call_var(9, 1, 2)), Unknown(call_var(9, 1, 3))]);
main_binary::C_byte[1] = call_var(9, 1, 3);
main_binary::C[2] = (main_binary::C[1] + (main_binary::C_byte[1] * 65536));
main_binary::operation_id_next[2] = main_binary::operation_id[3];
call_var(9, 2, 0) = main_binary::operation_id_next[2];
machine_call(9, [Known(call_var(9, 2, 0)), Known(call_var(9, 2, 1)), Known(call_var(9, 2, 2)), Unknown(call_var(9, 2, 3))]);
main_binary::C_byte[2] = call_var(9, 2, 3);
main_binary::C[3] = (main_binary::C[2] + (main_binary::C_byte[2] * 16777216));
params[3] = main_binary::C[3];"
        )
    }

    #[test]
    fn poseidon() {
        let input = read_to_string("../test_data/pil/poseidon_gl.pil").unwrap();
        generate_for_block_machine(&input, "main_poseidon", 12, 4).unwrap();
    }

    #[test]
    fn simple_prover_function() {
        let input = "
        namespace std::prover;
            let compute_from: expr, int, expr[], (fe[] -> fe) -> () = query |dest_col, row, input_cols, f| {};
        namespace Main(256);
            col witness a, b;
            [a, b] is [Sub.a, Sub.b]; 
        namespace Sub(256);
            col witness a, b;
            (a - 20) * (b + 3) = 1;
            query |i| std::prover::compute_from(b, i, [a], |values| 20);
        ";
        let code = generate_for_block_machine(input, "Sub", 1, 1).unwrap().code;
        assert_eq!(
            format_code(&code),
            "Sub::a[0] = params[0];
[Sub::b[0]] = prover_function_0(0, [Sub::a[0]]);
params[1] = Sub::b[0];"
        );
    }

    #[test]
    fn complex_fixed_lookup_range_constraint() {
        let input = "
        namespace main(256);
            col witness a, b, c;
            [a, b, c] is SubM.sel $ [SubM.a, SubM.b, SubM.c]; 
        namespace SubM(256);
            col witness a, b, c;
            let sel: col = |i| (i + 1) % 2;
            let clock_0: col = |i| i % 2;
            let clock_1: col = |i| (i + 1) % 2;
            let byte: col = |i| i & 0xff;
            [ b * clock_0 + c * clock_1 ] in [ byte ];
            (b' - b) * sel = 0;
            (c' - c) * sel = 0;
            a = b * 256 + c;
        ";
        let code = generate_for_block_machine(input, "SubM", 1, 2)
            .unwrap()
            .code;
        assert_eq!(
            format_code(&code),
            "SubM::a[0] = params[0];
SubM::b[0] = ((SubM::a[0] & 0xff00) // 256);
SubM::c[0] = (SubM::a[0] & 0xff);
assert (SubM::a[0] & 0xffffffffffff0000) == 0;
params[1] = SubM::b[0];
params[2] = SubM::c[0];
call_var(1, 0, 0) = SubM::c[0];
machine_call(1, [Known(call_var(1, 0, 0))]);
SubM::b[1] = SubM::b[0];
call_var(1, 1, 0) = SubM::b[1];
SubM::c[1] = SubM::c[0];
machine_call(1, [Known(call_var(1, 1, 0))]);
SubM::a[1] = ((SubM::b[1] * 256) + SubM::c[1]);"
        );
    }

    #[test]
    fn unused_fixed_lookup() {
        // Checks that irrelevant fixed lookups are still performed
        // in the generated code.
        let input = "
        namespace Main(256);
            col witness a, b, c;
            [a, b, c] is [S.a, S.b, S.c];
        namespace S(256);
            col witness a, b, c, x, y;
            let B: col = |i| i & 0xff;
            a * (a - 1) = 0;
            [ a * x ] in [ B ];
            [ (a - 1) * y ] in [ B ];
            a + b = c;
        ";
        let code = format_code(&generate_for_block_machine(input, "S", 2, 1).unwrap().code);
        assert_eq!(
            code,
            "S::a[0] = params[0];
S::b[0] = params[1];
S::c[0] = (S::a[0] + S::b[0]);
params[2] = S::c[0];
call_var(2, 0, 0) = 0;
call_var(3, 0, 0) = 0;
machine_call(2, [Known(call_var(2, 0, 0))]);
machine_call(3, [Known(call_var(3, 0, 0))]);"
        );
    }

    #[test]
    fn stackable_with_same_value() {
        // In the following, we assign b[0] = 0 and b[4] = 0, which is a stackable
        // error only if we are not able to compare the actual values.
        let input = "
        namespace Main(256);
            col witness a, b, c;
            [a, b, c] is S.sel $ [S.a, S.b, S.c];
        namespace S(256);
            col witness a, b, c;
            let sel: col = |i| if i % 4 == 0 { 1 } else { 0 };
            col fixed FACTOR = [0, 0, 1, 0]*;
            b' = FACTOR * 8;
            c = b + 1;
        ";
        let code = format_code(&generate_for_block_machine(input, "S", 1, 2).unwrap().code);
        assert_eq!(
            code,
            "S::a[0] = params[0];
S::b[0] = 0;
params[1] = 0;
S::b[1] = 0;
S::c[0] = 1;
params[2] = 1;
S::b[2] = 0;
S::c[1] = 1;
S::b[3] = 8;
S::c[2] = 1;
S::c[3] = 9;"
        );
    }
}
