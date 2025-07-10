use std::collections::{BTreeMap, BTreeSet, HashSet};

use bit_vec::BitVec;
use itertools::Itertools;
use num_traits::{One, Zero};
use powdr_ast::analyzed::{ContainsNextRef, PolyID, PolynomialType};
use powdr_number::FieldElement;

use crate::witgen::{
    jit::{
        code_cleaner,
        identity_queue::QueueItem,
        processor::{
            algebraic_expression_to_queue_items, algebraic_variable_equation_to_queue_items,
            Processor,
        },
        prover_function_heuristics::decode_prover_functions,
        QuadraticSymbolicExpression,
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

    /// Generates the JIT code for a given combination of bus and known arguments.
    /// Fails if it cannot solve for the outputs, or if any sub-machine calls cannot be completed.
    pub fn generate_code(
        &self,
        can_process: impl CanProcessCall<T>,
        bus_id: T,
        known_args: &BitVec,
        known_concrete: Option<(usize, T)>,
    ) -> Result<(ProcessorResult<T>, Vec<ProverFunction<'a, T>>), String> {
        let bus_receive = self.machine_parts.bus_receives[&bus_id];
        assert_eq!(
            bus_receive.selected_payload.expressions.len(),
            known_args.len()
        );

        // Set up WitgenInference with known arguments.
        let known_variables = known_args
            .iter()
            .enumerate()
            .filter_map(|(i, is_input)| is_input.then_some(Variable::Param(i)))
            .collect::<HashSet<_>>();
        let witgen = WitgenInference::new(self.fixed_data, self, known_variables, []);

        let mut queue_items = vec![];

        let prover_functions = decode_prover_functions(&self.machine_parts, self.fixed_data)?;

        // In the latch row, set the RHS selector to 1.
        let selector = &bus_receive.selected_payload.selector;
        queue_items.extend(algebraic_expression_to_queue_items(
            selector,
            QuadraticSymbolicExpression::one(),
            self.latch_row as i32,
            &witgen,
        ));

        if let Some((index, value)) = known_concrete {
            // Set the known argument to the concrete value.
            queue_items.extend(algebraic_expression_to_queue_items(
                &bus_receive.selected_payload.expressions[index],
                QuadraticSymbolicExpression::from_number(value),
                self.latch_row as i32,
                &witgen,
            ));
        }

        // Set all other selectors to 0 in the latch row.
        for other_receive in self.machine_parts.bus_receives.values() {
            let other_selector = &other_receive.selected_payload.selector;
            if other_selector != selector {
                queue_items.extend(algebraic_expression_to_queue_items(
                    other_selector,
                    QuadraticSymbolicExpression::zero(),
                    self.latch_row as i32,
                    &witgen,
                ));
            }
        }

        // For each argument, connect the expression on the RHS with the formal parameter.
        for (index, expr) in bus_receive.selected_payload.expressions.iter().enumerate() {
            queue_items.extend(algebraic_variable_equation_to_queue_items(
                expr,
                &Variable::Param(index),
                self.latch_row as i32,
                &witgen,
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

        // Add the intermediate definitions.
        for (poly_id, name) in self.machine_parts.intermediates.iter().sorted() {
            let value = &intermediate_definitions[&(*poly_id).into()];
            for row_offset in start_row..=end_row {
                queue_items.extend(algebraic_expression_to_queue_items(
                    value,
                    QuadraticSymbolicExpression::from_unknown_variable(Variable::IntermediateCell(
                        Cell {
                            column_name: name.clone(),
                            id: poly_id.id,
                            row_offset,
                        },
                    )),
                    row_offset,
                    &witgen,
                ));
            }
        }

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
                Variable::Param(i) => format!("{} (receive param)", &bus_receive.selected_payload.expressions[*i]),
                _ => var.to_string(),
            });
            log::trace!("\nCode generation failed for bus receive:\n  {bus_receive}");
            let known_args_str = known_args
                .iter()
                .enumerate()
                .filter_map(|(i, b)| b.then_some(bus_receive.selected_payload.expressions[i].to_string()))
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
        let optional_vars = code_cleaner::optional_vars(&code, requested_known);

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
        .filter_map(|v| match v {
            Variable::WitnessCell(cell) => Some((cell.id, cell.row_offset)),
            _ => None,
        })
        .fold(BTreeMap::new(), |mut map, (id, row)| {
            map.entry(id).or_default().insert(row);
            map
        })
}

/// Returns, for each bus send *identity* ID, the collection of row offsets that have a machine call
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

/// Returns all machine calls (bus send identity ID and row offset) found in the effect.
/// Recurses into branches.
fn machine_calls<T: FieldElement>(
    e: &Effect<T, Variable>,
) -> Box<dyn Iterator<Item = (u64, i32, &Effect<T, Variable>)> + '_> {
    match e {
        Effect::MachineCall(_, _, arguments) => match &arguments[0] {
            Variable::MachineCallParam(MachineCallVariable {
                identity_id,
                row_offset,
                ..
            }) => Box::new(std::iter::once((*identity_id, *row_offset, e))),
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

    use powdr_constraint_solver::range_constraint::RangeConstraint;
    use pretty_assertions::assert_eq;
    use test_log::test;

    use powdr_number::GoldilocksField;

    use crate::witgen::{
        data_structures::mutable_state::MutableState,
        global_constraints,
        jit::{effect::format_code, test_util::read_pil},
        machines::{machine_extractor::MachineExtractor, KnownMachine, Machine},
        FixedData,
    };

    use super::*;

    fn generate_for_block_machine<T: FieldElement>(
        input_pil: &str,
        machine_name: &str,
        receive_bus_id: Option<T>,
        num_inputs: usize,
        num_outputs: usize,
    ) -> Result<ProcessorResult<T>, String> {
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

        let bus_id = receive_bus_id.unwrap_or_else(|| {
            assert_eq!(
                machine_parts.bus_receives.len(),
                1,
                "No bus ID given and multiple receives present: {}",
                machine_parts.bus_receives.keys().format(", ")
            );
            *machine_parts
                .bus_receives
                .keys()
                .next()
                .expect("No bus receives found")
        });
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
            .generate_code(&mutable_state, bus_id, &known_values, None)
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
        let code = generate_for_block_machine::<GoldilocksField>(input, "Add", None, 2, 1)
            .unwrap()
            .code;
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
        let err_str =
            generate_for_block_machine::<GoldilocksField>(input, "Unconstrained", None, 2, 1)
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
        generate_for_block_machine::<GoldilocksField>(input, "NotStackable", None, 1, 0).unwrap();
    }

    #[test]
    fn binary() {
        let input = read_to_string("../test_data/pil/binary.pil").unwrap();
        let result =
            generate_for_block_machine::<GoldilocksField>(&input, "main_binary", None, 3, 1)
                .unwrap();
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
2**0 * main_binary::A[2] + 2**24 * main_binary::A_byte[2] := main_binary::A[3];
2**0 * main_binary::B[2] + 2**24 * main_binary::B_byte[2] := main_binary::B[3];
main_binary::operation_id_next[2] = main_binary::operation_id[3];
call_var(9, 2, 0) = main_binary::operation_id_next[2];
call_var(9, 2, 1) = main_binary::A_byte[2];
call_var(9, 2, 2) = main_binary::B_byte[2];
main_binary::operation_id[1] = main_binary::operation_id[2];
2**0 * main_binary::A[1] + 2**16 * main_binary::A_byte[1] := main_binary::A[2];
2**0 * main_binary::B[1] + 2**16 * main_binary::B_byte[1] := main_binary::B[2];
main_binary::operation_id_next[1] = main_binary::operation_id[2];
call_var(9, 1, 0) = main_binary::operation_id_next[1];
call_var(9, 1, 1) = main_binary::A_byte[1];
call_var(9, 1, 2) = main_binary::B_byte[1];
main_binary::operation_id[0] = main_binary::operation_id[1];
main_binary::operation_id_next[0] = main_binary::operation_id[1];
2**0 * main_binary::A[0] + 2**8 * main_binary::A_byte[0] := main_binary::A[1];
2**0 * main_binary::B[0] + 2**8 * main_binary::B_byte[0] := main_binary::B[1];
main_binary::operation_id_next[-1] = main_binary::operation_id[0];
call_var(9, 0, 0) = main_binary::operation_id_next[0];
main_binary::A_byte[-1] = main_binary::A[0];
call_var(9, 0, 1) = main_binary::A_byte[0];
main_binary::B_byte[-1] = main_binary::B[0];
call_var(9, 0, 2) = main_binary::B_byte[0];
call_var(9, -1, 0) = main_binary::operation_id_next[-1];
call_var(9, -1, 1) = main_binary::A_byte[-1];
call_var(9, -1, 2) = main_binary::B_byte[-1];
machine_call(2, [Known(call_var(9, -1, 0)), Known(call_var(9, -1, 1)), Known(call_var(9, -1, 2)), Unknown(call_var(9, -1, 3))]);
main_binary::C_byte[-1] = call_var(9, -1, 3);
main_binary::C[0] = main_binary::C_byte[-1];
machine_call(2, [Known(call_var(9, 0, 0)), Known(call_var(9, 0, 1)), Known(call_var(9, 0, 2)), Unknown(call_var(9, 0, 3))]);
main_binary::C_byte[0] = call_var(9, 0, 3);
main_binary::C[1] = (main_binary::C[0] + (256 * main_binary::C_byte[0]));
machine_call(2, [Known(call_var(9, 1, 0)), Known(call_var(9, 1, 1)), Known(call_var(9, 1, 2)), Unknown(call_var(9, 1, 3))]);
main_binary::C_byte[1] = call_var(9, 1, 3);
main_binary::C[2] = (main_binary::C[1] + (65536 * main_binary::C_byte[1]));
machine_call(2, [Known(call_var(9, 2, 0)), Known(call_var(9, 2, 1)), Known(call_var(9, 2, 2)), Unknown(call_var(9, 2, 3))]);
main_binary::C_byte[2] = call_var(9, 2, 3);
main_binary::C[3] = (main_binary::C[2] + (16777216 * main_binary::C_byte[2]));
params[3] = main_binary::C[3];"
        )
    }

    #[test]
    fn poseidon() {
        let input = read_to_string("../test_data/pil/poseidon_gl.pil").unwrap();
        generate_for_block_machine::<GoldilocksField>(&input, "main_poseidon", None, 12, 4)
            .map_err(|e| eprintln!("{e}"))
            .unwrap();
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
        let code = generate_for_block_machine::<GoldilocksField>(input, "Sub", None, 1, 1)
            .unwrap()
            .code;
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
        let code = generate_for_block_machine::<GoldilocksField>(input, "SubM", None, 1, 2)
            .unwrap()
            .code;
        assert_eq!(
            format_code(&code),
            "SubM::a[0] = params[0];
2**0 * SubM::c[0] + 2**8 * SubM::b[0] := SubM::a[0];
params[2] = SubM::c[0];
call_var(1, 0, 0) = SubM::c[0];
SubM::c[1] = SubM::c[0];
params[1] = SubM::b[0];
SubM::b[1] = SubM::b[0];
SubM::a[1] = (SubM::c[1] + (256 * SubM::b[1]));
call_var(1, 1, 0) = SubM::b[1];
machine_call(2, [Known(call_var(1, 0, 0))]);
machine_call(2, [Known(call_var(1, 1, 0))]);"
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
        let code = format_code(
            &generate_for_block_machine::<GoldilocksField>(input, "S", None, 2, 1)
                .unwrap()
                .code,
        );
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
        let code = format_code(
            &generate_for_block_machine::<GoldilocksField>(input, "S", None, 1, 2)
                .unwrap()
                .code,
        );
        assert_eq!(
            code,
            "S::a[0] = params[0];
S::b[0] = 0;
S::b[1] = 0;
S::c[0] = 1;
S::b[2] = 0;
S::c[1] = 1;
S::b[3] = 8;
S::c[2] = 1;
S::c[3] = 9;
params[1] = 0;
params[2] = 1;"
        );
    }

    #[test]
    fn intermediate() {
        let input = "
        namespace Main(256);
            col witness a, b, c;
            [a, b, c] is [S.X, S.Y, S.Z];
        namespace S(256);
            let X;
            let Y;
            let Z;
            let Z1: inter = X + Y;
            let Z2: inter = Z1 * Z1 + X;
            let Z3: inter = Z2 * Z2 + Y;
            let Z4: inter = Z3 * Z3 + X;
            let Z5: inter = Z4 * Z4 + Y;
            let Z6: inter = Z5 * Z5 + X;
            let Z7: inter = Z6 * Z6 + Z3;
            let Z8: inter = Z7 * Z7 + X;
            let Z9: inter = Z8 * Z8 + Y;
            let Z10: inter = Z9 * Z9 + X;
            let Z11: inter = Z10 * Z10 + Z8;
            let Z12: inter = Z11 * Z11 + X;
            let Z13: inter = Z12 * Z12 + Y;
            let Z14: inter = Z13 * Z13 + X;
            let Z15: inter = Z14 * Z14 + Z12;
            let Z16: inter = Z15 * Z15 + X;
            let Z17: inter = Z16 * Z16 + Y;
            let Z18: inter = Z17 * Z17 + X;
            let Z19: inter = Z18 * Z18 + Z16;
            let Z20: inter = Z19 * Z19 + X;
            let Z21: inter = Z20 * Z20 + Y;
            let Z22: inter = Z21 * Z21 + X;
            let Z23: inter = Z22 * Z22 + Z20;
            let Z24: inter = Z23 * Z23 + X;
            let Z25: inter = Z24 * Z24 + Y;
            let Z26: inter = Z25 * Z25 + X;
            let Z27: inter = Z26 * Z26 + Z24;
            let Z28: inter = Z27 * Z27 + X;
            Z = Z28;
        ";
        let code = format_code(
            &generate_for_block_machine::<GoldilocksField>(input, "S", None, 2, 1)
                .unwrap()
                .code,
        );
        assert_eq!(
            code,
            "\
S::X[0] = params[0];
S::Y[0] = params[1];
S::Z1[0] = (S::X[0] + S::Y[0]);
S::Z2[0] = (S::X[0] + (S::Z1[0] * S::Z1[0]));
S::Z3[0] = (S::Y[0] + (S::Z2[0] * S::Z2[0]));
S::Z4[0] = (S::X[0] + (S::Z3[0] * S::Z3[0]));
S::Z5[0] = (S::Y[0] + (S::Z4[0] * S::Z4[0]));
S::Z6[0] = (S::X[0] + (S::Z5[0] * S::Z5[0]));
S::Z7[0] = (S::Z3[0] + (S::Z6[0] * S::Z6[0]));
S::Z8[0] = (S::X[0] + (S::Z7[0] * S::Z7[0]));
S::Z9[0] = (S::Y[0] + (S::Z8[0] * S::Z8[0]));
S::Z10[0] = (S::X[0] + (S::Z9[0] * S::Z9[0]));
S::Z11[0] = (S::Z8[0] + (S::Z10[0] * S::Z10[0]));
S::Z12[0] = (S::X[0] + (S::Z11[0] * S::Z11[0]));
S::Z13[0] = (S::Y[0] + (S::Z12[0] * S::Z12[0]));
S::Z14[0] = (S::X[0] + (S::Z13[0] * S::Z13[0]));
S::Z15[0] = (S::Z12[0] + (S::Z14[0] * S::Z14[0]));
S::Z16[0] = (S::X[0] + (S::Z15[0] * S::Z15[0]));
S::Z17[0] = (S::Y[0] + (S::Z16[0] * S::Z16[0]));
S::Z18[0] = (S::X[0] + (S::Z17[0] * S::Z17[0]));
S::Z19[0] = (S::Z16[0] + (S::Z18[0] * S::Z18[0]));
S::Z20[0] = (S::X[0] + (S::Z19[0] * S::Z19[0]));
S::Z21[0] = (S::Y[0] + (S::Z20[0] * S::Z20[0]));
S::Z22[0] = (S::X[0] + (S::Z21[0] * S::Z21[0]));
S::Z23[0] = (S::Z20[0] + (S::Z22[0] * S::Z22[0]));
S::Z24[0] = (S::X[0] + (S::Z23[0] * S::Z23[0]));
S::Z25[0] = (S::Y[0] + (S::Z24[0] * S::Z24[0]));
S::Z26[0] = (S::X[0] + (S::Z25[0] * S::Z25[0]));
S::Z27[0] = (S::Z24[0] + (S::Z26[0] * S::Z26[0]));
S::Z28[0] = (S::X[0] + (S::Z27[0] * S::Z27[0]));
S::Z[0] = S::Z28[0];
params[2] = S::Z[0];"
        );
    }

    #[test]
    fn intermediate_array() {
        let input = "
        namespace Main(256);
            col witness a, b, c;
            [a, b, c] is [S.X, S.Y, S.Z];
        namespace S(256);
            let X;
            let Y;
            let Z;
            let Zi: inter[3] = [X + Y, 2 * X, Y * Y];
            Z = Zi[0] + Zi[1] + Zi[2];
        ";
        let code = format_code(
            &generate_for_block_machine::<GoldilocksField>(input, "S", None, 2, 1)
                .unwrap()
                .code,
        );
        assert_eq!(
            code,
            "\
S::X[0] = params[0];
S::Y[0] = params[1];
S::Zi[0][0] = (S::X[0] + S::Y[0]);
S::Zi[1][0] = (2 * S::X[0]);
S::Zi[2][0] = (S::Y[0] * S::Y[0]);
S::Z[0] = ((S::Zi[0][0] + S::Zi[1][0]) + S::Zi[2][0]);
params[2] = S::Z[0];"
        );
    }

    #[test]
    fn bit_decomp_negative() {
        let input = "
        namespace Main(256);
            col witness a, b, c;
            [a, b, c] is [S.Y, S.Z,  S.carry];
        namespace S(256);
            let BYTE: col = |i| i & 0xff;
            let X;
            let Y;
            let Z;
            let carry;
            carry * (1 - carry) = 0;
            [ X ] in [ BYTE ];
            [ Y ] in [ BYTE ];
            [ Z ] in [ BYTE ];
            X + Y = Z + 256 * carry;
        ";
        let code = format_code(
            &generate_for_block_machine::<GoldilocksField>(input, "S", None, 2, 1)
                .unwrap()
                .code,
        );
        assert_eq!(
            code,
            "\
S::Y[0] = params[0];
S::Z[0] = params[1];
-2**0 * S::X[0] + 2**8 * S::carry[0] := (S::Y[0] + -S::Z[0]);
params[2] = S::carry[0];"
        );
    }

    #[test]
    fn block_lookup_or_permutation() {
        let input = read_to_string("../test_data/pil/block_lookup_or_permutation.pil").unwrap();
        generate_for_block_machine::<GoldilocksField>(&input, "Or", Some(2.into()), 2, 1)
            .map_err(|e| eprintln!("{e}"))
            .unwrap();
    }

    #[test]
    fn bit_decomp_negative_concrete() {
        let input = "
        namespace Main(256);
            col witness a, b, c;
            [a, b, c] is [S.Y, S.Z,  S.carry];
        namespace S(256);
            let BYTE: col = |i| i & 0xff;
            let X;
            let Y;
            let Z;
            Y = 19;
            Z = 16;
            let carry;
            carry * (1 - carry) = 0;
            [ X ] in [ BYTE ];
            [ Y ] in [ BYTE ];
            [ Z ] in [ BYTE ];
            X + Y = Z + 256 * carry;
        ";
        let code = format_code(
            &generate_for_block_machine::<GoldilocksField>(input, "S", None, 2, 1)
                .unwrap()
                .code,
        );
        assert_eq!(
            code,
            "\
S::Y[0] = params[0];
S::Z[0] = params[1];
S::X[0] = 253;
S::carry[0] = 1;
params[2] = 1;"
        );
    }

    #[test]
    fn bit_decomp_negative_concrete_2() {
        let input = "
        namespace Main(256);
            col witness a, b, c;
            [a, b, c] is [S.Y, S.Z,  S.carry];
        namespace S(256);
            let BYTE: col = |i| i & 0xff;
            let X;
            let Y;
            let Z;
            Y = 1;
            Z = 16;
            let carry;
            carry * (1 - carry) = 0;
            [ X ] in [ BYTE ];
            [ Y ] in [ BYTE ];
            [ Z ] in [ BYTE ];
            X + Y = Z + 256 * carry;
        ";
        let code = format_code(
            &generate_for_block_machine::<GoldilocksField>(input, "S", None, 2, 1)
                .unwrap()
                .code,
        );
        assert_eq!(
            code,
            "\
S::Y[0] = params[0];
S::Z[0] = params[1];
S::X[0] = 15;
S::carry[0] = 0;
params[2] = 0;"
        );
    }
}
