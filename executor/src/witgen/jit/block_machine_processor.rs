use std::collections::HashSet;

use bit_vec::BitVec;
use itertools::Itertools;
use powdr_ast::analyzed::{AlgebraicReference, Identity, SelectedExpressions};
use powdr_number::FieldElement;

use crate::witgen::{jit::effect::format_code, machines::MachineParts, FixedData};

use super::{
    effect::Effect,
    variable::Variable,
    witgen_inference::{CanProcessCall, FixedEvaluator, WitgenInference},
};

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
    pub fn generate_code<CanProcess: CanProcessCall<T> + Clone>(
        &self,
        can_process: CanProcess,
        identity_id: u64,
        known_args: &BitVec,
    ) -> Result<Vec<Effect<T, Variable>>, String> {
        let connection = self.machine_parts.connections[&identity_id];
        assert_eq!(connection.right.expressions.len(), known_args.len());

        // Set up WitgenInference with known arguments.
        let known_variables = known_args
            .iter()
            .enumerate()
            .filter_map(|(i, is_input)| is_input.then_some(Variable::Param(i)))
            .collect::<HashSet<_>>();
        let mut witgen = WitgenInference::new(self.fixed_data, self, known_variables);

        // In the latch row, set the RHS selector to 1.
        witgen.assign_constant(&connection.right.selector, self.latch_row as i32, T::one());

        // For each argument, connect the expression on the RHS with the formal parameter.
        for (index, expr) in connection.right.expressions.iter().enumerate() {
            witgen.assign_variable(expr, self.latch_row as i32, Variable::Param(index));
        }

        // Solve for the block witness.
        // Fails if any machine call cannot be completed.
        match self.solve_block(can_process, &mut witgen, connection.right) {
            Ok(()) => Ok(witgen.code()),
            Err(e) => {
                log::debug!("\nCode generation failed for connection:\n  {connection}");
                let known_args_str = known_args
                    .iter()
                    .enumerate()
                    .filter_map(|(i, b)| b.then_some(connection.right.expressions[i].to_string()))
                    .join("\n  ");
                log::debug!("Known arguments:\n  {known_args_str}");
                log::debug!("Error:\n  {e}");
                log::debug!(
                    "The following code was generated so far:\n{}",
                    format_code(witgen.code().as_slice())
                );
                Err(format!("Code generation failed: {e}\nRun with RUST_LOG=debug to see the code generated so far."))
            }
        }
    }

    /// Repeatedly processes all identities on all rows, until no progress is made.
    /// Fails iff there are incomplete machine calls in the latch row.
    fn solve_block<CanProcess: CanProcessCall<T> + Clone>(
        &self,
        can_process: CanProcess,
        witgen: &mut WitgenInference<'a, T, &Self>,
        connection_rhs: &SelectedExpressions<T>,
    ) -> Result<(), String> {
        let mut complete = HashSet::new();
        for iteration in 0.. {
            let mut progress = false;

            // TODO: This algorithm is assuming a rectangular block shape.
            for row in 0..self.block_size {
                for id in &self.machine_parts.identities {
                    if !complete.contains(&(id.id(), row)) {
                        let result = witgen.process_identity(can_process.clone(), id, row as i32);
                        if result.complete {
                            complete.insert((id.id(), row));
                        }
                        progress |= result.progress;
                    }
                }
            }
            if !progress {
                log::trace!(
                    "Finishing block machine witgen code generation after {iteration} iterations"
                );
                break;
            }
        }

        for (index, expr) in connection_rhs.expressions.iter().enumerate() {
            if !witgen.is_known(&Variable::Param(index)) {
                return Err(format!(
                    "Unable to derive algorithm to compute output value \"{expr}\""
                ));
            }
        }

        // If any machine call could not be completed, that's bad because machine calls typically have side effects.
        // So, the underlying lookup / permutation / bus argument likely does not hold.
        // TODO: This assumes a rectangular block shape.
        let has_incomplete_machine_calls = (0..self.block_size)
            .flat_map(|row| {
                self.machine_parts
                    .identities
                    .iter()
                    .filter(|id| is_machine_call(id))
                    .map(move |id| (id, row))
            })
            .any(|(identity, row)| !complete.contains(&(identity.id(), row)));

        match has_incomplete_machine_calls {
            true => Err("Incomplete machine calls".to_string()),
            false => Ok(()),
        }
    }
}

fn is_machine_call<T>(identity: &Identity<T>) -> bool {
    match identity {
        Identity::Lookup(_)
        | Identity::Permutation(_)
        | Identity::PhantomLookup(_)
        | Identity::PhantomPermutation(_)
        | Identity::PhantomBusInteraction(_) => true,
        Identity::Polynomial(_) | Identity::Connect(_) => false,
    }
}

impl<T: FieldElement> FixedEvaluator<T> for &BlockMachineProcessor<'_, T> {
    fn evaluate(&self, var: &AlgebraicReference, row_offset: i32) -> Option<T> {
        assert!(var.is_fixed());
        let values = self.fixed_data.fixed_cols[&var.poly_id].values_max_size();
        let row = (row_offset + var.next as i32 + values.len() as i32) as usize % values.len();
        Some(values[row])
    }
}

#[cfg(test)]
mod test {
    use std::fs::read_to_string;

    use test_log::test;

    use powdr_number::GoldilocksField;

    use crate::witgen::{
        data_structures::mutable_state::MutableState,
        global_constraints,
        jit::{effect::Effect, test_util::read_pil},
        machines::{machine_extractor::MachineExtractor, KnownMachine, Machine},
        FixedData,
    };

    use super::*;

    fn generate_for_block_machine(
        input_pil: &str,
        machine_name: &str,
        num_inputs: usize,
        num_outputs: usize,
    ) -> Result<Vec<Effect<GoldilocksField, Variable>>, String> {
        let (analyzed, fixed_col_vals) = read_pil(input_pil);

        let fixed_data = FixedData::new(&analyzed, &fixed_col_vals, &[], Default::default(), 0);
        let (fixed_data, retained_identities) =
            global_constraints::set_global_constraints(fixed_data, &analyzed.identities);
        let machines = MachineExtractor::new(&fixed_data).split_out_machines(retained_identities);
        let mutable_state = MutableState::new(machines.into_iter(), &|_| {
            Err("Query not implemented".to_string())
        });

        let machine = mutable_state.get_machine(machine_name);
        let ((machine_parts, block_size, latch_row), connection_ids) = match *machine.borrow() {
            KnownMachine::BlockMachine(ref m) => (m.machine_info(), m.identity_ids()),
            _ => panic!("Expected a block machine"),
        };
        assert_eq!(connection_ids.len(), 1);

        let processor = BlockMachineProcessor {
            fixed_data: &fixed_data,
            machine_parts,
            block_size,
            latch_row,
        };

        let known_values = BitVec::from_iter(
            (0..num_inputs)
                .map(|_| true)
                .chain((0..num_outputs).map(|_| false)),
        );

        processor.generate_code(&mutable_state, connection_ids[0], &known_values)
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
        let code = generate_for_block_machine(input, "Add", 2, 1);
        assert_eq!(
            format_code(&code.unwrap()),
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
            .contains("Unable to derive algorithm to compute output value \"Unconstrained::c\""));
    }

    #[test]
    // TODO: Currently fails, because the machine has a non-rectangular block shape.
    #[should_panic = "Unable to derive algorithm to compute output value \\\"main_binary::C\\\""]
    fn binary() {
        let input = read_to_string("../test_data/pil/binary.pil").unwrap();
        generate_for_block_machine(&input, "main_binary", 3, 1).unwrap();
    }

    #[test]
    fn poseidon() {
        let input = read_to_string("../test_data/pil/poseidon_gl.pil").unwrap();
        generate_for_block_machine(&input, "main_poseidon", 12, 4).unwrap();
    }
}
