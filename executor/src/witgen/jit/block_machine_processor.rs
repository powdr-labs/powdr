use std::collections::HashSet;

use bit_vec::BitVec;
use powdr_ast::analyzed::{AlgebraicReference, Identity};
use powdr_number::FieldElement;

use crate::witgen::{machines::MachineParts, FixedData};

use super::{
    effect::Effect,
    variable::Variable,
    witgen_inference::{FixedEvaluator, WitgenInference},
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
    pub fn generate_code(
        &self,
        identity_id: u64,
        known_args: &BitVec,
    ) -> Result<Vec<Effect<T, Variable>>, String> {
        let connection_rhs = self.machine_parts.connections[&identity_id].right;
        assert_eq!(connection_rhs.expressions.len(), known_args.len());

        // Set up WitgenInference with known arguments.
        let known_variables = known_args
            .iter()
            .enumerate()
            .filter_map(|(i, is_input)| is_input.then_some(Variable::Param(i)))
            .collect::<HashSet<_>>();
        let mut witgen = WitgenInference::new(self.fixed_data, self, known_variables);

        // In the latch row, set the RHS selector to 1.
        witgen.assign_constant(&connection_rhs.selector, self.latch_row as i32, T::one());

        // For each argument, connect the expression on the RHS with the formal parameter.
        for (index, expr) in connection_rhs.expressions.iter().enumerate() {
            witgen.assign_variable(expr, self.latch_row as i32, Variable::Param(index));
        }

        // Solve for the block witness.
        // Fails if any machine call cannot be completed.
        self.solve_block(&mut witgen)?;

        for (index, expr) in connection_rhs.expressions.iter().enumerate() {
            if !witgen.is_known(&Variable::Param(index)) {
                return Err(format!(
                    "Unable to derive algorithm to compute output value \"{expr}\""
                ));
            }
        }

        Ok(witgen.code())
    }

    /// Repeatedly processes all identities on all rows, until no progress is made.
    /// Fails iff there are incomplete machine calls in the latch row.
    fn solve_block(&self, witgen: &mut WitgenInference<'a, T, &Self>) -> Result<(), String> {
        let mut complete = HashSet::new();
        for iteration in 0.. {
            let mut progress = false;

            // TODO: This algorithm is assuming a rectangular block shape.
            for row in 0..self.block_size {
                for id in &self.machine_parts.identities {
                    if !complete.contains(&(id.id(), row)) {
                        let result = witgen.process_identity(id, row as i32);
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
    use std::{collections::BTreeMap, fs::read_to_string};

    use bit_vec::BitVec;
    use powdr_ast::analyzed::{
        AlgebraicExpression, AlgebraicReference, Analyzed, SelectedExpressions,
    };
    use powdr_number::GoldilocksField;

    use crate::{
        constant_evaluator,
        witgen::{
            global_constraints,
            jit::{effect::Effect, test_util::format_code},
            machines::{Connection, ConnectionKind, MachineParts},
            FixedData,
        },
    };

    use super::{BlockMachineProcessor, Variable};

    fn generate_for_block_machine(
        input_pil: &str,
        block_size: usize,
        latch_row: usize,
        selector_name: &str,
        input_names: &[&str],
        output_names: &[&str],
    ) -> Result<Vec<Effect<GoldilocksField, Variable>>, String> {
        let analyzed: Analyzed<GoldilocksField> =
            powdr_pil_analyzer::analyze_string(input_pil).unwrap();
        let fixed_col_vals = constant_evaluator::generate(&analyzed);
        let fixed_data = FixedData::new(&analyzed, &fixed_col_vals, &[], Default::default(), 0);
        let (fixed_data, retained_identities) =
            global_constraints::set_global_constraints(fixed_data, &analyzed.identities);

        // Build a connection that encodes:
        // [] is <selector_name> $ [<input_names...>, <output_names...>]
        let witnesses_by_name = analyzed
            .committed_polys_in_source_order()
            .flat_map(|(symbol, _)| symbol.array_elements())
            .collect::<BTreeMap<_, _>>();
        let to_expr = |name: &str| {
            let (column_name, next) = if let Some(name) = name.strip_suffix("'") {
                (name, true)
            } else {
                (name, false)
            };
            AlgebraicExpression::Reference(AlgebraicReference {
                name: name.to_string(),
                poly_id: witnesses_by_name[column_name],
                next,
            })
        };
        let rhs = input_names
            .iter()
            .chain(output_names)
            .map(|name| to_expr(name))
            .collect::<Vec<_>>();
        let right = SelectedExpressions {
            selector: to_expr(selector_name),
            expressions: rhs,
        };
        // The LHS is not used by the processor.
        let left = SelectedExpressions::default();
        let connection = Connection {
            id: 0,
            left: &left,
            right: &right,
            kind: ConnectionKind::Permutation,
            multiplicity_column: None,
        };

        let machine_parts = MachineParts::new(
            &fixed_data,
            [(0, connection)].into_iter().collect(),
            retained_identities,
            witnesses_by_name.values().copied().collect(),
            // No prover functions
            Vec::new(),
        );

        let processor = BlockMachineProcessor {
            fixed_data: &fixed_data,
            machine_parts,
            block_size,
            latch_row,
        };

        let known_values = BitVec::from_iter(
            input_names
                .iter()
                .map(|_| true)
                .chain(output_names.iter().map(|_| false)),
        );

        processor.generate_code(0, &known_values)
    }

    #[test]
    fn add() {
        let input = "
        namespace Add(256);
            col witness sel, a, b, c;
            c = a + b;
        ";
        let code =
            generate_for_block_machine(input, 1, 0, "Add::sel", &["Add::a", "Add::b"], &["Add::c"]);
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
        namespace Unconstrained(256);
            col witness sel, a, b, c;
            a + b = 0;
        ";
        let err_str = generate_for_block_machine(
            input,
            1,
            0,
            "Unconstrained::sel",
            &["Unconstrained::a", "Unconstrained::b"],
            &["Unconstrained::c"],
        )
        .err()
        .unwrap();
        assert_eq!(
            err_str,
            "Unable to derive algorithm to compute output value \"Unconstrained::c\""
        );
    }

    #[test]
    // TODO: Currently fails, because the machine has a non-rectangular block shape.
    #[should_panic = "Incomplete machine calls"]
    fn binary() {
        let input = read_to_string("../test_data/pil/binary.pil").unwrap();
        generate_for_block_machine(
            &input,
            4,
            3,
            "main_binary::sel[0]",
            &["main_binary::A", "main_binary::B"],
            &["main_binary::C"],
        )
        .unwrap();
    }

    #[test]
    fn poseidon() {
        let input = read_to_string("../test_data/pil/poseidon_gl.pil").unwrap();
        let array_element = |name: &str, i: usize| {
            &*Box::leak(format!("main_poseidon::{name}[{i}]").into_boxed_str())
        };
        generate_for_block_machine(
            &input,
            31,
            0,
            "main_poseidon::sel[0]",
            &(0..12)
                .map(|i| array_element("state", i))
                .collect::<Vec<_>>(),
            &(0..4)
                .map(|i| array_element("output", i))
                .collect::<Vec<_>>(),
        )
        .unwrap();
    }
}
