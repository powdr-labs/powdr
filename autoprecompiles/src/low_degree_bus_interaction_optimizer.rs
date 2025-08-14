use itertools::Itertools;
use powdr_constraint_solver::constraint_system::{
    BusInteraction, BusInteractionHandler, ConstraintSystem,
};
use powdr_constraint_solver::grouped_expression::{GroupedExpression, NoRangeConstraints};
use powdr_constraint_solver::inliner::DegreeBound;
use powdr_constraint_solver::range_constraint::RangeConstraint;
use powdr_constraint_solver::runtime_constant::RuntimeConstant;
use powdr_constraint_solver::solver::Solver;
use powdr_number::FieldElement;
use powdr_number::LargeInt;
use std::fmt::Display;
use std::hash::Hash;
use std::marker::PhantomData;

use crate::constraint_optimizer::IsBusStateful;
use crate::range_constraint_optimizer::{RangeConstraintHandler, RangeConstraints};

/// An optimizer that replaces some stateless bus interactions (a.k.a. lookups)
/// by low-degree algebraic constraints.
pub struct LowDegreeBusInteractionOptimizer<'a, T, V, S, B> {
    solver: &'a mut S,
    bus_interaction_handler: B,
    degree_bound: DegreeBound,
    _phantom: PhantomData<(T, V)>,
}

impl<
        'a,
        T: FieldElement,
        V: Ord + Clone + Ord + Eq + Display + Hash,
        S: Solver<T, V>,
        B: BusInteractionHandler<T> + IsBusStateful<T> + RangeConstraintHandler<T>,
    > LowDegreeBusInteractionOptimizer<'a, T, V, S, B>
{
    pub fn new(solver: &'a mut S, bus_interaction_handler: B, degree_bound: DegreeBound) -> Self {
        Self {
            solver,
            bus_interaction_handler,
            degree_bound,
            _phantom: PhantomData,
        }
    }

    pub fn optimize(self, mut system: ConstraintSystem<T, V>) -> ConstraintSystem<T, V> {
        let mut new_constraints: Vec<GroupedExpression<T, V>> = vec![];
        system.bus_interactions = system
            .bus_interactions
            .into_iter()
            .flat_map(|bus_int| {
                if let Some(replacement) = self.try_replace_bus_interaction(&bus_int) {
                    // If we found a replacement, add the polynomial constraints and replace
                    // the bus interaction with interactions implementing the range constraints.
                    // Note that many of these may be optimized away by the range constraint optimizer.
                    new_constraints.push(replacement);
                    self.bus_interaction_handler
                        .batch_make_range_constraints(self.range_constraints(&bus_int))
                } else {
                    // Keep the bus interaction as is if a replacement can't be found.
                    vec![bus_int]
                }
            })
            .collect();

        // Knowing the low-degree functions might help the solver.
        // The range constraints do not need to be added, because they don't carry information
        // that is not already implied by the existing bus interactions.
        self.solver
            .add_algebraic_constraints(new_constraints.iter().cloned());

        system.algebraic_constraints.extend(new_constraints);
        system
    }

    /// Checks whether a bus interaction can be replaced by a low-degree constraint + range checks.
    /// Returns None if no replacement is found.
    fn try_replace_bus_interaction(
        &self,
        bus_interaction: &BusInteraction<GroupedExpression<T, V>>,
    ) -> Option<GroupedExpression<T, V>> {
        let bus_id = bus_interaction.bus_id.try_to_number()?;
        if self.bus_interaction_handler.is_stateful(bus_id) {
            return None;
        }

        self.symbolic_function_candidates_with_small_domain(bus_interaction)
            .into_iter()
            .find_map(|symbolic_function| {
                let low_degree_function =
                    self.find_low_degree_function(bus_interaction, &symbolic_function)?;

                // Build polynomial constraint
                let symbolic_inputs = symbolic_function
                    .inputs
                    .into_iter()
                    .map(|input| input.expression)
                    .collect();
                let low_degree_function = low_degree_function(symbolic_inputs);
                let polynomial_constraint =
                    symbolic_function.output.expression.clone() - low_degree_function;

                // Check degree
                let within_degree_bound =
                    polynomial_constraint.degree() <= self.degree_bound.identities;
                within_degree_bound.then_some(polynomial_constraint)
            })
    }

    /// Given a bus interaction of 2 or 3 unknown fields, finds all combinations of (symbolic)
    /// inputs and outputs where the input space is small enough.
    fn symbolic_function_candidates_with_small_domain(
        &self,
        bus_interaction: &BusInteraction<GroupedExpression<T, V>>,
    ) -> Vec<SymbolicFunction<T, V>> {
        let unknown_fields = bus_interaction
            .payload
            .iter()
            .cloned()
            .enumerate()
            .filter(|(_i, expr)| expr.try_to_number().is_none())
            .map(|(index, expression)| {
                let range_constraint = self.solver.range_constraint_for_expression(&expression);
                SymbolicField {
                    index,
                    expression,
                    range_constraint,
                }
            })
            .collect_vec();

        let unknown_field_count = unknown_fields.len();
        // Currently, we only have hypotheses for:
        // - 2 unknown fields (1 input, 1 output)
        // - 3 unknown fields (2 inputs, 1 output)
        if !(unknown_field_count == 2 || unknown_field_count == 3) {
            return Vec::new();
        }

        unknown_fields
            .into_iter()
            .permutations(unknown_field_count)
            .map(|mut fields| {
                let output = fields.pop().unwrap();
                SymbolicFunction {
                    inputs: fields,
                    output,
                }
            })
            .filter(|function| {
                self.has_few_possible_values(
                    function.inputs.iter().map(|f| f.range_constraint.clone()),
                    MAX_DOMAIN_SIZE,
                )
            })
            .collect_vec()
    }

    /// Given a list of range constraints, computes whether space of all possible values
    /// is small enough.
    fn has_few_possible_values(
        &self,
        range_constraints: impl Iterator<Item = RangeConstraint<T>>,
        max_size: u64,
    ) -> bool {
        range_constraints
            .map(|rc| {
                // TODO: This should share code with `has_few_possible_assignments`,
                // But this only currently only considers the range width which ignores the mask
                // and might be way larger than the actual number of allowed values.
                rc.range_width().try_into_u64().and_then(|width| {
                    if width < 1 << 16 {
                        Some(rc.allowed_values().count() as u64)
                    } else {
                        None
                    }
                })
            })
            .try_fold(1u64, |acc, x| acc.checked_mul(x?))
            .is_some_and(|count| count <= max_size)
    }

    /// Given a bus interaction and a symbolic input-output pair, tries to find a low-degree function
    /// by testing all of the hard-coded hypotheses against set of all concrete input-output pairs.
    fn find_low_degree_function(
        &self,
        bus_interaction: &BusInteraction<GroupedExpression<T, V>>,
        symbolic_function: &SymbolicFunction<T, V>,
    ) -> Option<LowDegreeFunction<T, V>> {
        let mut hypotheses = hypotheses(symbolic_function.inputs.len());

        // Generate the function graph, to match against the hypotheses.
        let all_possible_assignments =
            self.concrete_input_output_pairs(bus_interaction, symbolic_function);

        for assignment in all_possible_assignments {
            let Ok((inputs, output)) = assignment else {
                // We can't enumerate all possible assignments, so the hypotheses can't be tested.
                return None;
            };
            let inputs = inputs
                .into_iter()
                .map(|value| GroupedExpression::from_number(value))
                .collect::<Vec<_>>();
            hypotheses.retain(|hypothesis| {
                let hypothesis_evaluation = hypothesis(inputs.clone());
                hypothesis_evaluation.try_to_number().unwrap() == output
            });
            if hypotheses.is_empty() {
                // No hypothesis left
                return None;
            }
        }

        // If we got this far, the hypothesis is correct!
        Some(hypotheses.into_iter().exactly_one().unwrap_or_else(|_| {
            panic!("Expected exactly one multilinear extension, but got multiple.")
        }))
    }

    /// Returns the range constraints enforced by the given bus interaction.
    fn range_constraints(
        &self,
        bus_interaction: &BusInteraction<GroupedExpression<T, V>>,
    ) -> RangeConstraints<T, V> {
        let range_constraints = self
            .bus_interaction_handler
            .handle_bus_interaction(bus_interaction.to_range_constraints(&NoRangeConstraints));
        bus_interaction
            .payload
            .iter()
            .zip_eq(range_constraints.payload)
            .map(|(expr, rc)| (expr.clone(), rc))
            .filter(|(expr, _rc)| expr.try_to_number().is_none())
            .collect()
    }

    /// Generate all concrete input-output pairs given a symbolic one.
    ///
    /// The inputs are generated as the cross product of all allowed values of the
    /// individual inputs.
    /// The outputs are generated by asking the bus interaction handler for each input assignment.
    ///
    /// If at any time (1) the inputs violate a constraint or (2) the outputs are not unique,
    /// an error is yielded.
    fn concrete_input_output_pairs<'b>(
        &'b self,
        bus_interaction: &BusInteraction<GroupedExpression<T, V>>,
        input_output_pair: &'b SymbolicFunction<T, V>,
    ) -> impl Iterator<Item = Result<(Vec<T>, T), ()>> + 'b {
        let bus_interaction = bus_interaction.to_range_constraints(self.solver);

        // Consider all possible input assignments, which is the cross product of all allowed values.
        let input_assignments = input_output_pair
            .inputs
            .iter()
            .map(move |input| {
                input
                    .range_constraint
                    .allowed_values()
                    .map(|v| (input.index, v))
                    .collect_vec()
            })
            .multi_cartesian_product();

        // For each input assignment, try it and ask the bus interaction handler if there
        // is a unique output assignment.
        input_assignments.map(move |assignment| {
            // Set all inputs to concrete values
            let mut bus_interaction = bus_interaction.clone();
            for (i, value) in assignment.iter() {
                bus_interaction.payload[*i] = RangeConstraint::from_value(*value);
            }

            let inputs = assignment.into_iter().map(|(_i, value)| value).collect();

            // Get the output from the bus interaction handler, if it exists and is unique.
            let output = self
                .bus_interaction_handler
                .handle_bus_interaction_checked(bus_interaction)
                // If the assignment violates a constraint, return an error.
                .map_err(|_| ())?
                .payload
                .get(input_output_pair.output.index)
                .unwrap()
                .try_to_single_value()
                // If the output is not unique, return an error.
                .ok_or(())?;
            Ok((inputs, output))
        })
    }
}

/// Represents a low-degree function, mapping a list of inputs to a single output.
type LowDegreeFunction<T, V> = Box<dyn Fn(Vec<GroupedExpression<T, V>>) -> GroupedExpression<T, V>>;

/// The maximum size of the input domain for low-degree functions.
const MAX_DOMAIN_SIZE: u64 = 256;

/// Represents a bus interaction field.
#[derive(Clone, Debug)]
struct SymbolicField<T: FieldElement, V> {
    /// The index into the bus interaction payload
    index: usize,
    /// The expression in the bus interaction payload
    expression: GroupedExpression<T, V>,
    /// The range constraint for the expression
    range_constraint: RangeConstraint<T>,
}

#[derive(Clone, Debug)]
struct SymbolicFunction<T: FieldElement, V> {
    inputs: Vec<SymbolicField<T, V>>,
    output: SymbolicField<T, V>,
}

/// Some well-known low-degree functions that are tested against the input-output pairs.
fn hypotheses<T: FieldElement, V: Ord + Clone + Hash + Eq>(
    num_inputs: usize,
) -> Vec<LowDegreeFunction<T, V>> {
    match num_inputs {
        1 => vec![
            // Identity function
            Box::new(|inputs| inputs[0].clone()),
            // Logical not (1 bit)
            Box::new(|inputs| GroupedExpression::from_number(T::from_u64(1)) - inputs[0].clone()),
            // Logical not (8 bit)
            Box::new(|inputs| {
                GroupedExpression::from_number(T::from_u64(0xff)) - inputs[0].clone()
            }),
            // Logical not (16 bit)
            Box::new(|inputs| {
                GroupedExpression::from_number(T::from_u64(0xffff)) - inputs[0].clone()
            }),
        ],
        2 => vec![
            // Identity on x
            Box::new(|inputs| inputs[0].clone()),
            // Identity on y
            Box::new(|inputs| inputs[1].clone()),
            // x + y
            Box::new(|inputs| inputs[0].clone() + inputs[1].clone()),
            // AND on bits:
            Box::new(|inputs| inputs[0].clone() * inputs[1].clone()),
            // OR on bits:
            Box::new(|inputs| {
                inputs[0].clone() + inputs[1].clone() - (inputs[0].clone() * inputs[1].clone())
            }),
            // XOR on bits:
            Box::new(|inputs| {
                inputs[0].clone() + inputs[1].clone()
                    - GroupedExpression::from_number(T::from_u64(2))
                        * (inputs[0].clone() * inputs[1].clone())
            }),
        ],
        _ => panic!("Unexpected number of inputs: {num_inputs}"),
    }
}

#[cfg(test)]
mod tests {

    use std::array::from_fn;

    use powdr_constraint_solver::solver::new_solver;
    use powdr_number::BabyBearField;

    use crate::range_constraint_optimizer::RangeConstraints;

    use super::*;

    pub type Var = &'static str;
    pub fn var(name: Var) -> GroupedExpression<BabyBearField, Var> {
        GroupedExpression::from_unknown_variable(name)
    }

    pub fn constant(value: u64) -> GroupedExpression<BabyBearField, Var> {
        GroupedExpression::from_number(BabyBearField::from(value))
    }

    #[derive(Clone, Debug)]
    struct XorBusHandler;
    impl BusInteractionHandler<BabyBearField> for XorBusHandler {
        fn handle_bus_interaction(
            &self,
            bus_interaction: BusInteraction<RangeConstraint<BabyBearField>>,
        ) -> BusInteraction<RangeConstraint<BabyBearField>> {
            let range_constraints = match (
                bus_interaction.payload[0].try_to_single_value(),
                bus_interaction.payload[1].try_to_single_value(),
            ) {
                // If x and y are known, compute z
                (Some(x), Some(y)) => {
                    let z = BabyBearField::from(x.to_degree() ^ y.to_degree());
                    [
                        RangeConstraint::from_value(x),
                        RangeConstraint::from_value(y),
                        RangeConstraint::from_value(z),
                    ]
                }
                // By default, just return byte range constraints
                _ => from_fn(|_i| RangeConstraint::from_mask(0xffu32)),
            };
            BusInteraction {
                bus_id: bus_interaction.bus_id,
                payload: range_constraints.into_iter().collect(),
                multiplicity: bus_interaction.multiplicity,
            }
        }
    }
    impl IsBusStateful<BabyBearField> for XorBusHandler {
        fn is_stateful(&self, _bus_id: BabyBearField) -> bool {
            false
        }
    }
    impl RangeConstraintHandler<BabyBearField> for XorBusHandler {
        fn pure_range_constraints<V: Ord + Clone + Eq + Display + Hash>(
            &self,
            _bus_interaction: &BusInteraction<GroupedExpression<BabyBearField, V>>,
        ) -> Option<RangeConstraints<BabyBearField, V>> {
            unreachable!()
        }

        fn batch_make_range_constraints<V: Ord + Clone + Eq + Display + Hash>(
            &self,
            _range_constraints: RangeConstraints<BabyBearField, V>,
        ) -> Vec<BusInteraction<GroupedExpression<BabyBearField, V>>> {
            unreachable!()
        }
    }

    fn compute_replacement(
        mut solver: impl Solver<BabyBearField, Var>,
        bus_interaction: &BusInteraction<GroupedExpression<BabyBearField, Var>>,
    ) -> Option<GroupedExpression<BabyBearField, Var>> {
        let optimizer = LowDegreeBusInteractionOptimizer {
            solver: &mut solver,
            bus_interaction_handler: XorBusHandler,
            degree_bound: DegreeBound {
                identities: 2,
                bus_interactions: 1,
            },
            _phantom: PhantomData,
        };
        optimizer.try_replace_bus_interaction(bus_interaction)
    }

    #[test]
    fn test_try_replace_bus_interaction_generic_xor() {
        let mut solver = new_solver(ConstraintSystem::default(), XorBusHandler);
        // The input search space is small, but xor is not linear.
        solver.add_range_constraint(&"x", RangeConstraint::from_mask(0xfu32));
        solver.add_range_constraint(&"y", RangeConstraint::from_mask(0xfu32));
        let bus_interaction = BusInteraction {
            bus_id: constant(0),
            payload: vec![var("x"), var("y"), var("z")],
            multiplicity: constant(1),
        };
        let replacement = compute_replacement(solver, &bus_interaction);
        assert!(replacement.is_none());
    }

    #[test]
    fn test_try_replace_bus_interaction_logical_not() {
        let mut solver = new_solver(ConstraintSystem::default(), XorBusHandler);
        // not(x) is a linear function (255 - x).
        solver.add_range_constraint(&"x", RangeConstraint::from_mask(0xffu32));
        let bus_interaction = BusInteraction {
            bus_id: constant(0),
            payload: vec![var("x"), constant(0xff), var("z")],
            multiplicity: constant(1),
        };
        let Some(replacement) = compute_replacement(solver, &bus_interaction) else {
            panic!("Expected a replacement")
        };
        assert_eq!(replacement.to_string(), "x + z - 255");
    }

    #[test]
    fn test_try_replace_bus_interaction_binary_inputs() {
        let mut solver = new_solver(ConstraintSystem::default(), XorBusHandler);
        // Any function on two bits has a multilinear extension.
        solver.add_range_constraint(&"x", RangeConstraint::from_mask(1u32));
        solver.add_range_constraint(&"y", RangeConstraint::from_mask(1u32));
        let bus_interaction = BusInteraction {
            bus_id: constant(0),
            payload: vec![var("x"), var("y"), var("z")],
            multiplicity: constant(1),
        };
        let Some(replacement) = compute_replacement(solver, &bus_interaction) else {
            panic!("Expected a replacement")
        };
        assert_eq!(replacement.to_string(), "(2 * x) * (y) - x - y + z");
    }

    #[test]
    fn test_try_replace_bus_interaction_disjoint_masks() {
        let mut solver = new_solver(ConstraintSystem::default(), XorBusHandler);
        // Because the masks are disjoint, there is a multilinear extension: z = x + y.
        solver.add_range_constraint(&"x", RangeConstraint::from_mask(0x0fu32));
        solver.add_range_constraint(&"y", RangeConstraint::from_mask(0xf0u32));
        let bus_interaction = BusInteraction {
            bus_id: constant(0),
            payload: vec![var("x"), var("y"), var("z")],
            multiplicity: constant(1),
        };
        let Some(replacement) = compute_replacement(solver, &bus_interaction) else {
            panic!("Expected a replacement")
        };
        assert_eq!(replacement.to_string(), "-(x + y - z)");
    }

    #[test]
    fn test_range_constraints() {
        let mut solver = new_solver(ConstraintSystem::default(), XorBusHandler);
        let optimizer = LowDegreeBusInteractionOptimizer {
            solver: &mut solver,
            bus_interaction_handler: XorBusHandler,
            degree_bound: DegreeBound {
                identities: 2,
                bus_interactions: 1,
            },
            _phantom: PhantomData,
        };
        let bus_interaction = BusInteraction {
            bus_id: constant(0),
            payload: vec![var("x"), var("y"), var("z")],
            multiplicity: constant(1),
        };
        let range_constraints = optimizer.range_constraints(&bus_interaction);
        assert_eq!(
            range_constraints,
            vec![
                (var("x"), RangeConstraint::from_mask(0xffu32)),
                (var("y"), RangeConstraint::from_mask(0xffu32)),
                (var("z"), RangeConstraint::from_mask(0xffu32)),
            ]
        );
    }
}
