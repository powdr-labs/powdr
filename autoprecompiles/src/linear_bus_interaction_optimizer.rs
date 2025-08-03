use auto_enums::auto_enum;
use itertools::Itertools;
use num_traits::Zero;
use powdr_constraint_solver::constraint_system::{
    BusInteraction, BusInteractionHandler, ConstraintSystem,
};
use powdr_constraint_solver::grouped_expression::GroupedExpression;
use powdr_constraint_solver::range_constraint::RangeConstraint;
use powdr_constraint_solver::runtime_constant::VarTransformable;
use powdr_constraint_solver::solver::Solver;
use powdr_number::FieldElement;
use powdr_number::LargeInt;
use std::collections::{BTreeMap, BTreeSet};
use std::marker::PhantomData;

use crate::constraint_optimizer::IsBusStateful;

pub struct LinearBusInteractionOptimizer<
    'a,
    T: FieldElement,
    V: Ord + Clone + Eq,
    S: Solver<T, V>,
    B: BusInteractionHandler<T> + IsBusStateful<T>,
> {
    solver: &'a S,
    bus_interaction_handler: B,
    _phantom: PhantomData<(T, V)>,
}

impl<
        'a,
        T: FieldElement,
        V: Ord + Clone + Ord + Eq,
        S: Solver<T, V>,
        B: BusInteractionHandler<T> + IsBusStateful<T>,
    > LinearBusInteractionOptimizer<'a, T, V, S, B>
{
    pub fn new(solver: &'a S, bus_interaction_handler: B) -> Self {
        Self {
            solver,
            bus_interaction_handler,
            _phantom: PhantomData,
        }
    }

    pub fn optimize(&self, mut system: ConstraintSystem<T, V>) -> ConstraintSystem<T, V> {
        let mut new_constraints: Vec<GroupedExpression<T, V>> = vec![];
        system.bus_interactions.retain(|bus_int| {
            let replacement = self.try_replace_bus_interaction(bus_int);
            let Some(bus_id) = bus_int.bus_id.try_to_number() else {
                return true;
            };
            if self.bus_interaction_handler.is_stateful(bus_id) {
                return true;
            }
            if let Some(replacement) = replacement {
                // TODO: Also add range constraints
                new_constraints.push(replacement.polynomial_constraint);
                return false;
            }
            true
        });

        // TODO: Need to mutate solver too?
        system.algebraic_constraints.extend(new_constraints);
        system
    }

    fn try_replace_bus_interaction(
        &self,
        bus_interaction: &BusInteraction<GroupedExpression<T, V>>,
    ) -> Option<Replacement<T, V>> {
        for input_output_pair in self.possible_input_output_pairs(bus_interaction) {
            // - Enumerate all possible input assignments
            // - Sample 2^{num_inputs} inputs, interpolate multilinear polynomial
            // - Check all inputs: If the input assignment is set, is the output:
            //   - Unique (i.e., a concrete number)?
            //   - Equal to the multilinear polynomial?
            // - If yes, return the replacement:
            //   - Polynomial constraint: output = polynomial(inputs)
            //   - Range constraints for all inputs and outputs

            let mut all_possible_assignments =
                self.all_possible_assignments(bus_interaction, &input_output_pair);
            let Ok(hypothesis) = self.interpolate_polynomial(
                input_output_pair.inputs.len(),
                &mut all_possible_assignments,
            ) else {
                continue;
            };

            // Test the hypothesis on all remaining assignments.
            let mut has_error = false;
            for assignment in all_possible_assignments {
                let Ok((inputs, output)) = assignment else {
                    has_error = true;
                    break;
                };
                let mut hypothesis_evaluation = hypothesis.clone();
                for (i, input) in inputs.iter().enumerate() {
                    hypothesis_evaluation.substitute_by_known(&i, input);
                }
                if hypothesis_evaluation.try_to_number().unwrap() != output {
                    // The hypothesis does not hold for this assignment.
                    has_error = true;
                    break;
                }
            }
            if has_error {
                // The hypothesis does not hold for all assignments.
                continue;
            }

            // If we got this far, the hypothesis is correct!
            #[derive(Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
            enum Var<V> {
                Input(usize),
                Var(V),
            }
            let mut multilinear_extension =
                hypothesis.transform_var_type(&mut |input_index| Var::Input(*input_index));

            for (i, input) in input_output_pair.inputs.iter().enumerate() {
                let input_expression = input
                    .expression
                    .transform_var_type(&mut |var| Var::Var(var.clone()));
                multilinear_extension.substitute_by_unknown(&Var::Input(i), &input_expression);
            }
            let multilinear_extension =
                multilinear_extension.transform_var_type(&mut |var| match var {
                    Var::Input(..) => unreachable!(),
                    Var::Var(v) => v.clone(),
                });
            let polynomial_constraint =
                input_output_pair.output.expression.clone() - multilinear_extension;
            // TODO: Check degree
            return Some(Replacement {
                polynomial_constraint,
                // TODO
                _range_constraints: [].into_iter().collect(),
            });
        }

        // No multilinear extension found.
        None
    }

    #[auto_enum(Iterator)]
    fn possible_input_output_pairs(
        &self,
        bus_interaction: &'a BusInteraction<GroupedExpression<T, V>>,
    ) -> impl Iterator<Item = InputOutputPair<T, V>> + 'a {
        let unknown_fields = bus_interaction
            .payload
            .iter()
            .enumerate()
            .filter(|(_i, expr)| expr.try_to_number().is_none())
            .map(|(i, _)| i)
            .collect::<Vec<_>>();

        if unknown_fields.len() <= 1 || unknown_fields.len() > MAX_INPUTS + 1 {
            std::iter::empty()
        } else {
            unknown_fields
                .clone()
                .into_iter()
                .combinations(unknown_fields.len() - 1)
                // Map to list of (index, range constraint) pairs
                .map(|inputs| {
                    inputs
                        .into_iter()
                        .map(|i| {
                            let expr = &bus_interaction.payload[i];
                            let rc = self.solver.range_constraint_for_expression(expr);
                            (i, rc)
                        })
                        .collect::<Vec<_>>()
                })
                // Discard if the domain would be too large
                .filter(|inputs_with_rc| {
                    inputs_with_rc
                        .iter()
                        .map(|(_, rc)| {
                            // TODO: range_width() is too loose, because it ignores the mask!
                            rc.range_width().try_into_u64().and_then(|width| {
                                if width < 1 << 16 {
                                    Some(rc.allowed_values().count() as u64)
                                } else {
                                    None
                                }
                            })
                        })
                        .try_fold(1u64, |acc, x| acc.checked_mul(x?))
                        .is_some_and(|count| count <= MAX_DOMAIN_SIZE)
                })
                // Build the input-output pairs
                .map(move |inputs_with_rc| {
                    let input_indices = inputs_with_rc
                        .iter()
                        .map(|(i, _)| *i)
                        .collect::<BTreeSet<_>>();
                    InputOutputPair {
                        inputs: inputs_with_rc
                            .into_iter()
                            .map(|(i, rc)| Input {
                                index: i,
                                expression: bus_interaction.payload[i].clone(),
                                range_constraint: rc,
                            })
                            .collect(),
                        output: unknown_fields
                            .clone()
                            .into_iter()
                            .filter(|i| !input_indices.contains(i))
                            .map(|i| Output {
                                index: i,
                                expression: bus_interaction.payload[i].clone(),
                            })
                            .exactly_one()
                            .unwrap_or_else(|_| panic!()),
                    }
                })
        }
    }

    fn all_possible_assignments<'b>(
        &'b self,
        bus_interaction: &BusInteraction<GroupedExpression<T, V>>,
        input_output_pair: &'b InputOutputPair<T, V>,
    ) -> impl Iterator<Item = Result<(Vec<T>, T), ()>> + 'b {
        let bus_interaction = bus_interaction.to_range_constraints(&self.solver);
        let num_inputs = input_output_pair.inputs.len();
        let assignments_per_variable = input_output_pair
            .inputs
            .iter()
            .map(move |input| {
                (
                    input.index,
                    input.range_constraint.allowed_values().collect_vec(),
                )
            })
            .collect_vec();

        let mut rectangle = Vec::new();
        for i in 0..(1 << num_inputs) {
            let mut assignment = Vec::new();
            for (j, (index, variable_assignments)) in assignments_per_variable.iter().enumerate() {
                let value = if i & (1 << (num_inputs - 1 - j)) != 0 {
                    variable_assignments[1]
                } else {
                    variable_assignments[0]
                };
                assignment.push((*index, value));
            }
            rectangle.push(assignment);
        }

        let all_assignments = assignments_per_variable
            .into_iter()
            .map(|(index, values)| values.into_iter().map(move |value| (index, value)))
            .multi_cartesian_product();

        rectangle
            .into_iter()
            .chain(all_assignments)
            .map(move |assignment| {
                let mut bus_interaction = bus_interaction.clone();
                for (i, value) in assignment.iter() {
                    bus_interaction.payload[*i] = RangeConstraint::from_value(*value);
                }
                let Ok(bus_interaction) = self
                    .bus_interaction_handler
                    .handle_bus_interaction_checked(bus_interaction)
                else {
                    // The inputs violate the constraints.
                    return Err(());
                };
                let Some(output) = bus_interaction
                    .payload
                    .get(input_output_pair.output.index)
                    .and_then(|expr| expr.try_to_single_value())
                else {
                    // The output is not uniquely determined by the inputs.
                    return Err(());
                };
                let inputs = assignment
                    .into_iter()
                    .map(|(_i, value)| value)
                    .collect::<Vec<_>>();
                Ok((inputs, output))
            })
    }

    fn interpolate_polynomial(
        &self,
        num_inputs: usize,
        assignments: &mut impl Iterator<Item = Result<(Vec<T>, T), ()>>,
    ) -> Result<GroupedExpression<T, usize>, ()> {
        let assignments = assignments
            .take(1 << num_inputs)
            .collect::<Result<Vec<_>, ()>>()?;
        let mut rectangle_coordinates = vec![BTreeSet::new(); num_inputs];
        for (inputs, _output) in assignments.iter() {
            for (i, input) in inputs.iter().enumerate() {
                rectangle_coordinates[i].insert(*input);
            }
        }

        let rectangle_coordinates = rectangle_coordinates
            .into_iter()
            .map(|set| set.into_iter().collect_vec())
            .collect_vec();
        for (i, assignment) in assignments.iter().enumerate() {
            for (j, assignment) in assignment.0.iter().enumerate() {
                let expected_value_index = if i & (1 << (num_inputs - 1 - j)) != 0 {
                    1
                } else {
                    0
                };
                let expected_value = rectangle_coordinates[j][expected_value_index];
                assert_eq!(
                    *assignment, expected_value,
                    "Expected value for input {j} at assignment {i} to be {expected_value}, but got {assignment}."
                );
            }
        }

        let var: fn(usize) -> GroupedExpression<T, usize> =
            |var| GroupedExpression::from_unknown_variable(var);
        let constant: fn(T) -> GroupedExpression<T, usize> =
            |value| GroupedExpression::from_number(value);

        let lagrange_polynomials = match num_inputs {
            1 => {
                // Simplification of the num_inputs == 2 case.
                let x = var(0);
                let x0 = rectangle_coordinates[0][0];
                let x1 = rectangle_coordinates[0][1];

                let lx0 = (x.clone() - constant(x1)) * constant(T::one() / (x0 - x1));
                let lx1 = (x.clone() - constant(x0)) * constant(T::one() / (x1 - x0));

                vec![lx0, lx1]
            }
            2 => {
                let x = var(0);
                let y = var(1);
                let x0 = rectangle_coordinates[0][0];
                let x1 = rectangle_coordinates[0][1];
                let y0 = rectangle_coordinates[1][0];
                let y1 = rectangle_coordinates[1][1];

                // According to ChatGPT:
                // let lx0 = |x: F| (x - x1) / (x0 - x1);
                // let lx1 = |x: F| (x - x0) / (x1 - x0);
                // let ly0 = |y: F| (y - y1) / (y0 - y1);
                // let ly1 = |y: F| (y - y0) / (y1 - y0);
                // p(x,y) = z00*lx0(x)*ly0(y) + z10*lx1(x)*ly0(y)
                //     + z01*lx0(x)*ly1(y) + z11*lx1(x)*ly1(y);

                let lx0 = (x.clone() - constant(x1)) * constant(T::one() / (x0 - x1));
                let lx1 = (x.clone() - constant(x0)) * constant(T::one() / (x1 - x0));
                let ly0 = (y.clone() - constant(y1)) * constant(T::one() / (y0 - y1));
                let ly1 = (y.clone() - constant(y0)) * constant(T::one() / (y1 - y0));

                vec![
                    lx0.clone() * ly0.clone(), // (x0,y0)  -> z00
                    lx0.clone() * ly1.clone(), // (x0,y1)  -> z01
                    lx1.clone() * ly0,         // (x1,y0)  -> z10
                    lx1 * ly1,                 // (x1,y1)  -> z11
                ]
            }
            _ => panic!("Unexpected number of inputs: {num_inputs}"),
        };

        Ok(lagrange_polynomials
            .into_iter()
            .zip_eq(assignments)
            .map(|(lagrange_polynomial, (_inputs, output))| {
                lagrange_polynomial * GroupedExpression::from_number(output)
            })
            .fold(GroupedExpression::zero(), |acc, expr| acc + expr))
    }
}

const MAX_INPUTS: usize = 2;
const MAX_DOMAIN_SIZE: u64 = 256;

#[derive(Clone, Debug)]
struct Replacement<T: FieldElement, V> {
    polynomial_constraint: GroupedExpression<T, V>,
    _range_constraints: BTreeMap<V, RangeConstraint<T>>,
}

struct Input<T: FieldElement, V> {
    index: usize,
    expression: GroupedExpression<T, V>,
    range_constraint: RangeConstraint<T>,
}

struct Output<T: FieldElement, V> {
    index: usize,
    expression: GroupedExpression<T, V>,
}

struct InputOutputPair<T: FieldElement, V> {
    inputs: Vec<Input<T, V>>,
    output: Output<T, V>,
}

#[cfg(test)]
mod tests {

    use powdr_constraint_solver::solver::new_solver;
    use powdr_number::BabyBearField;

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
            match (
                bus_interaction.payload[0].try_to_single_value(),
                bus_interaction.payload[1].try_to_single_value(),
            ) {
                (Some(x), Some(y)) => {
                    let z = BabyBearField::from(x.to_degree() ^ y.to_degree());
                    BusInteraction {
                        bus_id: bus_interaction.bus_id,
                        payload: vec![
                            RangeConstraint::from_value(BabyBearField::from(x)),
                            RangeConstraint::from_value(BabyBearField::from(y)),
                            RangeConstraint::from_value(BabyBearField::from(z)),
                        ],
                        multiplicity: bus_interaction.multiplicity,
                    }
                }
                _ => bus_interaction, // If not both inputs are known, return unchanged
            }
        }
    }
    impl IsBusStateful<BabyBearField> for XorBusHandler {
        fn is_stateful(&self, _bus_id: BabyBearField) -> bool {
            false
        }
    }

    fn compute_replacement(
        mut solver: impl Solver<BabyBearField, Var>,
        bus_interaction: &BusInteraction<GroupedExpression<BabyBearField, Var>>,
    ) -> Option<Replacement<BabyBearField, Var>> {
        let optimizer = LinearBusInteractionOptimizer {
            solver: &mut solver,
            bus_interaction_handler: XorBusHandler,
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
        assert_eq!(replacement.polynomial_constraint.to_string(), "x + z - 255");
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
        assert_eq!(
            replacement.polynomial_constraint.to_string(),
            "(x - 1) * (y) + (x) * (y - 1) + z"
        );
    }

    #[test]
    fn test_try_replace_bus_interaction_distinct_masks() {
        let mut solver = new_solver(ConstraintSystem::default(), XorBusHandler);
        // Because the masks are distinct, there is a multilinear extension: z = x + y.
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
        // This looks complicated, but it is correct:
        // -((16 * x - 16) * (125829120 * y) + (x) * (125829120 * y + 1) - (17 * x) * (125829120 * y) - z)
        // = -(16 * 125829120 * x * y - 16 * 125829120 * y + 125829120 * x * y + x - 17 * 125829120 * x * y - z)
        // = -(-16 * 125829120 * y + x - z)
        // = -(y + x - z)
        assert_eq!(replacement.polynomial_constraint.to_string(), "-((16 * x - 16) * (125829120 * y) + (x) * (125829120 * y + 1) - (17 * x) * (125829120 * y) - z)");
    }
}
