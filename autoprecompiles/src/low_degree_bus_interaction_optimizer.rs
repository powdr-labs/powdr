use auto_enums::auto_enum;
use itertools::Itertools;
use powdr_constraint_solver::constraint_system::{
    BusInteraction, BusInteractionHandler, ConstraintSystem,
};
use powdr_constraint_solver::grouped_expression::GroupedExpression;
use powdr_constraint_solver::inliner::DegreeBound;
use powdr_constraint_solver::range_constraint::RangeConstraint;
use powdr_constraint_solver::runtime_constant::RuntimeConstant;
use powdr_constraint_solver::solver::Solver;
use powdr_number::FieldElement;
use powdr_number::LargeInt;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Display;
use std::hash::Hash;
use std::marker::PhantomData;

use crate::constraint_optimizer::IsBusStateful;
use crate::range_constraint_optimizer::RangeConstraintHandler;

pub struct LowDegreeBusInteractionOptimizer<
    'a,
    T: FieldElement,
    V: Ord + Clone + Eq,
    S: Solver<T, V>,
    B: BusInteractionHandler<T> + IsBusStateful<T> + RangeConstraintHandler<T>,
> {
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
                // Keep the bus interaction as is if a replacement can't be found.
                let Some(bus_id) = bus_int.bus_id.try_to_number() else {
                    return vec![bus_int];
                };
                if self.bus_interaction_handler.is_stateful(bus_id) {
                    return vec![bus_int];
                }
                let Some(replacement) = self.try_replace_bus_interaction(&bus_int) else {
                    return vec![bus_int];
                };

                // If we found a replacement, add the polynomial constraints and replace
                // the bus interaction with interactions implementing the range constraints.
                // Note that many of these may be optimized away by the range constraint optimizer.
                new_constraints.push(replacement.polynomial_constraint);
                let range_constraints = replacement
                    .range_constraints
                    .into_iter()
                    .map(|(variable, rc)| (GroupedExpression::from_unknown_variable(variable), rc))
                    .collect();
                self.bus_interaction_handler
                    .batch_make_range_constraints(range_constraints)
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

    fn try_replace_bus_interaction(
        &self,
        bus_interaction: &BusInteraction<GroupedExpression<T, V>>,
    ) -> Option<Replacement<T, V>> {
        for input_output_pair in self.possible_input_output_pairs(bus_interaction) {
            if let Some(low_degree_function) =
                self.find_low_degree_function(bus_interaction, &input_output_pair)
            {
                // Build polynomial constraint
                let symbolic_inputs = input_output_pair
                    .inputs
                    .into_iter()
                    .map(|input| input.expression)
                    .collect();
                let low_degree_function = low_degree_function(symbolic_inputs);
                let polynomial_constraint =
                    input_output_pair.output.expression.clone() - low_degree_function;

                // Check degree
                if polynomial_constraint.degree() > self.degree_bound.identities {
                    continue;
                }

                return Some(Replacement {
                    polynomial_constraint,
                    // TODO
                    range_constraints: [].into_iter().collect(),
                });
            }
        }

        // No low-degree function found.
        None
    }

    #[auto_enum(Iterator)]
    fn possible_input_output_pairs(
        &'a self,
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

    fn find_low_degree_function(
        &self,
        bus_interaction: &BusInteraction<GroupedExpression<T, V>>,
        input_output_pair: &InputOutputPair<T, V>,
    ) -> Option<LowDegreeFunction<T, V>> {
        let all_possible_assignments =
            self.all_possible_assignments(bus_interaction, input_output_pair);
        let mut hypotheses = hypotheses(input_output_pair.inputs.len());

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

    fn all_possible_assignments<'b>(
        &'b self,
        bus_interaction: &BusInteraction<GroupedExpression<T, V>>,
        input_output_pair: &'b InputOutputPair<T, V>,
    ) -> impl Iterator<Item = Result<(Vec<T>, T), ()>> + 'b {
        let bus_interaction = bus_interaction.to_range_constraints(self.solver);
        input_output_pair
            .inputs
            .iter()
            // Map to (index, range constraint) pairs
            .map(move |input| {
                (
                    input.index,
                    input.range_constraint.allowed_values().collect_vec(),
                )
            })
            .map(|(index, values)| values.into_iter().map(move |value| (index, value)))
            .multi_cartesian_product()
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
}

type LowDegreeFunction<T, V> = Box<dyn Fn(Vec<GroupedExpression<T, V>>) -> GroupedExpression<T, V>>;

const MAX_INPUTS: usize = 2;
const MAX_DOMAIN_SIZE: u64 = 256;

#[derive(Clone, Debug)]
struct Replacement<T: FieldElement, V> {
    polynomial_constraint: GroupedExpression<T, V>,
    range_constraints: BTreeMap<V, RangeConstraint<T>>,
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

/// Some well-known multilinear functions that are tested against the input-output pairs.
fn hypotheses<T: FieldElement, V: Ord + Clone + Hash + Eq>(
    num_inputs: usize,
) -> Vec<LowDegreeFunction<T, V>> {
    match num_inputs {
        1 => vec![
            // Identity function
            Box::new(|inputs| inputs[0].clone()),
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
                            RangeConstraint::from_value(x),
                            RangeConstraint::from_value(y),
                            RangeConstraint::from_value(z),
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
        let optimizer = LowDegreeBusInteractionOptimizer {
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
            "(2 * x) * (y) - x - y + z"
        );
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
        assert_eq!(
            replacement.polynomial_constraint.to_string(),
            "-(x + y - z)"
        );
    }
}
