use std::collections::BTreeMap;
use std::fmt::Display;
use std::hash::Hash;

use itertools::Itertools;
use powdr_constraint_solver::constraint_system::{
    AlgebraicConstraint, BusInteraction, BusInteractionHandler, ConstraintSystem,
};
use powdr_constraint_solver::grouped_expression::GroupedExpression;
use powdr_constraint_solver::inliner::DegreeBound;
use powdr_constraint_solver::range_constraint::RangeConstraint;
use powdr_constraint_solver::solver::{new_solver, Solver};
use powdr_number::FieldElement;

pub type RangeConstraints<T, V> = Vec<(GroupedExpression<T, V>, RangeConstraint<T>)>;

/// The requested range constraint cannot be implemented.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MakeRangeConstraintsError(pub String);

pub trait RangeConstraintHandler<T: FieldElement> {
    /// If the bus interaction *only* enforces range constraints, returns them
    /// as a map of expressions to range constraints.
    ///
    /// For example:
    /// - If a bus interaction takes two arguments `a` and `b` and enforces the
    ///   range constraints `0 <= a < 2^b`, it is *not* a pure range constraint if
    ///   both values are unknown (because the allowed values of `a` depend on `b`)
    /// - On the other hand, if `b` is known, it is a pure range constraint.
    ///
    /// Any stateful bus interaction is not a pure range constraint.
    /// This function will only be called with bus interactions with multiplicity 1.
    fn pure_range_constraints<V: Ord + Clone + Eq + Display + Hash>(
        &self,
        bus_interaction: &BusInteraction<GroupedExpression<T, V>>,
    ) -> Option<RangeConstraints<T, V>>;

    /// Given a set of range constraints, returns a list of bus interactions
    /// that implements them. The implementation is free to implement multiple
    /// range constraints using a single bus interaction.
    /// As all input range constraints are unconditional, the multiplicity of
    /// the returned bus interactions should be 1.
    /// If one of the range constraints cannot be implemented exactly, an error
    /// is returned. For soundness, the implementation should *never* relax the
    /// range constraint.
    fn batch_make_range_constraints<V: Ord + Clone + Eq + Display + Hash>(
        &self,
        range_constraints: RangeConstraints<T, V>,
    ) -> Result<Vec<BusInteraction<GroupedExpression<T, V>>>, MakeRangeConstraintsError>;
}

/// Optimizes range constraints, minimizing the number of bus interactions.
///
/// This step:
/// - removes range constraints that are already implied by existing constraints
/// - batches several range constraints into one bus interaction, if possible
/// - implements bit constraints via polynomial constraints, if the degree bound allows
pub fn optimize_range_constraints<T: FieldElement, V: Ord + Clone + Hash + Eq + Display>(
    mut system: ConstraintSystem<T, V>,
    bus_interaction_handler: impl BusInteractionHandler<T> + RangeConstraintHandler<T> + Clone,
    degree_bound: DegreeBound,
) -> ConstraintSystem<T, V> {
    // Remove all pure range constraints, but collect what was removed.
    // We store the expressions to constrain in a vector, so that we can keep the order of
    // the range constraints as much as possible.
    let mut to_constrain = Vec::new();
    let mut range_constraints = BTreeMap::new();
    // Tracks whether two unconditional pure range constraints for the same expression turn out
    // to be mutually exclusive, which makes the whole system unsatisfiable (handled below).
    let mut has_contradiction = false;
    system.bus_interactions.retain(|bus_int| {
        if bus_int.multiplicity != GroupedExpression::from_number(T::one()) {
            // Most range constraints are unconditional in practice, it's probably not
            // worth dealing with the conditional ones.
            return true;
        }

        match bus_interaction_handler.pure_range_constraints(bus_int) {
            Some(new_range_constraints) => {
                to_constrain.extend(new_range_constraints.iter().map(|(expr, _)| expr.clone()));
                for (expr, rc) in new_range_constraints {
                    let existing_rc = range_constraints
                        .entry(expr)
                        .or_insert_with(RangeConstraint::default);
                    if existing_rc.is_disjoint(&rc) {
                        has_contradiction = true;
                    }
                    *existing_rc = existing_rc.conjunction(&rc);
                }
                false
            }
            None => true,
        }
    });

    if has_contradiction {
        // Two unconditional pure range constraints for the same expression are mutually
        // exclusive, so the original system is unsatisfiable. `RangeConstraint::conjunction`
        // cannot represent an empty range and would relax such a contradiction into a
        // satisfiable constraint (e.g. {1} and {2} merge to {0}), so we instead record the
        // contradiction explicitly to keep the system unsatisfiable.
        system
            .algebraic_constraints
            .push(AlgebraicConstraint::assert_zero(
                GroupedExpression::from_number(T::one()),
            ));
        return system;
    }

    // Filter range constraints that are already implied by existing constraints.
    // TODO: They could also be implied by each other.
    let mut solver = new_solver(system.clone(), bus_interaction_handler.clone());
    solver.solve().unwrap();
    let to_constrain = to_constrain
        .into_iter()
        .unique()
        .map(|expr| {
            let rc = range_constraints.remove(&expr).unwrap();
            (expr, rc)
        })
        .filter(|(expr, rc)| {
            let current_rc = solver.range_constraint_for_expression(expr);
            current_rc != current_rc.conjunction(rc)
        })
        .collect::<Vec<_>>();

    // Implement bit constraints via polynomial constraints, if the degree bound allows.
    let mut bit_constraints = Vec::new();
    let to_constrain = to_constrain
        .into_iter()
        .filter(|(expr, rc)| {
            let bit_range_constraint = AlgebraicConstraint::assert_bool(expr.clone());
            if rc == &RangeConstraint::from_mask(1)
                && bit_range_constraint.degree() <= degree_bound.identities
            {
                bit_constraints.push(bit_range_constraint);
                false
            } else {
                true
            }
        })
        .collect();

    // Create all range constraints in batch and add them to the system.
    // Note that unwrapping here should be fine, because we only pass range constraints
    // that were returned from `pure_range_constraints`, so clearly the VM is able to
    // implement them.
    let range_constraints = bus_interaction_handler
        .batch_make_range_constraints(to_constrain)
        .unwrap();
    for bus_interaction in &range_constraints {
        assert_eq!(bus_interaction.multiplicity.try_to_number(), Some(T::one()));
    }
    system.bus_interactions.extend(range_constraints);
    system.algebraic_constraints.extend(bit_constraints);

    system
}

/// Utility functions useful for implementing `batch_make_range_constraints`.
pub mod utils {
    use itertools::Itertools;
    use powdr_constraint_solver::{
        grouped_expression::GroupedExpression, range_constraint::RangeConstraint,
    };
    use powdr_number::FieldElement;
    use std::fmt::Display;
    use std::hash::Hash;

    use crate::range_constraint_optimizer::RangeConstraints;

    /// If the range constraints is the range 0..(2^bits - 1), returns Some(bits).
    pub fn range_constraint_to_num_bits<T: FieldElement>(
        range_constraint: &RangeConstraint<T>,
    ) -> Option<usize> {
        (0..30).find(|num_bits| {
            let mask = (1u64 << num_bits) - 1;
            range_constraint == &RangeConstraint::from_mask(mask)
        })
    }

    /// Given a set of range constraints, filters out the byte constraints and returns them.
    pub fn filter_byte_constraints<T: FieldElement, V: Ord + Clone + Eq + Display + Hash>(
        range_constraints: &mut RangeConstraints<T, V>,
    ) -> Vec<GroupedExpression<T, V>> {
        let mut byte_constraints = Vec::new();
        range_constraints.retain(|(expr, rc)| match range_constraint_to_num_bits(rc) {
            Some(8) => {
                byte_constraints.push(expr.clone());
                false
            }
            _ => true,
        });
        byte_constraints.into_iter().unique().collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use num_traits::One;
    use powdr_constraint_solver::inliner::DegreeBound;
    use powdr_number::GoldilocksField;

    type T = GoldilocksField;
    type Var = &'static str;

    /// Test handler that treats each bus interaction as the pure range constraint
    /// `payload[0] == payload[1]` (a single value).
    #[derive(Clone)]
    struct SingleValueHandler;

    impl BusInteractionHandler<T> for SingleValueHandler {
        fn handle_bus_interaction(
            &self,
            bus_interaction: BusInteraction<RangeConstraint<T>>,
        ) -> BusInteraction<RangeConstraint<T>> {
            bus_interaction
        }
    }

    impl RangeConstraintHandler<T> for SingleValueHandler {
        fn pure_range_constraints<V: Ord + Clone + Eq + Display + Hash>(
            &self,
            bus_interaction: &BusInteraction<GroupedExpression<T, V>>,
        ) -> Option<RangeConstraints<T, V>> {
            let expr = bus_interaction.payload.first()?.clone();
            let value = bus_interaction.payload.get(1)?.try_to_number()?;
            Some(vec![(expr, RangeConstraint::from_value(value))])
        }

        fn batch_make_range_constraints<V: Ord + Clone + Eq + Display + Hash>(
            &self,
            range_constraints: RangeConstraints<T, V>,
        ) -> Result<Vec<BusInteraction<GroupedExpression<T, V>>>, MakeRangeConstraintsError>
        {
            Ok(range_constraints
                .into_iter()
                .map(|(expr, _)| BusInteraction {
                    bus_id: GroupedExpression::from_number(T::from(0u64)),
                    multiplicity: GroupedExpression::from_number(T::one()),
                    payload: vec![expr],
                })
                .collect())
        }
    }

    fn single_value_bus_interaction(
        var: Var,
        value: u64,
    ) -> BusInteraction<GroupedExpression<T, Var>> {
        BusInteraction {
            bus_id: GroupedExpression::from_number(T::from(0u64)),
            multiplicity: GroupedExpression::from_number(T::one()),
            payload: vec![
                GroupedExpression::from_unknown_variable(var),
                GroupedExpression::from_number(T::from(value)),
            ],
        }
    }

    fn always_false() -> AlgebraicConstraint<GroupedExpression<T, Var>> {
        AlgebraicConstraint::assert_zero(GroupedExpression::from_number(T::one()))
    }

    fn run(system: ConstraintSystem<T, Var>) -> ConstraintSystem<T, Var> {
        optimize_range_constraints(
            system,
            SingleValueHandler,
            DegreeBound {
                identities: 100,
                bus_interactions: 100,
            },
        )
    }

    /// Two unconditional pure range constraints force `x` to be both 1 and 2, so the system is
    /// unsatisfiable. The optimizer must not relax this into a satisfiable `x = 0`.
    #[test]
    fn contradictory_pure_range_constraints_stay_unsatisfiable() {
        let system = ConstraintSystem::<T, Var> {
            bus_interactions: vec![
                single_value_bus_interaction("x", 1),
                single_value_bus_interaction("x", 2),
            ],
            ..Default::default()
        };
        let optimized = run(system);
        assert!(
            optimized.algebraic_constraints.contains(&always_false()),
            "contradiction must be preserved as an unsatisfiable constraint, not relaxed"
        );
    }

    /// Two compatible constraints (`x == 1` twice) must not be flagged as a contradiction.
    #[test]
    fn compatible_pure_range_constraints_are_not_flagged() {
        let system = ConstraintSystem::<T, Var> {
            bus_interactions: vec![
                single_value_bus_interaction("x", 1),
                single_value_bus_interaction("x", 1),
            ],
            ..Default::default()
        };
        let optimized = run(system);
        assert!(
            !optimized.algebraic_constraints.contains(&always_false()),
            "compatible constraints must not be treated as a contradiction"
        );
    }
}
