use std::collections::BTreeMap;
use std::fmt::Display;
use std::hash::Hash;

use itertools::Itertools;
use num_traits::One;
use powdr_constraint_solver::constraint_system::{
    BusInteraction, BusInteractionHandler, ConstraintSystem,
};
use powdr_constraint_solver::grouped_expression::GroupedExpression;
use powdr_constraint_solver::inliner::DegreeBound;
use powdr_constraint_solver::range_constraint::RangeConstraint;
use powdr_constraint_solver::solver::{new_solver, Solver};
use powdr_number::FieldElement;

pub type RangeConstraints<T, V> = Vec<(GroupedExpression<T, V>, RangeConstraint<T>)>;

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
    /// Note that only range constraints returned from `pure_range_constraints`
    /// are passed here, so the implementation should always be able to construct
    /// a valid bus interaction from them.
    fn batch_make_range_constraints<V: Ord + Clone + Eq + Display + Hash>(
        &self,
        range_constraints: RangeConstraints<T, V>,
    ) -> Vec<BusInteraction<GroupedExpression<T, V>>>;
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
                    *existing_rc = existing_rc.conjunction(&rc);
                }
                false
            }
            None => true,
        }
    });

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
            if rc == &RangeConstraint::from_mask(1) && expr.degree() < degree_bound.identities {
                bit_constraints.push(expr.clone() * (expr.clone() - GroupedExpression::one()));
                false
            } else {
                true
            }
        })
        .collect();

    // Create all range constraints in batch and add them to the system.
    let range_constraints = bus_interaction_handler.batch_make_range_constraints(to_constrain);
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
