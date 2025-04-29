use std::collections::BTreeSet;
use std::fmt::{Debug, Display};
use std::hash::Hash;

use powdr_number::FieldElement;

use crate::quadratic_symbolic_expression::QuadraticSymbolicExpression;

/// Returns the set of all known variables in a list of algebraic expressions.
/// Panics if a variable appears as both known and unknown.
pub fn known_variables<'a, T: FieldElement, V: Clone + Hash + Ord + Eq + Debug + Display + 'a>(
    expressions: impl Iterator<Item = &'a QuadraticSymbolicExpression<T, V>>,
) -> BTreeSet<V> {
    let mut all_known_variables = BTreeSet::new();
    let mut all_unknown_variables = BTreeSet::new();

    for expression in expressions {
        let all_vars = expression
            .referenced_variables()
            .cloned()
            .collect::<BTreeSet<_>>();
        let unknown_variables = expression
            .referenced_unknown_variables()
            .cloned()
            .collect::<BTreeSet<_>>();
        let known_variables = all_vars.difference(&unknown_variables).cloned();

        all_known_variables.extend(known_variables);
        all_unknown_variables.extend(unknown_variables);
    }

    let inconsistent_variables = all_known_variables
        .intersection(&all_unknown_variables)
        .collect::<Vec<_>>();
    if !inconsistent_variables.is_empty() {
        panic!(
            "The following variables appear as both known and unknown: {inconsistent_variables:?}",
        );
    }

    all_known_variables
}
