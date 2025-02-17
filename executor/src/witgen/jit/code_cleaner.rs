use std::collections::HashSet;

use powdr_number::FieldElement;

use super::{effect::Effect, variable::Variable};

/// Returns the list of variables that are not needed to compute the requested
/// variables.
pub fn optional_vars<T: FieldElement>(
    code: &[Effect<T, Variable>],
    requested_variables: &[Variable],
) -> HashSet<Variable> {
    let mut required: HashSet<_> = requested_variables.iter().cloned().collect();
    let mut optional: HashSet<_> = Default::default();
    for effect in code.iter().rev() {
        optional.extend(optional_vars_in_effect(effect, &mut required));
    }
    optional
}

/// Removes the given variables from the code and all variables that depend on them.
pub fn remove_variables<T: FieldElement>(
    code: Vec<Effect<T, Variable>>,
    mut to_remove: HashSet<Variable>,
) -> Vec<Effect<T, Variable>> {
    code.into_iter()
        .filter_map(|effect| remove_variables_from_effect(effect, &mut to_remove))
        .collect()
}

/// Returns the variables in the effect that are not needed
/// to compute the requested variables and also updates the requested
/// variables.
/// This is intended to be used in reverse on a list of effects.
fn optional_vars_in_effect<T: FieldElement>(
    effect: &Effect<T, Variable>,
    requested_variables: &mut HashSet<Variable>,
) -> HashSet<Variable> {
    let needed = match &effect {
        Effect::Assignment(..) | Effect::ProverFunctionCall(..) => effect
            .written_vars()
            .any(|(v, _)| requested_variables.contains(v)),
        Effect::Assertion(_) => {
            // If none of the variables in the assertion are required, we can
            // remove the assertion.
            !effect
                .referenced_variables()
                .any(|v| requested_variables.contains(v))
        }
        Effect::MachineCall(..) => {
            // We always require machine calls.
            true
        }
        Effect::Branch(condition, left, right) => {
            let mut requested_left = requested_variables.clone();
            let optional_left = optional_vars_in_branch(left, &mut requested_left);
            let mut requested_right = requested_variables.clone();
            let optional_right = optional_vars_in_branch(right, &mut requested_right);
            requested_variables
                .extend(requested_left.iter().chain(requested_right.iter()).cloned());
            requested_variables.insert(condition.variable.clone());
            return optional_left
                .intersection(&optional_right)
                .cloned()
                .collect();
        }
        Effect::RangeConstraint(..) => unreachable!(),
    };
    if needed {
        requested_variables.extend(effect.referenced_variables().cloned());
        HashSet::new()
    } else {
        effect.written_vars().map(|(v, _)| v).cloned().collect()
    }
}

fn optional_vars_in_branch<T: FieldElement>(
    branch: &[Effect<T, Variable>],
    requested_variables: &mut HashSet<Variable>,
) -> HashSet<Variable> {
    let mut optional = HashSet::new();
    for effect in branch.iter().rev() {
        optional.extend(optional_vars_in_effect(effect, requested_variables));
    }
    optional
}

fn remove_variables_from_effect<T: FieldElement>(
    effect: Effect<T, Variable>,
    to_remove: &mut HashSet<Variable>,
) -> Option<Effect<T, Variable>> {
    if effect.referenced_variables().any(|v| to_remove.contains(v)) {
        to_remove.extend(effect.written_vars().map(|(v, _)| v).cloned());
        None
    } else {
        Some(effect)
    }
}
