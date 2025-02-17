use std::collections::{BTreeSet, HashSet};

use itertools::Itertools;
use powdr_number::FieldElement;

use crate::witgen::jit::effect::format_code;

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

pub fn remove_variables<T: FieldElement>(
    code: Vec<Effect<T, Variable>>,
    mut variables: HashSet<Variable>,
) -> Vec<Effect<T, Variable>> {
    code.into_iter()
        .filter_map(|effect| remove_variables_from_effect(effect, &mut variables))
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
        Effect::Assignment(written, _) => requested_variables.contains(written),
        Effect::Assertion(_) | Effect::MachineCall(..) | Effect::ProverFunctionCall(_) => {
            // We could do another pass and remove assertions that only contain
            //
            // variables that we have not computed, but we just mark
            // all variables as required for now.
            // For the others, we always mark them as required for now.
            true
        }
        Effect::Branch(condition, left, right) => {
            requested_variables.insert(condition.variable.clone());
            // TODO do we need to create two indpependent copies of requested vars
            return optional_vars_in_branch(left, requested_variables)
                .union(&optional_vars_in_branch(right, requested_variables))
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
    variables: &mut HashSet<Variable>,
) -> Option<Effect<T, Variable>> {
    let keep = match &effect {
        Effect::Assignment(written, expr) => !variables.contains(&written),
        Effect::Assertion(_) | Effect::MachineCall(..) | Effect::ProverFunctionCall(_) => true,
        Effect::Branch(condition, left, right) => {
            todo!()
        }
        Effect::RangeConstraint(..) => unreachable!(),
    };
    if keep {
        variables.extend(effect.referenced_variables().cloned());
        Some(effect)
    } else {
        None
    }
}
