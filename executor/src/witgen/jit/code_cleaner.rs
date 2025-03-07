use std::collections::HashSet;

use powdr_number::FieldElement;

use super::{
    effect::Effect,
    variable::{MachineCallVariable, Variable},
};

/// Returns the list of variables that are not needed to compute the required
/// variables.
pub fn optional_vars<T: FieldElement>(
    code: &[Effect<T, Variable<T>>],
    required: &[Variable<T>],
) -> HashSet<Variable<T>> {
    let mut required: HashSet<_> = required.iter().cloned().collect();
    let mut optional: HashSet<_> = Default::default();
    for effect in code.iter().rev() {
        optional.extend(optional_vars_in_effect(effect, &mut required));
    }
    optional
}

/// Removes the given variables from the code and all variables that depend on them.
pub fn remove_variables<T: FieldElement>(
    code: Vec<Effect<T, Variable<T>>>,
    mut to_remove: HashSet<Variable<T>>,
) -> Vec<Effect<T, Variable<T>>> {
    code.into_iter()
        .filter_map(|effect| remove_variables_from_effect(effect, &mut to_remove))
        .collect()
}

/// Removes all calls to machines with the given IDs on the given row offsets.
pub fn remove_machine_calls<T: FieldElement>(
    code: Vec<Effect<T, Variable<T>>>,
    to_remove: &HashSet<(u64, i32)>,
) -> Vec<Effect<T, Variable<T>>> {
    code.into_iter()
        .filter_map(|effect| remove_machine_calls_from_effect(effect, to_remove))
        .collect()
}

/// Returns the variables in the effect that are not needed
/// to compute the required variables and also updates the required
/// variables.
/// This is intended to be used in reverse on a list of effects.
fn optional_vars_in_effect<T: FieldElement>(
    effect: &Effect<T, Variable<T>>,
    required: &mut HashSet<Variable<T>>,
) -> HashSet<Variable<T>> {
    let needed = match &effect {
        Effect::Assignment(..) | Effect::ProverFunctionCall(..) => {
            effect.written_vars().any(|(v, _)| required.contains(v))
        }
        Effect::Assertion(_) => false,
        Effect::MachineCall(..) => {
            // We always require machine calls.
            true
        }
        Effect::Branch(condition, left, right) => {
            let mut required_left = required.clone();
            let optional_left = optional_vars_in_branch(left, &mut required_left);
            let mut required_right = required.clone();
            let optional_right = optional_vars_in_branch(right, &mut required_right);
            required.extend(required_left.iter().chain(required_right.iter()).cloned());
            required.insert(condition.variable.clone());
            return optional_left
                .intersection(&optional_right)
                .cloned()
                .collect();
        }
        Effect::RangeConstraint(..) => unreachable!(),
    };
    if needed {
        required.extend(effect.referenced_variables().cloned());
        HashSet::new()
    } else {
        effect.written_vars().map(|(v, _)| v).cloned().collect()
    }
}

fn optional_vars_in_branch<T: FieldElement>(
    branch: &[Effect<T, Variable<T>>],
    required: &mut HashSet<Variable<T>>,
) -> HashSet<Variable<T>> {
    branch
        .iter()
        .rev()
        .flat_map(|effect| optional_vars_in_effect(effect, required))
        .collect()
}

fn remove_variables_from_effect<T: FieldElement>(
    effect: Effect<T, Variable<T>>,
    to_remove: &mut HashSet<Variable<T>>,
) -> Option<Effect<T, Variable<T>>> {
    if let Effect::Branch(condition, left, right) = effect {
        let mut remove_left = to_remove.clone();
        let left = left
            .into_iter()
            .filter_map(|effect| remove_variables_from_effect(effect, &mut remove_left))
            .collect();
        let right = right
            .into_iter()
            .filter_map(|effect| remove_variables_from_effect(effect, to_remove))
            .collect();
        to_remove.extend(remove_left);
        Some(Effect::Branch(condition, left, right))
    } else if effect.referenced_variables().any(|v| to_remove.contains(v)) {
        to_remove.extend(effect.written_vars().map(|(v, _)| v).cloned());
        None
    } else {
        Some(effect)
    }
}

fn remove_machine_calls_from_effect<T: FieldElement>(
    effect: Effect<T, Variable<T>>,
    to_remove: &HashSet<(u64, i32)>,
) -> Option<Effect<T, Variable<T>>> {
    match effect {
        Effect::MachineCall(id, known, arguments) => {
            let Variable::MachineCallParam(MachineCallVariable {
                identity_id,
                row_offset,
                ..
            }) = &arguments[0]
            else {
                panic!()
            };
            assert_eq!(id, *identity_id);
            if to_remove.contains(&(id, *row_offset)) {
                None
            } else {
                Some(Effect::MachineCall(id, known, arguments))
            }
        }
        Effect::Branch(condition, first, second) => {
            let first = remove_machine_calls(first, to_remove);
            let second = remove_machine_calls(second, to_remove);
            Some(Effect::Branch(condition, first, second))
        }
        _ => Some(effect),
    }
}
