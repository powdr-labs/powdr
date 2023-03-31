use std::collections::{HashMap, HashSet};

use num_bigint::BigInt;

use super::Machine;
use crate::analyzer::{Identity, IdentityKind, SelectedExpressions};
use crate::number::AbstractNumberType;

use crate::witness_generator::util::is_simple_poly;
use crate::witness_generator::{
    affine_expression::AffineExpression,
    eval_error::{self, EvalError},
    util::contains_witness_ref,
    EvalResult, FixedData,
};

/// Machine to perform a lookup in fixed columns only.
/// It only supports lookup in the first column of the query and will use the first match.
pub struct FixedLookup {}

impl FixedLookup {
    pub fn try_new(
        _fixed_data: &FixedData,
        identities: &[&Identity],
        witness_names: &HashSet<&str>,
    ) -> Option<Box<Self>> {
        if identities.is_empty() && witness_names.is_empty() {
            Some(Box::new(FixedLookup {}))
        } else {
            None
        }
    }
}

impl Machine for FixedLookup {
    fn process_plookup(
        &mut self,
        fixed_data: &FixedData,
        kind: IdentityKind,
        left: &[Result<AffineExpression, EvalError>],
        right: &SelectedExpressions,
    ) -> Option<EvalResult> {
        // This is a matching machine if it is a plookup and the RHS is fully constant.
        if kind != IdentityKind::Plookup
            || right.selector.is_some()
            || right
                .expressions
                .iter()
                .any(|e| contains_witness_ref(e, fixed_data))
        {
            return None;
        }

        // get the values of the fixed columns
        let right = right
            .expressions
            .iter()
            .map(|right_key| {
                is_simple_poly(right_key).and_then(|name| fixed_data.fixed_cols.get(name))
            })
            .collect::<Option<_>>()?;

        // If we already know the LHS, skip it.
        if left
            .iter()
            .all(|v| v.is_ok() && v.as_ref().unwrap().is_constant())
        {
            return Some(Ok(vec![]));
        }

        Some(self.process_plookup_internal(fixed_data, left, right))
    }

    fn witness_col_values(
        &mut self,
        _fixed_data: &FixedData,
    ) -> HashMap<String, Vec<AbstractNumberType>> {
        Default::default()
    }
}

impl FixedLookup {
    fn process_plookup_internal(
        &mut self,
        fixed_data: &FixedData,
        left: &[Result<AffineExpression, EvalError>],
        right: Vec<&&Vec<BigInt>>,
    ) -> EvalResult {
        let matches = (0..fixed_data.degree as usize)
            // get all lookup rows which match the lhs
            .filter_map(|row| {
                left.iter()
                    .zip(right.iter())
                    .map(|(left, right)| {
                        let right = &right[row];
                        match left {
                            Ok(left) => left
                                .constant_value()
                                // if the lhs is constant, it's a match iff the values match
                                .map(|left| (left == *right).then_some(right))
                                // if it's not constant, it's a match
                                .unwrap_or_else(|| Some(right)),
                            // if we do not know the lhs, it's a match
                            Err(_) => Some(right),
                        }
                    })
                    .collect::<Option<Vec<_>>>()
            });

        // fold the set of values for the individual columns into precise values and masks
        let right_values = matches
            .fold(None, |acc, new_value| {
                match acc {
                    // the first match gives a precise value for all columns
                    None => Some(new_value.into_iter().map(Result::Ok).collect::<Vec<_>>()),
                    // subsequent matches degrade the knowledge we have of each column
                    Some(value) => Some(
                        value
                            .into_iter()
                            .zip(new_value)
                            .map(|(value, new_value)| {
                                match value {
                                    // we have a precise value for this column
                                    Ok(value) => {
                                        // if another row matches
                                        if value == new_value {
                                            // if it has the same value, we didn't lose anything
                                            Ok(value)
                                        } else {
                                            // otherwise we degrade to a bit constraint
                                            Err(value | new_value)
                                        }
                                    }
                                    // if we already start with a mask, we update it with the new value
                                    Err(value) => Err(value | new_value),
                                }
                            })
                            .collect(),
                    ),
                }
            })
            // if the accumulator is None, no row matched, which is an error
            .ok_or_else(|| EvalError::Generic("Plookup is not satisfied".to_string()))?;

        let mut reasons = vec![];
        let mut result = vec![];
        for (l, r) in left.iter().zip(right_values) {
            match (l, r) {
                // we have a precise value
                (Ok(l), Ok(r)) => {
                    let evaluated = l.clone() - r.clone().into();
                    match evaluated.solve() {
                        Ok(constraints) => result.extend(constraints),
                        Err(_) => {
                            let formatted = l.format(fixed_data);
                            if evaluated.is_invalid() {
                                // Fail the whole lookup
                                return Err(
                                    format!("Constraint is invalid ({formatted} != {r}).",).into(),
                                );
                            } else {
                                reasons.push(
                                    format!("Could not solve expression {formatted} = {r}.",)
                                        .into(),
                                )
                            }
                        }
                    }
                }
                // we have a bitmask, we may be able to get a bit constraint if the lhs is `1 * x`
                (Ok(l), Err(mask)) => {
                    let constraint = l.generate_bit_constraint_from_mask(mask);
                    result.extend(constraint);
                }
                (Err(err), _) => {
                    reasons.push(format!("Value of LHS component too complex: {err}").into());
                }
            }
        }
        if result.is_empty() {
            Err(reasons.into_iter().reduce(eval_error::combine).unwrap())
        } else {
            Ok(result)
        }
    }
}
