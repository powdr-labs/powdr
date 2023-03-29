use std::collections::{HashMap, HashSet};

use super::Machine;
use crate::analyzer::{Expression, Identity, IdentityKind, SelectedExpressions};
use crate::number::AbstractNumberType;

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
        right: &SelectedExpressions,
    ) -> EvalResult {
        let columns = right
            .expressions
            .iter()
            .map(|right_key| {
                if let Expression::PolynomialReference(poly) = right_key {
                    fixed_data
                        .fixed_cols
                        .get(poly.name.as_str())
                        .ok_or_else(|| {
                            format!(
                                "Unable to find a fixed column for the fixed lookup: {}",
                                poly.name
                            )
                        })
                } else {
                    Err("RHS must be a polynomial reference.".to_string())
                }
            })
            .collect::<Result<Vec<_>, _>>()?;

        let len = columns[0].len();

        let res = (0..len)
            // get all lookup rows which match the lhs
            .filter_map(|row| {
                left.iter()
                    .zip(columns.iter())
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
                    .map(|values| (row, values))
            })
            // deduplicate values
            .fold(None, |acc, (new_row, new_value)| {
                match acc {
                    None => Some(Ok((new_row, new_value))),
                    Some(Ok((row, value))) => {
                        // if a second row matches
                        if value == new_value {
                            // if it has the same value, we keep the first one we found
                            Some(Ok((row, value)))
                        } else {
                            // otherwise we can in the best case learn some bit constraints: the lhs is smaller than the max of all the matching cells
                            // TODO: if some of the new value coincides with the existing one, use that partial knowledge
                            // For this, the state here should be more granular to cover each column separately
                            let max: Vec<_> = value
                                .into_iter()
                                .zip(new_value)
                                .map(|(v0, v1)| std::cmp::max(v0, v1))
                                .collect();
                            Some(Err(max))
                        }
                    }
                    Some(Err(max)) => {
                        let max: Vec<_> = new_value
                            .into_iter()
                            .zip(max)
                            .map(|(v0, v1)| std::cmp::max(v0, v1))
                            .collect();
                        Some(Err(max))
                    }
                }
            })
            // if the accumulator is None, no row matched, which is an error
            .ok_or_else(|| EvalError::Generic("Plookup is not satisfied".to_string()))?;

        let (_, right_values) = match res {
            Ok(res) => res,
            Err(_) => {
                // TODO: create bit constraints from the max
                return Ok(vec![]);
            }
        };

        let mut reasons = vec![];
        let mut result = vec![];
        for (l, r) in left.iter().zip(right_values) {
            match l {
                Ok(l) => {
                    let evaluated = l.clone() - r.clone().into();
                    // TODO we could use bit constraints here
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
                Err(err) => {
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
