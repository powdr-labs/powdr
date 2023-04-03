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
        let mut matches = (0..fixed_data.degree as usize)
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
            })
            // deduplicate values
            .collect::<HashSet<_>>();

        let right_values = match matches.len() {
            // no match, we error out
            0 => {
                return Err(EvalError::Generic("Plookup is not satisfied".to_string()));
            }
            // a single match, we continue
            1 => matches.drain().next().unwrap(),
            // multiple matches, we stop and learnt nothing
            _ => return Ok(vec![]),
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
                        Err(EvalError::ConstraintUnsatisfiable(_)) => {
                            // Fail the whole lookup
                            return Err(EvalError::ConstraintUnsatisfiable(format!(
                                "Constraint is invalid ({} != {r}).",
                                l.format(fixed_data)
                            )));
                        }
                        Err(err) => reasons.push(
                            format!(
                                "Could not solve expression {} = {r}: {err}",
                                l.format(fixed_data)
                            )
                            .into(),
                        ),
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
