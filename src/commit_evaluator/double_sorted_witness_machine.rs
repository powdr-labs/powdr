use std::collections::{BTreeMap, HashMap, HashSet};
use std::iter::once;

use itertools::{Either, Itertools};

use crate::analyzer::PolynomialReference;
use crate::analyzer::{Expression, Identity, IdentityKind, SelectedExpressions};
use crate::commit_evaluator::eval_error;
use crate::commit_evaluator::machine::LookupReturn;
use crate::number::AbstractNumberType;

use super::affine_expression::AffineExpression;
use super::eval_error::EvalError;
use super::machine::{LookupResult, Machine};
use super::FixedData;

/// TODO make this generic

#[derive(Default)]
pub struct DoubleSortedWitnesses {
    //key_col: String,
    /// Position of the witness columns in the data.
    /// The key column has a position of usize::max
    //witness_positions: HashMap<String, usize>,
    /// (addr, step) -> value
    trace: BTreeMap<(AbstractNumberType, AbstractNumberType), Operation>,
    data: BTreeMap<AbstractNumberType, AbstractNumberType>,
}

struct Operation {
    pub is_write: bool,
    pub value: AbstractNumberType,
}

impl DoubleSortedWitnesses {
    pub fn try_new(
        _fixed_data: &FixedData,
        _identities: &[&Identity],
        witness_names: &HashSet<&str>,
    ) -> Option<Box<Self>> {
        // TODO check the identities.
        let expected_witnesses: HashSet<_> = [
            "Assembly.m_value",
            "Assembly.m_addr",
            "Assembly.m_step",
            "Assembly.m_change",
            "Assembly.m_op",
            "Assembly.m_is_write",
            "Assembly.m_is_read",
        ]
        .into_iter()
        .collect();
        if expected_witnesses
            .symmetric_difference(witness_names)
            .next()
            .is_none()
        {
            Some(Box::default())
        } else {
            None
        }
    }
}

impl Machine for DoubleSortedWitnesses {
    fn process_plookup(
        &mut self,
        fixed_data: &FixedData,
        kind: IdentityKind,
        left: &[Result<AffineExpression, EvalError>],
        right: &SelectedExpressions,
    ) -> LookupResult {
        if kind != IdentityKind::Permutation
            || (right.selector
                != Some(Expression::PolynomialReference(PolynomialReference {
                    name: "Assembly.m_is_read".to_owned(),
                    index: None,
                    next: false,
                }))
                && right.selector
                    != Some(Expression::PolynomialReference(PolynomialReference {
                        name: "Assembly.m_is_write".to_owned(),
                        index: None,
                        next: false,
                    })))
        {
            return Ok(LookupReturn::NotApplicable);
        }

        // We blindly assume the lookup is of the form
        // OP { ADDR, STEP, X } is m_is_write { m_addr, m_step, m_value }
        // or
        // OP { ADDR, STEP, X } is m_is_read { m_addr, m_step, m_value }

        // Fail if the LHS has an error.
        let (left, errors): (Vec<_>, Vec<_>) = left.iter().partition_map(|x| match x {
            Ok(x) => Either::Left(x),
            Err(x) => Either::Right(x),
        });
        if !errors.is_empty() {
            return Err(errors
                .into_iter()
                .cloned()
                .reduce(eval_error::combine)
                .unwrap());
        }

        let is_write = match &right.selector {
            Some(Expression::PolynomialReference(p)) => p.name == "Assembly.m_is_write",
            _ => panic!(),
        };
        let addr = left[0].constant_value().ok_or_else(|| {
            format!(
                "Address must be known: {} = {}",
                left[0].format(fixed_data),
                right.expressions[0]
            )
        })?;
        let step = left[1].constant_value().ok_or_else(|| {
            format!(
                "Step must be known: {} = {}",
                left[1].format(fixed_data),
                right.expressions[1]
            )
        })?;

        println!(
            "Query addr={addr}, step={step}, write: {is_write}, left: {}",
            left[2].format(fixed_data)
        );

        // TODO this does not check any of the failure modes
        let mut assignments = vec![];
        if is_write {
            let value = match left[2].constant_value() {
                Some(v) => v,
                None => return Ok(LookupReturn::Assignments(vec![])),
            };
            if fixed_data.verbose {
                println!("Memory write: addr={addr}, step={step}, value={value}");
            }
            self.data.insert(addr.clone(), value.clone());
            self.trace
                .insert((addr, step), Operation { is_write, value });
        } else {
            let value = self.data.entry(addr.clone()).or_default();
            self.trace.insert(
                (addr.clone(), step.clone()),
                Operation {
                    is_write,
                    value: value.clone(),
                },
            );
            if fixed_data.verbose {
                println!("Memory read: addr={addr}, step={step}, value={value}");
            }
            assignments.push(match (left[2].clone() - value.clone().into()).solve() {
                Some(ass) => ass,
                None => return Ok(LookupReturn::Assignments(vec![])),
            });
        }
        Ok(LookupReturn::Assignments(assignments))
    }

    fn witness_col_values(
        &mut self,
        fixed_data: &FixedData,
    ) -> HashMap<String, Vec<AbstractNumberType>> {
        let mut addr = vec![];
        let mut step = vec![];
        let mut value = vec![];
        let mut op = vec![];
        let mut is_write = vec![];
        let mut is_read = vec![];

        for ((a, s), o) in std::mem::take(&mut self.trace) {
            addr.push(a.clone());
            step.push(s.clone());
            value.push(o.value);
            op.push(1.into());

            is_write.push((if o.is_write { 1 } else { 0 }).into());
            is_read.push((if o.is_write { 0 } else { 1 }).into());
        }
        if addr.is_empty() {
            todo!();
        }
        while addr.len() < fixed_data.degree as usize {
            addr.push(addr.last().unwrap().clone());
            step.push(step.last().unwrap().clone() + 1);
            value.push(value.last().unwrap().clone());
            op.push(0.into());
            is_write.push(0.into());
            is_read.push(0.into());
        }

        let change = addr
            .iter()
            .tuple_windows()
            .map(|(a, a_next)| if a == a_next { 0.into() } else { 1.into() })
            .chain(once(1.into()))
            .collect::<Vec<_>>();
        assert_eq!(change.len(), addr.len());

        vec![
            ("Assembly.m_value", value),
            ("Assembly.m_addr", addr),
            ("Assembly.m_step", step),
            ("Assembly.m_change", change),
            ("Assembly.m_op", op),
            ("Assembly.m_is_write", is_write),
            ("Assembly.m_is_read", is_read),
        ]
        .into_iter()
        .map(|(n, v)| (n.to_string(), v))
        .collect()
    }
}
