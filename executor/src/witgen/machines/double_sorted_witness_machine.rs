use std::collections::{BTreeMap, HashMap, HashSet};
use std::iter::once;

use itertools::{Either, Itertools};
use num_traits::Zero;

use super::{FixedLookup, Machine};
use crate::witgen::affine_expression::AffineResult;
use crate::witgen::util::is_simple_poly_of_name;
use crate::witgen::{EvalError, EvalResult, FixedData};
use crate::witgen::{EvalValue, IncompleteCause};
use number::{FieldElement, FieldElementTrait};

use pil_analyzer::{Expression, Identity, IdentityKind, PolynomialReference, SelectedExpressions};

/// TODO make this generic

#[derive(Default)]
pub struct DoubleSortedWitnesses {
    //key_col: String,
    /// Position of the witness columns in the data.
    /// The key column has a position of usize::max
    //witness_positions: HashMap<String, usize>,
    /// (addr, step) -> value
    trace: BTreeMap<(FieldElement, FieldElement), Operation>,
    data: BTreeMap<FieldElement, FieldElement>,
}

struct Operation {
    pub is_write: bool,
    pub value: FieldElement,
}

impl DoubleSortedWitnesses {
    pub fn try_new(
        _fixed_data: &FixedData,
        _identities: &[&Identity],
        witness_cols: &HashSet<&PolynomialReference>,
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
            .symmetric_difference(&witness_cols.iter().map(|c| c.name.as_str()).collect())
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
    fn process_plookup<'a>(
        &mut self,
        _fixed_data: &FixedData,
        _fixed_lookup: &mut FixedLookup,
        kind: IdentityKind,
        left: &[AffineResult<&'a PolynomialReference>],
        right: &SelectedExpressions,
    ) -> Option<EvalResult<&'a PolynomialReference>> {
        if kind != IdentityKind::Permutation
            || !(is_simple_poly_of_name(right.selector.as_ref()?, "Assembly.m_is_read")
                || is_simple_poly_of_name(right.selector.as_ref()?, "Assembly.m_is_write"))
        {
            return None;
        }

        Some(self.process_plookup_internal(left, right))
    }

    fn witness_col_values(&mut self, fixed_data: &FixedData) -> HashMap<String, Vec<FieldElement>> {
        let mut addr = vec![];
        let mut step = vec![];
        let mut value = vec![];
        let mut op = vec![];
        let mut is_write = vec![];
        let mut is_read = vec![];

        for ((a, s), o) in std::mem::take(&mut self.trace) {
            addr.push(a);
            step.push(s);
            value.push(o.value);
            op.push(1.into());

            is_write.push(o.is_write.into());
            is_read.push((!o.is_write).into());
        }
        if addr.is_empty() {
            // No memory access at all - fill a first row with something.
            addr.push(0.into());
            step.push(0.into());
            value.push(0.into());
            op.push(0.into());
            is_write.push(0.into());
            is_read.push(0.into());
        }
        while addr.len() < fixed_data.degree as usize {
            addr.push(*addr.last().unwrap());
            step.push(*step.last().unwrap() + FieldElement::from(1));
            value.push(*value.last().unwrap());
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

impl DoubleSortedWitnesses {
    fn process_plookup_internal<'a>(
        &mut self,
        left: &[AffineResult<&'a PolynomialReference>],
        right: &SelectedExpressions,
    ) -> EvalResult<&'a PolynomialReference> {
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
            return Ok(EvalValue::incomplete(
                errors
                    .into_iter()
                    .cloned()
                    .reduce(|x, y| x.combine(y))
                    .unwrap(),
            ));
        }

        let is_write = match &right.selector {
            Some(Expression::PolynomialReference(p)) => p.name == "Assembly.m_is_write",
            _ => panic!(),
        };
        let addr = left[0].constant_value().ok_or_else(|| {
            format!(
                "Address must be known: {} = {}",
                left[0], right.expressions[0]
            )
        })?;
        let step = left[1]
            .constant_value()
            .ok_or_else(|| format!("Step must be known: {} = {}", left[1], right.expressions[1]))?;

        log::trace!(
            "Query addr={:x}, step={step}, write: {is_write}, left: {}",
            addr.to_integer(),
            left[2]
        );
        if !(addr.clone().to_integer() % 4u32).is_zero() {
            panic!("UNALIGNED");
        }

        // TODO this does not check any of the failure modes
        let mut assignments = EvalValue::complete(vec![]);
        if is_write {
            let value = match left[2].constant_value() {
                Some(v) => v,
                None => {
                    return Ok(EvalValue::incomplete(
                        IncompleteCause::NonConstantWriteValue,
                    ))
                }
            };

            log::debug!(
                "Memory write: addr={:x}, step={step}, value={:x}",
                addr.to_integer(),
                value.to_integer()
            );
            self.data.insert(addr, value);
            self.trace
                .insert((addr, step), Operation { is_write, value });
        } else {
            let value = self.data.entry(addr).or_default();
            self.trace.insert(
                (addr, step),
                Operation {
                    is_write,
                    value: *value,
                },
            );
            log::debug!(
                "Memory read: addr={:x}, step={step}, value={:x}",
                addr.to_integer(),
                value.to_integer()
            );
            let ass = (left[2].clone() - (*value).into())
                .solve()
                .map_err(|()| EvalError::ConstraintUnsatisfiable(String::new()))?;
            assignments.combine(ass);
        }
        Ok(assignments)
    }
}
