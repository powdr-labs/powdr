use std::collections::{BTreeMap, HashMap, HashSet};
use std::iter::once;

use itertools::{Either, Itertools};
use num_traits::Zero;

use super::{FixedLookup, KnownMachine, Machine};
use crate::witgen::affine_expression::AffineResult;
use crate::witgen::util::is_simple_poly_of_name;
use crate::witgen::{EvalResult, FixedData};
use crate::witgen::{EvalValue, IncompleteCause};
use number::{DegreeType, FieldElement};

use ast::analyzed::{
    Expression, Identity, IdentityKind, PolyID, PolynomialReference, SelectedExpressions,
};

/// TODO make this generic

#[derive(Default)]
pub struct DoubleSortedWitnesses<T> {
    degree: DegreeType,
    //key_col: String,
    /// Position of the witness columns in the data.
    /// The key column has a position of usize::max
    //witness_positions: HashMap<String, usize>,
    /// (addr, step) -> value
    trace: BTreeMap<(T, T), Operation<T>>,
    data: BTreeMap<T, T>,
    namespace: String,
}

struct Operation<T> {
    pub is_write: bool,
    pub value: T,
}

impl<T: FieldElement> DoubleSortedWitnesses<T> {
    fn namespaced(&self, name: &str) -> String {
        format!("{}.{}", self.namespace, name)
    }

    pub fn try_new(
        fixed_data: &FixedData<T>,
        _identities: &[&Identity<T>],
        witness_cols: &HashSet<PolyID>,
    ) -> Option<Self> {
        // get the namespaces and column names
        let (mut namespaces, columns): (HashSet<_>, HashSet<_>) = witness_cols
            .iter()
            .map(|r| {
                let mut limbs = fixed_data.column_name(r).split('.');
                let namespace = limbs.next().unwrap();
                let col = limbs.next().unwrap();
                (namespace, col)
            })
            .unzip();

        if namespaces.len() > 1 {
            // columns are not in the same namespace, fail
            return None;
        }

        let namespace = namespaces.drain().next().unwrap().into();

        // TODO check the identities.
        let expected_witnesses: HashSet<_> = [
            "m_value",
            "m_addr",
            "m_step",
            "m_change",
            "m_op",
            "m_is_write",
            "m_is_read",
        ]
        .into_iter()
        .collect();
        if expected_witnesses
            .symmetric_difference(&columns)
            .next()
            .is_none()
        {
            Some(Self {
                // store the namespace
                namespace,
                degree: fixed_data.degree,
                ..Default::default()
            })
        } else {
            None
        }
    }
}

impl<'a, T: FieldElement> Machine<'a, T> for DoubleSortedWitnesses<T> {
    fn process_plookup(
        &mut self,
        _fixed_data: &FixedData<T>,
        _fixed_lookup: &mut FixedLookup<T>,
        kind: IdentityKind,
        left: &[AffineResult<&'a PolynomialReference, T>],
        right: &'a SelectedExpressions<T>,
        _machines: Vec<&mut KnownMachine<'a, T>>,
    ) -> Option<EvalResult<'a, T>> {
        if kind != IdentityKind::Permutation
            || !(is_simple_poly_of_name(right.selector.as_ref()?, &self.namespaced("m_is_read"))
                || is_simple_poly_of_name(right.selector.as_ref()?, &self.namespaced("m_is_write")))
        {
            return None;
        }

        Some(self.process_plookup_internal(left, right))
    }

    fn take_witness_col_values(
        &mut self,
        fixed_data: &FixedData<T>,
        _fixed_lookup: &mut FixedLookup<T>,
    ) -> HashMap<String, Vec<T>> {
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
            step.push(*step.last().unwrap() + T::from(1));
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

        [
            (self.namespaced("m_value"), value),
            (self.namespaced("m_addr"), addr),
            (self.namespaced("m_step"), step),
            (self.namespaced("m_change"), change),
            (self.namespaced("m_op"), op),
            (self.namespaced("m_is_write"), is_write),
            (self.namespaced("m_is_read"), is_read),
        ]
        .into_iter()
        .collect()
    }
}

impl<T: FieldElement> DoubleSortedWitnesses<T> {
    fn process_plookup_internal<'a>(
        &mut self,
        left: &[AffineResult<&'a PolynomialReference, T>],
        right: &SelectedExpressions<T>,
    ) -> EvalResult<'a, T> {
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
            Some(Expression::PolynomialReference(p)) => p.name == self.namespaced("m_is_write"),
            _ => panic!(),
        };
        let addr = left[0].constant_value().ok_or_else(|| {
            format!(
                "Address must be known: {} = {}",
                left[0], right.expressions[0]
            )
        })?;
        if addr.to_degree() >= self.degree {
            return Err(format!(
                "Memory access to too large address: 0x{addr:x} (must be less than 0x{:x})",
                self.degree
            )
            .into());
        }
        let step = left[1]
            .constant_value()
            .ok_or_else(|| format!("Step must be known: {} = {}", left[1], right.expressions[1]))?;

        log::trace!(
            "Query addr={:x}, step={step}, write: {is_write}, left: {}",
            addr.to_arbitrary_integer(),
            left[2]
        );
        if !(addr.clone().to_arbitrary_integer() % 4u32).is_zero() {
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
                addr,
                value
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
                addr,
                value
            );
            let ass = (left[2].clone() - (*value).into()).solve()?;
            assignments.combine(ass);
        }
        Ok(assignments)
    }
}
