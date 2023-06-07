use std::collections::{BTreeMap, HashMap, HashSet};
use std::iter::once;

use itertools::{Either, Itertools};
use num_traits::Zero;

use super::{FixedLookup, Machine};
use crate::witgen::affine_expression::AffineResult;
use crate::witgen::util::is_simple_poly_of_name;
use crate::witgen::{EvalError, EvalResult, FixedData};
use crate::witgen::{EvalValue, IncompleteCause};
use number::FieldElement;

use pil_analyzer::{Expression, Identity, IdentityKind, PolynomialReference, SelectedExpressions};

/// TODO make this generic

#[derive(Default)]
pub struct TripleSortedWitnesses<T> {
    trace: BTreeMap<(T, T, Operation), T>,
    data: BTreeMap<T, T>,
}

#[derive(Clone, Copy, Ord, PartialOrd, Eq, PartialEq)]
enum Operation {
    Read1,
    Read2,
    Write,
}

impl Operation {
    pub fn encode<T>(&self) -> T
    where
        T: From<u32>,
    {
        match self {
            Operation::Read1 => 1.into(),
            Operation::Read2 => 2.into(),
            Operation::Write => 3.into(),
        }
    }
}

impl<T: FieldElement> TripleSortedWitnesses<T> {
    pub fn try_new(
        _fixed_data: &FixedData<T>,
        _identities: &[&Identity<T>],
        witness_cols: &HashSet<&PolynomialReference>,
    ) -> Option<Box<Self>> {
        // TODO check the identities.
        let expected_witnesses: HashSet<_> = [
            "Memory.is_load1",
            "Memory.op",
            "Memory.address_change",
            "Memory.value",
            "Memory.addr",
            "Memory.is_write",
            "Memory.step",
            "Memory.is_load2",
            "Memory.step_change",
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

impl<T: FieldElement> Machine<T> for TripleSortedWitnesses<T> {
    fn process_plookup<'a>(
        &mut self,
        _fixed_data: &FixedData<T>,
        _fixed_lookup: &mut FixedLookup<T>,
        kind: IdentityKind,
        left: &[AffineResult<&'a PolynomialReference, T>],
        right: &SelectedExpressions<T>,
    ) -> Option<EvalResult<'a, T>> {
        if kind != IdentityKind::Permutation
            || !(is_simple_poly_of_name(right.selector.as_ref()?, "Memory.is_load1")
                || is_simple_poly_of_name(right.selector.as_ref()?, "Memory.is_load2")
                || is_simple_poly_of_name(right.selector.as_ref()?, "Memory.is_write"))
        {
            return None;
        }

        Some(self.process_plookup_internal(left, right))
    }

    fn witness_col_values(&mut self, fixed_data: &FixedData<T>) -> HashMap<String, Vec<T>> {
        let mut addr = vec![];
        let mut step = vec![];
        let mut value = vec![];
        let mut is_load1 = vec![];
        let mut is_load2 = vec![];
        let mut is_write = vec![];
        let mut op = vec![];

        for ((a, s, o), v) in std::mem::take(&mut self.trace) {
            addr.push(a);
            step.push(s);
            value.push(v);
            op.push(o.encode::<T>());
            is_load1.push((o == Operation::Read1).into());
            is_load2.push((o == Operation::Read2).into());
            is_write.push((o == Operation::Write).into());
        }
        if addr.is_empty() {
            todo!();
        }
        while addr.len() < fixed_data.degree as usize {
            addr.push(*addr.last().unwrap());
            step.push(*step.last().unwrap() + T::from(1));
            value.push(*value.last().unwrap());
            op.push(0.into());
            is_load1.push(0.into());
            is_load2.push(0.into());
            is_write.push(0.into());
        }

        let addr_change = addr
            .iter()
            .tuple_windows()
            .map(|(a, a_next)| if a == a_next { 0.into() } else { 1.into() })
            .chain(once(1.into()))
            .collect::<Vec<_>>();
        assert_eq!(addr_change.len(), addr.len());

        let step_change = step
            .iter()
            .tuple_windows()
            .map(|(s, s_next)| if s == s_next { 0.into() } else { 1.into() })
            .chain(once(1.into()))
            .collect::<Vec<_>>();
        assert_eq!(step_change.len(), step.len());

        for (((s, c), v), op) in addr.iter().zip(&step).zip(&value).zip(&is_write) {
            println!("{s} {c} {v} {op}");
        }

        vec![
            ("Memory.value", value),
            ("Memory.addr", addr),
            ("Memory.step", step),
            ("Memory.is_load1", is_load1),
            ("Memory.is_load2", is_load2),
            ("Memory.is_write", is_write),
            ("Memory.address_change", addr_change),
            ("Memory.step_change", step_change),
            ("Memory.op", op),
        ]
        .into_iter()
        .map(|(n, v)| (n.to_string(), v))
        .collect()
    }
}

impl<T: FieldElement> TripleSortedWitnesses<T> {
    fn process_plookup_internal<'a>(
        &mut self,
        left: &[AffineResult<&'a PolynomialReference, T>],
        right: &SelectedExpressions<T>,
    ) -> EvalResult<'a, T> {
        // We blindly assume the lookup is of the form
        // OP { ADDR, STEP, X } is is_load1/is_load2/is_write { addr, step, value }

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

        let operation = match &right.selector {
            Some(Expression::PolynomialReference(p)) => match p.name.as_str() {
                "Memory.is_load1" => Operation::Read1,
                "Memory.is_load2" => Operation::Read2,
                "Memory.is_write" => Operation::Write,
                _ => panic!(),
            },
            _ => panic!(),
        };

        let is_write = operation == Operation::Write;
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
            addr.to_arbitrary_integer(),
            left[2]
        );
        if !(addr.clone().to_arbitrary_integer() % 4u32).is_zero() {
            panic!("UNALIGNED");
        }

        // TODO we always have to handle all reads before all writes. enforce that somehow.
        // Maybe we could also just store the value that was there before the write if we do it on the same step.
        // TODO this does not check any of the failure modes
        let mut assignments = EvalValue::complete(vec![]);
        if is_write {
            let value = match left[2].constant_value() {
                Some(v) => v,
                None => {
                    return Ok(EvalValue::incomplete(
                        IncompleteCause::NonConstantWriteValue,
                    ));
                }
            };

            log::debug!(
                "Memory write: addr={:x}, step={step}, value={:x}",
                addr,
                value
            );
            // TODO this assumes that writes are always queried after reads.
            // This is probably OK since the value depends on the read values.
            // We should fix that properly anyway.
            self.data.insert(addr, value);
            self.trace.insert((addr, step, Operation::Write), value);
        } else {
            let value = self.data.entry(addr).or_default();
            self.trace.insert((addr, step, operation), *value);
            log::debug!(
                "Memory read: addr={:x}, step={step}, value={:x}",
                addr,
                value
            );
            let ass = (left[2].clone() - (*value).into())
                .solve()
                .map_err(|()| EvalError::ConstraintUnsatisfiable(String::new()))?;
            assignments.combine(ass);
        }
        Ok(assignments)
    }
}
