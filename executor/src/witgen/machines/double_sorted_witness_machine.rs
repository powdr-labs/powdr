use std::collections::{BTreeMap, HashMap, HashSet};
use std::iter::once;

use ast::parsed::SelectedExpressions;
use itertools::Itertools;
use num_traits::Zero;

use super::{FixedLookup, Machine};
use crate::witgen::affine_expression::AffineExpression;
use crate::witgen::global_constraints::GlobalConstraints;
use crate::witgen::util::is_simple_poly_of_name;
use crate::witgen::{EvalResult, FixedData, MutableState, QueryCallback};
use crate::witgen::{EvalValue, IncompleteCause};
use number::{DegreeType, FieldElement};

use ast::analyzed::{
    AlgebraicExpression as Expression, AlgebraicReference, Identity, IdentityKind, PolyID,
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
    name: String,
    /// If the machine has the `m_diff_upper` and `m_diff_lower` columns, this is the base of the
    /// two digits.
    diff_columns_base: Option<u64>,
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
        name: String,
        fixed_data: &FixedData<T>,
        _identities: &[&Identity<Expression<T>>],
        witness_cols: &HashSet<PolyID>,
        global_range_constraints: &GlobalConstraints<T>,
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
            "m_is_write",
            "m_is_read",
        ]
        .into_iter()
        .collect();
        let difference = expected_witnesses
            .symmetric_difference(&columns)
            .collect::<HashSet<_>>();
        match difference.len() {
            0 => Some(Self {
                name,
                namespace,
                degree: fixed_data.degree,
                // No diff columns.
                diff_columns_base: None,
                ..Default::default()
            }),
            2 if difference == ["m_diff_upper", "m_diff_lower"].iter().collect() => {
                // We have the `m_diff_upper` and `m_diff_lower` columns.
                // Now, we check that they both have the same range constraint and use it to determine
                // the base of the two digits.
                let upper_poly_id =
                    fixed_data.try_column_by_name(&format!("{namespace}.m_diff_upper"))?;
                let upper_range_constraint =
                    global_range_constraints.witness_constraints[&upper_poly_id].as_ref()?;
                let lower_poly_id =
                    fixed_data.try_column_by_name(&format!("{namespace}.m_diff_lower"))?;
                let lower_range_constraint =
                    global_range_constraints.witness_constraints[&lower_poly_id].as_ref()?;

                let (min, max) = upper_range_constraint.range();

                if upper_range_constraint == lower_range_constraint && min == T::zero() {
                    let diff_columns_base = Some(max.to_degree() + 1);
                    Some(Self {
                        name,
                        namespace,
                        degree: fixed_data.degree,
                        diff_columns_base,
                        ..Default::default()
                    })
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

impl<'a, T: FieldElement> Machine<'a, T> for DoubleSortedWitnesses<T> {
    fn name(&self) -> &str {
        &self.name
    }

    fn process_plookup<Q: QueryCallback<T>>(
        &mut self,
        _mutable_state: &mut MutableState<'a, '_, T, Q>,
        kind: IdentityKind,
        left: &[AffineExpression<&'a AlgebraicReference, T>],
        right: &'a SelectedExpressions<Expression<T>>,
    ) -> Option<EvalResult<'a, T>> {
        if kind != IdentityKind::Permutation
            || !(is_simple_poly_of_name(right.selector.as_ref()?, &self.namespaced("m_is_read"))
                || is_simple_poly_of_name(right.selector.as_ref()?, &self.namespaced("m_is_write")))
        {
            return None;
        }

        Some(self.process_plookup_internal(left, right))
    }

    fn take_witness_col_values<'b, Q: QueryCallback<T>>(
        &mut self,
        _fixed_lookup: &'b mut FixedLookup<T>,
        _query_callback: &'b mut Q,
    ) -> HashMap<String, Vec<T>> {
        let mut addr = vec![];
        let mut step = vec![];
        let mut value = vec![];
        let mut is_write = vec![];
        let mut is_read = vec![];
        let mut diff = vec![];

        for ((a, s), o) in std::mem::take(&mut self.trace) {
            if let Some(prev_address) = addr.last() {
                assert!(a >= *prev_address, "Expected addresses to be sorted");
                if self.diff_columns_base.is_none()
                    && (a - *prev_address).to_degree() >= self.degree
                {
                    log::error!("Jump in memory accesses between {prev_address:x} and {a:x} is larger than or equal to the degree {}! This will violate the constraints.", self.degree);
                }

                let current_diff = if a != *prev_address {
                    a - *prev_address
                } else {
                    s - *step.last().unwrap()
                };
                assert!(current_diff > T::zero());
                diff.push(current_diff.to_degree() - 1);
            }

            addr.push(a);
            step.push(s);
            value.push(o.value);

            is_write.push(o.is_write.into());
            is_read.push((!o.is_write).into());
        }
        if addr.is_empty() {
            // No memory access at all - fill a first row with something.
            addr.push(0.into());
            step.push(0.into());
            value.push(0.into());
            is_write.push(0.into());
            is_read.push(0.into());
        }
        while addr.len() < self.degree as usize {
            addr.push(*addr.last().unwrap());
            step.push(*step.last().unwrap() + T::from(1));
            diff.push(0);
            value.push(*value.last().unwrap());
            is_write.push(0.into());
            is_read.push(0.into());
        }

        // We have all diffs, except from the last to the first element, which is unconstrained.
        assert_eq!(diff.len(), self.degree as usize - 1);
        diff.push(0);

        let change = addr
            .iter()
            .tuple_windows()
            .map(|(a, a_next)| if a == a_next { 0.into() } else { 1.into() })
            .chain(once(1.into()))
            .collect::<Vec<_>>();
        assert_eq!(change.len(), addr.len());

        let diff_columns = if let Some(diff_columns_base) = self.diff_columns_base {
            let diff_upper = diff
                .iter()
                .map(|d| T::from(*d / diff_columns_base))
                .collect::<Vec<_>>();
            let diff_lower = diff
                .iter()
                .map(|d| T::from(*d % diff_columns_base))
                .collect::<Vec<_>>();
            vec![
                (self.namespaced("m_diff_upper"), diff_upper),
                (self.namespaced("m_diff_lower"), diff_lower),
            ]
        } else {
            vec![]
        };

        [
            (self.namespaced("m_value"), value),
            (self.namespaced("m_addr"), addr),
            (self.namespaced("m_step"), step),
            (self.namespaced("m_change"), change),
            (self.namespaced("m_is_write"), is_write),
            (self.namespaced("m_is_read"), is_read),
        ]
        .into_iter()
        .chain(diff_columns)
        .collect()
    }
}

impl<T: FieldElement> DoubleSortedWitnesses<T> {
    fn process_plookup_internal<'a>(
        &mut self,
        left: &[AffineExpression<&'a AlgebraicReference, T>],
        right: &SelectedExpressions<Expression<T>>,
    ) -> EvalResult<'a, T> {
        // We blindly assume the lookup is of the form
        // OP { ADDR, STEP, X } is m_is_write { m_addr, m_step, m_value }
        // or
        // OP { ADDR, STEP, X } is m_is_read { m_addr, m_step, m_value }

        let is_write = match &right.selector {
            Some(Expression::Reference(p)) => p.name == self.namespaced("m_is_write"),
            _ => panic!(),
        };
        let addr = match left[0].constant_value() {
            Some(v) => v,
            None => {
                return Ok(EvalValue::incomplete(
                    IncompleteCause::NonConstantRequiredArgument("m_addr"),
                ))
            }
        };

        let step = left[1]
            .constant_value()
            .ok_or_else(|| format!("Step must be known: {} = {}", left[1], right.expressions[1]))?;

        log::trace!(
            "Query addr={:x}, step={step}, write: {is_write}, left: {}",
            addr.to_arbitrary_integer(),
            left[2]
        );
        if !(addr.clone().to_arbitrary_integer() % 4u32).is_zero() {
            panic!(
                "Unaligned memory access: addr={:x}, step={step}, write: {is_write}, left: {}",
                addr.to_arbitrary_integer(),
                left[2]
            );
        }

        // TODO this does not check any of the failure modes
        let mut assignments = EvalValue::complete(vec![]);
        if is_write {
            let value = match left[2].constant_value() {
                Some(v) => v,
                None => {
                    return Ok(EvalValue::incomplete(
                        IncompleteCause::NonConstantRequiredArgument("m_value"),
                    ))
                }
            };

            log::trace!(
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
            log::trace!(
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
