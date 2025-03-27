use std::collections::{BTreeMap, BTreeSet, HashMap};

use itertools::{Either, Itertools};

use num_traits::One;
use powdr_ast::analyzed::{AlgebraicExpression, PolyID, PolynomialType};
use powdr_ast::parsed::visitor::AllChildren;
use powdr_number::{DegreeType, FieldElement};

use crate::witgen::data_structures::identity::BusReceive;
use crate::witgen::data_structures::mutable_state::MutableState;
use crate::witgen::global_constraints::RangeConstraintSet;
use crate::witgen::{
    util::try_to_simple_poly, EvalError, EvalResult, EvalValue, FixedData, IncompleteCause,
    QueryCallback,
};
use crate::witgen::{AffineExpression, AlgebraicVariable};

use super::{LookupCell, Machine, MachineParts};

/// A memory machine with a fixed address space, and each address can only have one
/// value during the lifetime of the program.
/// In the simplest case, it looks like this:
/// ```pil
/// let ADDR = |i| i;
/// let v;
/// // Stores a value, fails if the cell already has a value that's different
/// instr mstore X, Y -> { [X, Y] in [ADDR, v] }
/// // Loads a value. If the cell is empty, the prover can choose a value.
/// // Note that this is the same lookup, only Y is considered an output instead
/// // of an input.
/// instr mload X -> Y { [X, Y] in [ADDR, v] }
/// ```
pub struct WriteOnceMemory<'a, T: FieldElement> {
    degree: DegreeType,
    bus_receives: BTreeMap<T, &'a BusReceive<T>>,
    /// The fixed data
    fixed_data: &'a FixedData<'a, T>,
    /// The polynomials that are used as values (witness polynomials on the RHS)
    value_polys: Vec<PolyID>,
    /// A map from keys to row indices
    key_to_index: BTreeMap<Vec<T>, DegreeType>,
    /// The memory content
    data: BTreeMap<DegreeType, Vec<Option<T>>>,
    name: String,
    /// If true, exposes the first 8 public values as `hash_0`, `hash_1`, ..., `hash_7`.
    /// This is a special case for the `WriteOnceMemoryWith8Publics` machine.
    exposes_8_publics: bool,
    /// The PIL namespace of this machine instance
    namespace: String,
}

impl<'a, T: FieldElement> WriteOnceMemory<'a, T> {
    pub fn try_new(
        name: String,
        fixed_data: &'a FixedData<'a, T>,
        parts: &MachineParts<'a, T>,
    ) -> Option<Self> {
        // The only identities we'd expect would be to expose public values.
        if !parts.identities.iter().all(|id| {
            id.all_children()
                .any(|c| matches!(c, AlgebraicExpression::PublicReference(_)))
        }) {
            return None;
        }

        if parts.bus_receives.is_empty() {
            return None;
        }

        if !parts
            .bus_receives
            .values()
            .all(|r| r.has_arbitrary_multiplicity())
        {
            return None;
        }

        // All connecting identities should have a selector of 1
        if parts
            .bus_receives
            .values()
            .any(|r| !r.selected_payload.selector.is_one())
        {
            return None;
        }

        // All RHS expressions should be the same
        let rhs_exprs = parts
            .bus_receives
            .values()
            .map(|r| &r.selected_payload.expressions)
            .collect_vec();
        if !rhs_exprs.iter().all_equal() {
            return None;
        }

        let rhs_polys = rhs_exprs
            .first()
            .unwrap()
            .iter()
            .map(|e| try_to_simple_poly(e))
            .collect::<Option<Vec<_>>>();

        // Returns None if any RHS polynomial is a complex expression
        let rhs_polys = rhs_polys?;

        // Build a Vec<PolyID> for the key and value polynomials
        let (key_polys, value_polys): (Vec<_>, Vec<_>) = rhs_polys.into_iter().partition_map(|p| {
            assert!(!p.next);
            if p.poly_id.ptype == PolynomialType::Constant {
                Either::Left(p.poly_id)
            } else {
                Either::Right(p.poly_id)
            }
        });

        let degree = parts.common_degree_range().max;

        let mut key_to_index = BTreeMap::new();
        for row in 0..degree {
            let key = key_polys
                .iter()
                .map(|k| fixed_data.fixed_cols[k].values(degree)[row as usize])
                .collect::<Vec<_>>();
            if key_to_index.insert(key, row).is_some() {
                // Duplicate keys, can't be a write-once memory
                return None;
            }
        }

        if !parts.prover_functions.is_empty() {
            log::warn!(
                "WriteOnceMemory machine does not support prover functions.\
                The following prover functions are ignored:\n{}",
                parts.prover_functions.iter().format("\n")
            );
        }

        let namespace = parts
            .witnesses
            .iter()
            .map(|poly_id| {
                fixed_data
                    .column_name(poly_id)
                    .split("::")
                    .next()
                    .unwrap()
                    .to_string()
            })
            .unique()
            .exactly_one()
            .expect("Expected all columns to be in the same namespace");

        // Detect WriteOnceMemoryWith8Publics machine by checking for the <namespace>::hash_* publics
        let public_names_to_namespace = parts
            .fixed_data
            .analyzed
            .public_declarations_in_source_order()
            .map(|(name, _)| name)
            .collect::<BTreeSet<_>>();
        let exposes_8_publics = (0..8)
            .map(|i| format!("{namespace}::hash_{i}"))
            .all(|name| public_names_to_namespace.contains(&name));

        Some(Self {
            degree,
            bus_receives: parts.bus_receives.clone(),
            name,
            fixed_data,
            value_polys,
            key_to_index,
            data: BTreeMap::new(),
            exposes_8_publics,
            namespace,
        })
    }

    /// Returns the witness values of the machine, merging the externally provided values
    /// with the current state stored in `self.data`.
    fn get_witness_values(&self) -> HashMap<String, Vec<T>> {
        self.value_polys
            .iter()
            .enumerate()
            .map(|(value_index, poly)| {
                let column = self.fixed_data.witness_cols[poly]
                    .external_values
                    .cloned()
                    .map(|mut external_values| {
                        // External witness values might only be provided partially.
                        external_values.resize(self.degree as usize, T::zero());
                        external_values
                    })
                    .unwrap_or_else(|| {
                        let mut column = vec![T::zero(); self.degree as usize];
                        for (row, values) in self.data.iter() {
                            column[*row as usize] = values[value_index].unwrap_or_default();
                        }
                        column
                    });
                (*poly, column)
            })
            .map(|(poly_id, column)| (self.fixed_data.column_name(&poly_id).to_string(), column))
            .collect()
    }

    fn process_plookup_internal(
        &mut self,
        bus_id: T,
        arguments: &[AffineExpression<AlgebraicVariable<'a>, T>],
    ) -> EvalResult<'a, T> {
        let bus_receive = self.bus_receives[&bus_id];
        let (key_expressions, value_expressions): (Vec<_>, Vec<_>) = arguments
            .iter()
            .zip(bus_receive.selected_payload.expressions.iter())
            .partition(|(_, r)| {
                try_to_simple_poly(r).unwrap().poly_id.ptype == PolynomialType::Constant
            });
        let key = key_expressions
            .into_iter()
            .map(|(k, _)| k.constant_value())
            .collect::<Option<Vec<_>>>();
        let value_expressions = value_expressions
            .into_iter()
            .map(|(v, _)| v)
            .collect::<Vec<_>>();
        let value = value_expressions
            .iter()
            .map(|v| v.constant_value())
            .collect::<Vec<_>>();

        log::trace!("Key: {key:?}");
        log::trace!("Value: {value:?}");

        let Some(key) = key else {
            return Ok(EvalValue::incomplete(
                IncompleteCause::NonConstantRequiredArgument("key"),
            ));
        };

        let index = self.key_to_index.get(&key).cloned().ok_or_else(|| {
            EvalError::from(format!("Key {key:?} not found in write-once memory"))
        })?;

        // If there is an externally provided memory value, use it
        let external_witness_value = self
            .value_polys
            .iter()
            .map(|p| self.fixed_data.external_witness(index, p))
            .collect::<Vec<_>>();
        let stored_value = match self.data.get(&index) {
            Some(values) => values
                .iter()
                .zip(external_witness_value.iter())
                .map(|(&stored, &external)| external.or(stored))
                .collect(),
            None => external_witness_value,
        };

        let mut updates = vec![];
        let values = value_expressions
            .into_iter()
            .zip(stored_value.iter())
            .map(|(l, r)| {
                match (l.constant_value(), r) {
                    // No value provided and none stored -> keep value as None
                    (None, None) => Ok::<Option<T>, EvalError<T>>(None),
                    // Value provided but none stored -> write, no updates
                    (Some(l), None) => Ok(Some(l)),
                    // Value stored -> keep stored value & either update LHS or assert equality
                    (_, Some(r)) => {
                        updates.extend((l.clone() - (*r).into()).solve()?.constraints);
                        Ok(Some(*r))
                    }
                }
            })
            .collect::<Result<Vec<_>, _>>()?;

        // Write values
        let is_complete = !values.contains(&None);
        let side_effect = self.data.insert(index, values).is_none();

        match is_complete {
            true => Ok({
                let res = EvalValue::complete(updates);
                if side_effect {
                    res.report_side_effect()
                } else {
                    res
                }
            }),
            false => Ok(EvalValue::incomplete_with_constraints(
                updates,
                IncompleteCause::NonConstantRequiredArgument("value"),
            )),
        }
    }
}

impl<'a, T: FieldElement> Machine<'a, T> for WriteOnceMemory<'a, T> {
    fn process_lookup_direct<'b, 'c, Q: QueryCallback<T>>(
        &mut self,
        _mutable_state: &'b MutableState<'a, T, Q>,
        _bus_id: T,
        _values: &mut [LookupCell<'c, T>],
    ) -> Result<bool, EvalError<T>> {
        unimplemented!("Direct lookup not supported by machine {}.", self.name())
    }

    fn bus_ids(&self) -> Vec<T> {
        self.bus_receives.keys().copied().collect()
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn process_plookup<'b, Q: QueryCallback<T>>(
        &mut self,
        _mutable_state: &'b MutableState<'a, T, Q>,
        bus_id: T,
        arguments: &[AffineExpression<AlgebraicVariable<'a>, T>],
        _range_constraints: &dyn RangeConstraintSet<AlgebraicVariable<'a>, T>,
    ) -> EvalResult<'a, T> {
        self.process_plookup_internal(bus_id, arguments)
    }

    fn take_witness_col_values<'b, Q: QueryCallback<T>>(
        &mut self,
        _mutable_state: &'b MutableState<'a, T, Q>,
    ) -> HashMap<String, Vec<T>> {
        self.get_witness_values()
    }

    fn take_public_values(&mut self) -> BTreeMap<String, T> {
        if self.exposes_8_publics {
            // This code works specifically for the WriteOnceMemoryWith8Publics machine.
            // TODO: This could be generalized.
            let witness = self.get_witness_values();
            assert_eq!(witness.len(), 1, "Expected only one column of values.");
            let values = witness.into_values().next().unwrap();
            assert_eq!(values.len(), 8, "Expected exactly 8 cells.");

            values
                .into_iter()
                .enumerate()
                .map(|(i, v)| (format!("{}::hash_{i}", self.namespace), v))
                .collect()
        } else {
            BTreeMap::new()
        }
    }
}
