use std::collections::{btree_map::Entry, BTreeMap, HashMap, HashSet};

use crate::{
    analyzer::{Identity, SelectedExpressions},
    number::AbstractNumberType,
};

use super::{affine_expression::AffineExpression, EvalResult, FixedData};

/// A machine is a set of witness columns and identities where the columns
/// are used on the righ-hand-side
pub struct Machine<'a> {
    fixed_data: &'a FixedData<'a>,
    /// These are the identities only concerning the machine columns.
    _identities: Vec<&'a Identity>,
    /// Maps the witness polynomial names to their IDs internal to this component
    /// and optional parameter and query string.
    //witness_cols: BTreeMap<&'a String, &'a WitnessColumn<'a>>,
    witness_names: HashSet<&'a String>,

    // TODO this is specific to the sorted write-once memory machine.
    data: BTreeMap<AbstractNumberType, AbstractNumberType>,
}

impl<'a> Machine<'a> {
    pub fn new(
        fixed_data: &'a FixedData<'a>,
        identities: Vec<&'a Identity>,
        witness_names: HashSet<&'a String>,
    ) -> Machine<'a> {
        // TODO we should check the identities and select certain machines
        // based on patterns (and just return an object that implements a trait).
        // Now we just assume that we have two columns
        // that are sorted by a column called "m_addr"

        Machine {
            _identities: identities,
            fixed_data,
            witness_names,
            data: Default::default(),
        }
    }

    /// Process a plookup. The convention is to return an empty assignment
    /// if this is not a query to this machine and only return an error
    /// if the identity is violated.
    pub fn process_plookup(
        &mut self,
        left: &[Option<AffineExpression>],
        right: &SelectedExpressions,
    ) -> EvalResult {
        assert!(right.selector.is_none());
        let rhs = right
            .expressions
            .iter()
            .map(|e| match e {
                crate::analyzer::Expression::PolynomialReference(p) => {
                    if self.witness_names.contains(&p.name) {
                        Some(&p.name)
                    } else {
                        None
                    }
                }
                _ => None,
            })
            .collect::<Vec<_>>();
        if rhs.iter().any(|e| e.is_none()) {
            return Ok(vec![]);
        }
        assert_eq!(rhs[0].unwrap(), &"Assembly.m_addr".to_string());
        assert_eq!(rhs[1].unwrap(), &"Assembly.m_value".to_string());
        match (&left[0], &left[1]) {
            (Some(a), Some(v)) => match (a.constant_value(), v.constant_value()) {
                (Some(a), Some(v)) => match self.data.entry(a.clone()) {
                    Entry::Vacant(e) => {
                        if self.fixed_data.verbose {
                            println!("Stored in memory: {a}: {v}");
                        }
                        e.insert(v);
                        Ok(vec![])
                    }
                    Entry::Occupied(e) => {
                        if e.get() != &v {
                            Err(format!(
                                    "Lookup mismatch: There is already a unique row {a}, {} (new value: {v})", e.get()).into())
                        } else {
                            Ok(vec![])
                        }
                    }
                },
                (Some(a), None) => {
                    if let Some(value) = self.data.get(&a) {
                        if let Some(assignment) = (v.clone() - value.clone().into()).solve() {
                            if self.fixed_data.verbose {
                                println!("Read from memory: {a}: {value}");
                            }
                            Ok(vec![assignment])
                        } else {
                            Err("Cannot solve".to_string().into())
                        }
                    } else {
                        Err(format!("Value at address {a} not known").into())
                    }
                }
                (None, _) => Err("Address must be known: <format affin eexpr> TODO"
                    .to_string()
                    .into()),
            },

            _ => Ok(vec![]),
        }
    }

    pub fn witness_col_values(&mut self) -> HashMap<String, Vec<AbstractNumberType>> {
        let (mut addr, mut values): (Vec<_>, Vec<_>) =
            std::mem::take(&mut self.data).into_iter().unzip();
        values.resize(self.fixed_data.degree as usize, 0.into());
        let mut last_addr = addr.last().cloned().unwrap_or_default();
        while addr.len() < self.fixed_data.degree as usize {
            last_addr += 1;
            addr.push(last_addr.clone());
        }
        [
            ("Assembly.m_value".to_string(), values),
            ("Assembly.m_addr".to_string(), addr),
        ]
        .into_iter()
        .collect()
    }
}
