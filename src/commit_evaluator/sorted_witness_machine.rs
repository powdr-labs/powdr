use std::collections::{btree_map::Entry, BTreeMap, HashMap, HashSet};

use crate::analyzer::{Identity, SelectedExpressions};
use crate::commit_evaluator::machine::LookupReturn;
use crate::number::AbstractNumberType;

use super::affine_expression::AffineExpression;
use super::eval_error::EvalError;
use super::machine::{LookupResult, Machine};
use super::FixedData;

pub struct SortedWitnesses {
    /// These are the identities only concerning the machine columns.
    _identities: Vec<Identity>,
    /// Maps the witness polynomial names to their IDs internal to this component
    /// and optional parameter and query string.
    //witness_cols: BTreeMap<&'a str, &'a WitnessColumn<'a>>,
    witness_names: HashSet<String>,

    // TODO this is specific to the sorted write-once memory machine.
    data: BTreeMap<AbstractNumberType, AbstractNumberType>,
}

impl SortedWitnesses {
    pub fn try_new(
        _fixed_data: &FixedData,
        identities: &[&Identity],
        witness_names: &HashSet<&str>,
    ) -> Option<Box<Self>> {
        // TODO we should check the identities and select certain machines
        // based on patterns (and just return an object that implements a trait).
        // Now we just assume that we have two columns
        // that are sorted by a column called "m_addr"

        Some(Box::new(SortedWitnesses {
            _identities: identities.iter().map(|&i| i.clone()).collect(),
            witness_names: witness_names.iter().map(|&w| w.to_string()).collect(),
            data: Default::default(),
        }))
    }
}

impl Machine for SortedWitnesses {
    /// Process a plookup. The convention is to return an empty assignment
    /// if this is not a query to this machine and only return an error
    /// if the identity is violated.
    fn process_plookup(
        &mut self,
        fixed_data: &FixedData,
        left: &[Result<AffineExpression, EvalError>],
        right: &SelectedExpressions,
    ) -> LookupResult {
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
            return Ok(LookupReturn::NotApplicable);
        }
        assert_eq!(rhs[0].unwrap(), &"Assembly.m_addr".to_string());
        assert_eq!(rhs[1].unwrap(), &"Assembly.m_value".to_string());
        match (&left[0], &left[1]) {
            (Ok(a), Ok(v)) => match (a.constant_value(), v.constant_value()) {
                (Some(a), Some(v)) => match self.data.entry(a.clone()) {
                    Entry::Vacant(e) => {
                        if fixed_data.verbose {
                            println!("Stored in memory: {a}: {v}");
                        }
                        e.insert(v);
                        Ok(LookupReturn::Assignments(vec![]))
                    }
                    Entry::Occupied(e) => {
                        if e.get() != &v {
                            Err(format!(
                                    "Lookup mismatch: There is already a unique row {a}, {} (new value: {v})", e.get()).into())
                        } else {
                            Ok(LookupReturn::Assignments(vec![]))
                        }
                    }
                },
                (Some(a), None) => {
                    if let Some(value) = self.data.get(&a) {
                        if let Some(assignment) = (v.clone() - value.clone().into()).solve() {
                            if fixed_data.verbose {
                                println!("Read from memory: {a}: {value}");
                            }
                            Ok(LookupReturn::Assignments(vec![assignment]))
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
            (Err(e), Ok(_)) | (Ok(_), Err(e)) => {
                Err(format!("LHS value unknown for lookup: {e}").into())
            }
            (Err(e1), Err(e2)) => Err(format!("LHS values unknown for lookup: {e1}, {e2}").into()),
        }
    }

    fn witness_col_values(
        &mut self,
        fixed_data: &FixedData,
    ) -> HashMap<String, Vec<AbstractNumberType>> {
        let (mut addr, mut values): (Vec<_>, Vec<_>) =
            std::mem::take(&mut self.data).into_iter().unzip();
        values.resize(fixed_data.degree as usize, 0.into());
        let mut last_addr = addr.last().cloned().unwrap_or_default();
        while addr.len() < fixed_data.degree as usize {
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
