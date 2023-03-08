use std::collections::HashSet;

use crate::analyzer::{Expression, Identity, SelectedExpressions};

use super::machine::Machine;

use super::sorted_witness_machine::SortedWitnesses;
use super::{FixedData, WitnessColumn};

/// Finds machines in the witness columns and identities
/// and returns a list of machines and the identities
/// that are not "internal" to the machines.
pub fn split_out_machines<'a>(
    fixed: &'a FixedData<'a>,
    identities: &'a [Identity],
    witness_cols: &'a [WitnessColumn],
) -> (Vec<Box<dyn Machine>>, Vec<&'a Identity>) {
    // TODO we only split out one machine for now.
    // We could also split the machine into independent sub-machines.

    let witness_names = witness_cols.iter().map(|c| c.name).collect::<HashSet<_>>();
    let all_witnesses = ReferenceExtractor::new(witness_names.clone());
    // Extract all witness columns in the RHS of lookups.
    let machine_witnesses = identities
        .iter()
        .map(|i| all_witnesses.in_selected_expressions(&i.right))
        .reduce(|l, r| &l | &r)
        .unwrap_or_default();
    if machine_witnesses.is_empty() {
        return (vec![], identities.iter().collect());
    }

    let machine_witness_extractor = ReferenceExtractor::new(machine_witnesses.clone());

    // Split identities into those that only concern the machine
    // witnesses and those that concern any other witness.
    let (machine_identities, identities): (Vec<_>, _) = identities.iter().partition(|i| {
        // The identity has at least one a machine witness, but
        // all referenced witnesses are machine witnesses.
        let mw = machine_witness_extractor.in_identity(i);
        !mw.is_empty() && all_witnesses.in_identity(i).is_subset(&mw)
    });

    // TODO we probably nede to check that machine witnesses do not appear
    // in any identity among `identities` except on the RHS.

    let machine = SortedWitnesses::try_new(fixed, &machine_identities, &machine_witnesses);

    (vec![machine.unwrap()], identities)
}

/// Extracts all references to any of the given names
/// in expressions and identities.
struct ReferenceExtractor<'a> {
    names: HashSet<&'a str>,
}

impl<'a> ReferenceExtractor<'a> {
    pub fn new(names: HashSet<&'a str>) -> Self {
        ReferenceExtractor { names }
    }
    pub fn in_identity(&self, identity: &'a Identity) -> HashSet<&'a str> {
        &self.in_selected_expressions(&identity.left)
            | &self.in_selected_expressions(&identity.right)
    }
    pub fn in_selected_expressions(&self, selexpr: &'a SelectedExpressions) -> HashSet<&'a str> {
        selexpr
            .expressions
            .iter()
            .chain(selexpr.selector.iter())
            .map(|e| self.in_expression(e))
            .reduce(|l, r| &l | &r)
            .unwrap_or_default()
    }
    pub fn in_expression(&self, expr: &'a Expression) -> HashSet<&'a str> {
        match expr {
            Expression::Constant(_) => todo!(),
            Expression::PolynomialReference(p) => {
                if self.names.contains(p.name.as_str()) {
                    [p.name.as_str()].into()
                } else {
                    HashSet::default()
                }
            }
            Expression::Tuple(items) => self.in_expressions(items),
            Expression::BinaryOperation(l, _, r) => &self.in_expression(l) | &self.in_expression(r),
            Expression::UnaryOperation(_, e) => self.in_expression(e),
            Expression::FunctionCall(_, args) => self.in_expressions(args),
            Expression::LocalVariableReference(_)
            | Expression::PublicReference(_)
            | Expression::Number(_)
            | Expression::String(_) => HashSet::default(),
        }
    }
    pub fn in_expressions(&self, exprs: &'a [Expression]) -> HashSet<&'a str> {
        exprs
            .iter()
            .map(|e| self.in_expression(e))
            .reduce(|l, r| &l | &r)
            .unwrap_or_default()
    }
}
