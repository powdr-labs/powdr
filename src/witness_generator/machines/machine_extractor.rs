use std::collections::HashSet;

use crate::analyzer::{Expression, Identity, SelectedExpressions};

use super::double_sorted_witness_machine::DoubleSortedWitnesses;
use super::fixed_lookup_machine::FixedLookup;
use super::sorted_witness_machine::SortedWitnesses;
use super::FixedData;
use super::Machine;
use crate::witness_generator::WitnessColumn;

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

    // The lookup-in-fixed-columns machine, it always exists with an empty set of witnesses.
    let mut machines: Vec<Box<dyn Machine>> =
        vec![FixedLookup::try_new(fixed, &[], &Default::default()).unwrap()];

    let witness_names = witness_cols.iter().map(|c| c.name).collect::<HashSet<_>>();
    let all_witnesses = ReferenceExtractor::new(witness_names.clone());
    // Extract all witness columns in the RHS of lookups.
    let lookup_witnesses = identities
        .iter()
        .map(|i| all_witnesses.in_selected_expressions(&i.right))
        .reduce(|l, r| &l | &r)
        .unwrap_or_default();

    // Recursively extend the set to all witnesses connected through identities.
    let machine_witnesses = all_connected_witnesses(&all_witnesses, lookup_witnesses, identities);
    let machine_witness_extractor = ReferenceExtractor::new(machine_witnesses.clone());

    // Split identities into those that only concern the machine
    // witnesses and those that concern any other witness.
    let (machine_identities, base_identities): (Vec<_>, _) = identities.iter().partition(|i| {
        // The identity has at least one machine witness, but
        // all referenced witnesses are machine witnesses.
        let mw = machine_witness_extractor.in_identity(i);
        !mw.is_empty() && all_witnesses.in_identity(i).is_subset(&mw)
    });

    // TODO we probably need to check that machine witnesses do not appear
    // in any identity among `identities` except on the RHS.

    if let Some(machine) = SortedWitnesses::try_new(fixed, &machine_identities, &machine_witnesses)
    {
        if fixed.verbose {
            println!("Detected machine: sorted witnesses / write-once memory");
        }
        machines.push(machine);
    } else if let Some(machine) =
        DoubleSortedWitnesses::try_new(fixed, &machine_identities, &machine_witnesses)
    {
        if fixed.verbose {
            println!("Detected machine: memory");
        }
        machines.push(machine);
    }
    (machines, base_identities)
}

fn all_connected_witnesses<'a>(
    all_witnesses: &'a ReferenceExtractor,
    mut witnesses: HashSet<&'a str>,
    identities: &'a [Identity],
) -> HashSet<&'a str> {
    let mut count = witnesses.len();
    loop {
        for i in identities {
            match i.kind {
                crate::analyzer::IdentityKind::Polynomial => {
                    // Any current witness in the identity adds all other witnesses.
                    let in_identity = all_witnesses.in_identity(i);
                    if in_identity.intersection(&witnesses).next().is_some() {
                        witnesses.extend(in_identity);
                    }
                }
                crate::analyzer::IdentityKind::Plookup
                | crate::analyzer::IdentityKind::Permutation
                | crate::analyzer::IdentityKind::Connect => {
                    // If we already have witnesses on the LHS, include the RHS, but not vice-versa.
                    let in_lhs = all_witnesses.in_selected_expressions(&i.left);
                    let in_rhs = all_witnesses.in_selected_expressions(&i.right);
                    if in_lhs.intersection(&witnesses).next().is_some() {
                        witnesses.extend(in_lhs);
                        witnesses.extend(in_rhs);
                    } else if in_rhs.intersection(&witnesses).next().is_some() {
                        witnesses.extend(in_rhs);
                    }
                }
            };
        }
        if witnesses.len() == count {
            return witnesses;
        }
        count = witnesses.len()
    }
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
