use std::collections::HashSet;

use super::double_sorted_witness_machine::DoubleSortedWitnesses;
use super::fixed_lookup_machine::FixedLookup;
use super::sorted_witness_machine::SortedWitnesses;
use super::FixedData;
use super::Machine;
use crate::analyzer::{Expression, Identity, SelectedExpressions};
use crate::witness_generator::WitnessColumn;

/// Finds machines in the witness columns and identities
/// and returns a list of machines and the identities
/// that are not "internal" to the machines.
pub fn split_out_machines<'a>(
    fixed: &'a FixedData<'a>,
    identities: &'a [Identity],
    witness_cols: &'a [WitnessColumn],
) -> (Vec<Box<dyn Machine>>, Vec<&'a Identity>) {
    // The lookup-in-fixed-columns machine, it always exists with an empty set of witnesses.
    let mut machines: Vec<Box<dyn Machine>> =
        vec![FixedLookup::try_new(fixed, &[], &Default::default()).unwrap()];

    let all_witnesses = witness_cols.iter().map(|c| c.name).collect::<HashSet<_>>();
    let mut remaining_witnesses = all_witnesses.clone();
    let mut base_identities = identities.iter().collect::<Vec<_>>();
    for id in identities {
        // Extract all witness columns in the RHS of the lookup.
        let lookup_witnesses = &refs_in_selected_expressions(&id.right) & (&remaining_witnesses);
        if lookup_witnesses.is_empty() {
            continue;
        }

        // Recursively extend the set to all witnesses connected through identities that preserve
        // a fixed row relation.
        let machine_witnesses =
            all_row_connected_witnesses(lookup_witnesses, &remaining_witnesses, identities);

        // Split identities into those that only concern the machine
        // witnesses and those that concern any other witness.
        let (machine_identities, remaining_identities): (Vec<_>, _) =
            base_identities.iter().partition(|i| {
                // The identity has at least one machine witness, but
                // all referenced witnesses are machine witnesses.
                let all_refs = &refs_in_identity(i) & (&all_witnesses);
                !all_refs.is_empty() && all_refs.is_subset(&machine_witnesses)
            });
        base_identities = remaining_identities;
        remaining_witnesses = &remaining_witnesses - &machine_witnesses;

        if fixed.verbose {
            println!(
                "Extracted a machine with the following witnesses and identities:\n{}\n{}",
                machine_witnesses
                    .iter()
                    .map(|s| s.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
                machine_identities
                    .iter()
                    .map(|id| id.to_string())
                    .collect::<Vec<_>>()
                    .join("\n")
            );
        }

        if let Some(machine) =
            SortedWitnesses::try_new(fixed, &machine_identities, &machine_witnesses)
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
        } else {
            println!(
                "Could not find a matching machine to handle a query to the following witness set:\n{}",
                machine_witnesses
                    .iter()
                    .map(|s| s.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            );
            remaining_witnesses = &remaining_witnesses | &machine_witnesses;
            base_identities.extend(machine_identities);
            println!("Will try to continue as is, but this probably requires a specialized machine implementation.");
        }
    }
    (machines, base_identities)
}

/// Extends a set of witnesses to the full set of row-connected witnesses.
/// Two witnesses are row-connected if they are part of a polynomial identity
/// or part of the same side of a lookup.
fn all_row_connected_witnesses<'a>(
    mut witnesses: HashSet<&'a str>,
    all_witnesses: &HashSet<&'a str>,
    identities: &'a [Identity],
) -> HashSet<&'a str> {
    loop {
        let count = witnesses.len();
        for i in identities {
            match i.kind {
                crate::analyzer::IdentityKind::Polynomial => {
                    // Any current witness in the identity adds all other witnesses.
                    let in_identity = &refs_in_identity(i) & all_witnesses;
                    if in_identity.intersection(&witnesses).next().is_some() {
                        witnesses.extend(in_identity);
                    }
                }
                crate::analyzer::IdentityKind::Plookup
                | crate::analyzer::IdentityKind::Permutation
                | crate::analyzer::IdentityKind::Connect => {
                    // If we already have witnesses on the LHS, include the LHS,
                    // and vice-versa, but not across the "sides".
                    let in_lhs = &refs_in_selected_expressions(&i.left) & all_witnesses;
                    let in_rhs = &refs_in_selected_expressions(&i.right) & all_witnesses;
                    if in_lhs.intersection(&witnesses).next().is_some() {
                        witnesses.extend(in_lhs);
                    } else if in_rhs.intersection(&witnesses).next().is_some() {
                        witnesses.extend(in_rhs);
                    }
                }
            };
        }
        if witnesses.len() == count {
            return witnesses;
        }
    }
}

/// Extracts all references to names from an identity.
pub fn refs_in_identity(identity: &Identity) -> HashSet<&str> {
    &refs_in_selected_expressions(&identity.left) | &refs_in_selected_expressions(&identity.right)
}

/// Extracts all references to names from selected expressions.
pub fn refs_in_selected_expressions(selexpr: &SelectedExpressions) -> HashSet<&str> {
    selexpr
        .expressions
        .iter()
        .chain(selexpr.selector.iter())
        .map(refs_in_expression)
        .reduce(|l, r| &l | &r)
        .unwrap_or_default()
}

/// Extracts all references to names from an expression
pub fn refs_in_expression(expr: &Expression) -> HashSet<&str> {
    match expr {
        Expression::Constant(_) => todo!(),
        Expression::PolynomialReference(p) => [p.name.as_str()].into(),
        Expression::Tuple(items) => refs_in_expressions(items),
        Expression::BinaryOperation(l, _, r) => &refs_in_expression(l) | &refs_in_expression(r),
        Expression::UnaryOperation(_, e) => refs_in_expression(e),
        Expression::FunctionCall(_, args) => refs_in_expressions(args),
        Expression::MatchExpression(scrutinee, arms) => {
            &refs_in_expression(scrutinee)
                | &arms
                    .iter()
                    .map(|(_, e)| refs_in_expression(e))
                    .reduce(|a, b| &a | &b)
                    .unwrap_or_default()
        }
        Expression::LocalVariableReference(_)
        | Expression::PublicReference(_)
        | Expression::Number(_)
        | Expression::String(_) => HashSet::default(),
    }
}

/// Extracts all references to names from expressions.
pub fn refs_in_expressions(exprs: &[Expression]) -> HashSet<&str> {
    exprs
        .iter()
        .map(refs_in_expression)
        .reduce(|l, r| &l | &r)
        .unwrap_or_default()
}
