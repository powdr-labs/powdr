use std::collections::BTreeMap;
use std::collections::HashSet;

use super::block_machine::BlockMachine;
use super::double_sorted_witness_machine::DoubleSortedWitnesses;
use super::fixed_lookup_machine::FixedLookup;
use super::sorted_witness_machine::SortedWitnesses;
use super::FixedData;
use super::KnownMachine;
use crate::witgen::generator::Generator;
use crate::witgen::machines::write_once_memory::WriteOnceMemory;
use crate::witgen::machines::MachineParts;
use crate::Identity;
use itertools::Itertools;
use powdr_ast::analyzed::SelectedExpressions;
use powdr_ast::analyzed::{AlgebraicExpression as Expression, IdentityKind, PolyID};
use powdr_ast::parsed::visitor::ExpressionVisitable;
use powdr_number::FieldElement;

pub struct ExtractionOutput<'a, T: FieldElement> {
    pub machines: Vec<KnownMachine<'a, T>>,
    pub base_parts: MachineParts<'a, T>,
}

/// Finds machines in the witness columns and identities
/// and returns a list of machines and the identities
/// that are not "internal" to the machines.
pub fn split_out_machines<'a, T: FieldElement>(
    fixed: &'a FixedData<'a, T>,
    identities: Vec<&'a Identity<T>>,
) -> ExtractionOutput<'a, T> {
    let mut machines: Vec<KnownMachine<T>> = vec![];

    let all_witnesses = fixed.witness_cols.keys().collect::<HashSet<_>>();
    let mut remaining_witnesses = all_witnesses.clone();
    let mut base_identities = identities.clone();
    let mut id_counter = 0;
    for id in &identities {
        // Extract all witness columns in the RHS of the lookup.
        let lookup_witnesses = &refs_in_selected_expressions(&id.right) & (&remaining_witnesses);
        if lookup_witnesses.is_empty() {
            continue;
        }

        // Recursively extend the set to all witnesses connected through identities that preserve
        // a fixed row relation.
        let machine_witnesses =
            all_row_connected_witnesses(lookup_witnesses, &remaining_witnesses, &identities);

        // Split identities into those that only concern the machine
        // witnesses and those that concern any other witness.
        let (machine_identities, remaining_identities): (Vec<_>, _) =
            base_identities.iter().cloned().partition(|i| {
                // The identity's left side has at least one machine witness, but
                // all referenced witnesses are machine witnesses.
                // Note that expressions of "simple" polynomial identities are stored
                // in i.left.selector, so those are covered as well.
                // For lookups, any lookup calling from the current machine belongs
                // to the machine; lookups to the machine do not.
                let all_refs = &refs_in_selected_expressions(&i.left) & (&all_witnesses);
                !all_refs.is_empty() && all_refs.is_subset(&machine_witnesses)
            });
        base_identities = remaining_identities;
        remaining_witnesses = &remaining_witnesses - &machine_witnesses;

        // Identities that call into the current machine
        let connecting_identities = identities
            .iter()
            .cloned()
            .filter(|i| {
                refs_in_selected_expressions(&i.right)
                    .intersection(&machine_witnesses)
                    .next()
                    .is_some()
            })
            .map(|identity| (identity.id, identity))
            .collect::<BTreeMap<_, _>>();
        assert!(connecting_identities.contains_key(&id.id));

        log::trace!(
            "\nExtracted a machine with the following witnesses:\n{} \n and identities:\n{} \n and connecting identities:\n{}",
            machine_witnesses
                .iter()
                .map(|s| fixed.column_name(s))
                .sorted()
                .collect::<Vec<_>>()
                .join(", "),
            machine_identities
                .iter()
                .map(|id| id.to_string())
                .collect::<Vec<_>>()
                .join("\n"),
            connecting_identities
                .values()
                .map(|id| id.to_string())
                .collect::<Vec<_>>()
                .join("\n"),
        );

        let first_witness = machine_witnesses.iter().next().unwrap();
        let first_witness_name = fixed.column_name(first_witness);
        let namespace = first_witness_name
            .rfind("::")
            .map(|idx| &first_witness_name[..idx]);

        // For machines compiled using Powdr ASM we'll always have a namespace, but as a last
        // resort we'll use the first witness name.
        let name = namespace.unwrap_or(first_witness_name);
        let id = id_counter;
        id_counter += 1;
        let name_with_type = |t: &str| format!("Secondary machine {id}: {name} ({t})");

        let machine_parts = MachineParts::new(
            fixed,
            connecting_identities,
            machine_identities,
            machine_witnesses,
        );

        machines.push(build_machine(fixed, machine_parts, name_with_type));
    }

    // Always add a fixed lookup machine.
    // Note that this machine comes last, because some machines do a fixed lookup
    // in their take_witness_col_values() implementation.
    // TODO: We should also split this up and have several instances instead.
    let fixed_lookup = FixedLookup::new(
        fixed.global_range_constraints().clone(),
        identities.clone(),
        fixed,
    );
    machines.push(KnownMachine::FixedLookup(fixed_lookup));

    ExtractionOutput {
        machines,
        base_parts: MachineParts::new(
            fixed,
            Default::default(),
            base_identities,
            remaining_witnesses,
        ),
    }
}

fn build_machine<'a, T: FieldElement>(
    fixed_data: &'a FixedData<'a, T>,
    machine_parts: MachineParts<'a, T>,
    name_with_type: impl Fn(&str) -> String,
) -> KnownMachine<'a, T> {
    if let Some(machine) =
        SortedWitnesses::try_new(name_with_type("SortedWitness"), fixed_data, &machine_parts)
    {
        log::debug!("Detected machine: sorted witnesses / write-once memory");
        KnownMachine::SortedWitnesses(machine)
    } else if let Some(machine) = DoubleSortedWitnesses::try_new(
        name_with_type("DoubleSortedWitnesses"),
        fixed_data,
        &machine_parts,
    ) {
        log::debug!("Detected machine: memory");
        KnownMachine::DoubleSortedWitnesses(machine)
    } else if let Some(machine) = WriteOnceMemory::try_new(
        name_with_type("WriteOnceMemory"),
        fixed_data,
        &machine_parts,
    ) {
        log::debug!("Detected machine: write-once memory");
        KnownMachine::WriteOnceMemory(machine)
    } else if let Some(machine) =
        BlockMachine::try_new(name_with_type("BlockMachine"), fixed_data, &machine_parts)
    {
        log::debug!("Detected machine: {machine}");
        KnownMachine::BlockMachine(machine)
    } else {
        log::debug!("Detected machine: VM.");
        let latch = machine_parts.connecting_identities
            .values()
            .fold(None, |existing_latch, identity| {
                let current_latch = identity
                    .right
                    .selector
                    .as_ref()
                    .expect("Cannot handle lookup in this machine because it does not have a latch");
                if let Some(existing_latch) = existing_latch {
                    assert_eq!(
                        &existing_latch, current_latch,
                        "All connecting identities must have the same selector expression on the right hand side"
                    );
                    Some(existing_latch)
                } else {
                    Some(current_latch.clone())
                }
            })
            .unwrap();
        KnownMachine::Vm(Generator::new(
            name_with_type("Vm"),
            fixed_data,
            machine_parts.clone(),
            Some(latch),
        ))
    }
}

/// Extends a set of witnesses to the full set of row-connected witnesses.
/// Two witnesses are row-connected if they are part of a polynomial identity
/// or part of the same side of a lookup.
fn all_row_connected_witnesses<T>(
    mut witnesses: HashSet<PolyID>,
    all_witnesses: &HashSet<PolyID>,
    identities: &[&Identity<T>],
) -> HashSet<PolyID> {
    loop {
        let count = witnesses.len();
        for i in identities {
            match i.kind {
                IdentityKind::Polynomial => {
                    // Any current witness in the identity adds all other witnesses.
                    let in_identity = &refs_in_identity(i) & all_witnesses;
                    if in_identity.intersection(&witnesses).next().is_some() {
                        witnesses.extend(in_identity);
                    }
                }
                IdentityKind::Plookup | IdentityKind::Permutation | IdentityKind::Connect => {
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
pub fn refs_in_identity<T>(identity: &Identity<T>) -> HashSet<PolyID> {
    let mut refs: HashSet<PolyID> = Default::default();
    identity.pre_visit_expressions(&mut |expr| {
        ref_of_expression(expr).map(|id| refs.insert(id));
    });
    refs
}

/// Extracts all references to names from selected expressions.
pub fn refs_in_selected_expressions<T>(
    sel_expr: &SelectedExpressions<Expression<T>>,
) -> HashSet<PolyID> {
    let mut refs: HashSet<PolyID> = Default::default();
    sel_expr.pre_visit_expressions(&mut |expr| {
        ref_of_expression(expr).map(|id| refs.insert(id));
    });
    refs
}

/// Extracts all references to names from an expression,
/// NON-recursively.
pub fn ref_of_expression<T>(expr: &Expression<T>) -> Option<PolyID> {
    match expr {
        Expression::Reference(p) => Some(p.poly_id),
        _ => None,
    }
}
