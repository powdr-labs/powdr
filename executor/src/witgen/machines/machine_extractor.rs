use std::collections::{BTreeMap, BTreeSet, HashSet};

use itertools::Itertools;
use num_traits::One;
use powdr_ast::analyzed::LookupIdentity;
use powdr_ast::analyzed::PermutationIdentity;
use powdr_ast::analyzed::PhantomLookupIdentity;
use powdr_ast::analyzed::PhantomPermutationIdentity;
use powdr_ast::analyzed::PolynomialType;

use super::block_machine::BlockMachine;
use super::double_sorted_witness_machine_16::DoubleSortedWitnesses16;
use super::double_sorted_witness_machine_32::DoubleSortedWitnesses32;
use super::fixed_lookup_machine::FixedLookup;
use super::sorted_witness_machine::SortedWitnesses;
use super::FixedData;
use super::KnownMachine;
use crate::witgen::machines::Connection;
use crate::witgen::util::try_to_simple_poly_ref;
use crate::witgen::{
    generator::Generator,
    machines::{write_once_memory::WriteOnceMemory, MachineParts},
};
use crate::Identity;

use powdr_ast::analyzed::{
    self, AlgebraicExpression as Expression, PolyID, PolynomialReference, Reference,
    SelectedExpressions,
};
use powdr_ast::parsed::{
    self,
    visitor::{AllChildren, Children},
};
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
    stage: u8,
) -> ExtractionOutput<'a, T> {
    let mut machines: Vec<KnownMachine<T>> = vec![];

    // Ignore prover functions that reference columns of later stages.
    let prover_functions = fixed
        .analyzed
        .prover_functions
        .iter()
        .filter(|pf| {
            refs_in_parsed_expression(pf).unique().all(|n| {
                let def = fixed.analyzed.definitions.get(n);
                def.and_then(|(s, _)| s.stage).unwrap_or_default() <= stage as u32
            })
        })
        .collect::<Vec<&analyzed::Expression>>();

    let all_witnesses = fixed.witness_cols.keys().collect::<HashSet<_>>();
    let mut publics = PublicsTracker::default();
    let mut remaining_witnesses = all_witnesses.clone();
    let mut base_identities = identities.clone();
    let mut extracted_prover_functions = HashSet::new();
    let mut id_counter = 0;

    let all_connections = identities
        .iter()
        .filter_map(|i| Connection::try_from(*i).ok())
        .collect::<Vec<_>>();

    let mut fixed_lookup_connections = BTreeMap::new();

    for connection in &all_connections {
        // If the RHS only consists of fixed columns, record the connection and continue.
        if connection.is_lookup()
            && connection.right.selector.is_one()
            && connection.right.expressions.iter().all(|e| {
                try_to_simple_poly_ref(e)
                    .map(|poly| poly.poly_id.ptype == PolynomialType::Constant)
                    .unwrap_or(false)
            })
            && !connection.right.expressions.is_empty()
        {
            assert!(fixed_lookup_connections
                .insert(connection.id, *connection)
                .is_none());
            if let Some(multiplicity) = connection.multiplicity_column {
                remaining_witnesses.remove(&multiplicity);
            }
            continue;
        }

        // Extract all witness columns in the RHS of the lookup.
        let lookup_witnesses = {
            let callee_columns = refs_in_selected_expressions(connection.right)
                .into_iter()
                .chain(connection.multiplicity_column.iter().cloned())
                .collect();
            &callee_columns & (&remaining_witnesses)
        };
        if lookup_witnesses.is_empty() {
            // Skip connections to machines that were already created or point to FixedLookup.
            continue;
        }

        // Recursively extend the set to all witnesses connected through identities that preserve
        // a fixed row relation.
        let core_machine_witnesses =
            all_row_connected_witnesses(lookup_witnesses, &remaining_witnesses, &identities);

        // Split identities into those that only concern the machine
        // witnesses and those that concern any other witness.
        let (machine_identities, remaining_identities): (Vec<_>, _) =
            base_identities.iter().cloned().partition(|i| {
                // The identity's left side has at least one machine witness, but
                // all referenced witnesses are machine witnesses.
                // For lookups, any lookup calling from the current machine belongs
                // to the machine; lookups to the machine do not.
                let all_refs = &refs_in_identity_left(i) & (&all_witnesses);
                !all_refs.is_empty() && all_refs.is_subset(&core_machine_witnesses)
            });
        base_identities = remaining_identities;
        remaining_witnesses = &remaining_witnesses - &core_machine_witnesses;

        publics.add_all(machine_identities.as_slice()).unwrap();

        // Connections that call into the current machine
        let machine_connections = all_connections
            .iter()
            .filter_map(|connection| {
                // check if the identity connects to the current machine
                refs_in_selected_expressions(connection.right)
                    .intersection(&core_machine_witnesses)
                    .next()
                    .is_some()
                    .then_some((connection.id, *connection))
            })
            .collect::<BTreeMap<_, _>>();
        assert!(machine_connections.contains_key(&connection.id));

        let prover_functions = prover_functions
            .iter()
            .copied()
            .enumerate()
            .filter(|(_, pf)| {
                let refs = refs_in_parsed_expression(pf)
                    .unique()
                    .filter_map(|n| fixed.column_by_name.get(n).cloned())
                    .collect::<HashSet<_>>();
                refs.intersection(&core_machine_witnesses).next().is_some()
            })
            .collect::<Vec<(_, &analyzed::Expression)>>();

        log::trace!(
            "\nExtracted a machine with the following witnesses:\n{}\n identities:\n{}\n connecting identities:\n{}\n and prover functions:\n{}",
            core_machine_witnesses
                .iter()
                .map(|s| fixed.column_name(s))
                .sorted()
                .format(", "),
            machine_identities
                .iter()
                .format("\n"),
            machine_connections
                .values()
                .map(|id| id.to_string())
                .format("\n"),
            prover_functions
                .iter()
                .map(|(_, pf)| format!("{pf}"))
                .format("\n")
        );

        for (i, pf) in &prover_functions {
            if !extracted_prover_functions.insert(*i) {
                log::warn!("Prover function was assigned to multiple machines:\n{pf}");
            }
        }

        let first_witness = core_machine_witnesses.iter().next().unwrap();
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
            machine_connections,
            machine_identities,
            core_machine_witnesses,
            prover_functions.iter().map(|&(_, pf)| pf).collect(),
        );

        machines.push(build_machine(fixed, machine_parts, name_with_type));
    }
    publics.add_all(base_identities.as_slice()).unwrap();

    // Always add a fixed lookup machine.
    // Note that this machine comes last, because some machines do a fixed lookup
    // in their take_witness_col_values() implementation.
    // TODO: We should also split this up and have several instances instead.
    let fixed_lookup = FixedLookup::new(
        fixed.global_range_constraints().clone(),
        fixed,
        fixed_lookup_connections,
    );

    machines.push(KnownMachine::FixedLookup(fixed_lookup));

    // Use the remaining prover functions as base prover functions.
    let base_prover_functions = prover_functions
        .iter()
        .enumerate()
        .filter_map(|(i, &pf)| (!extracted_prover_functions.contains(&i)).then_some(pf))
        .collect::<Vec<_>>();

    log::trace!(
        "\nThe base machine contains the following witnesses:\n{}\n identities:\n{}\n and prover functions:\n{}",
        remaining_witnesses
            .iter()
            .map(|s| fixed.column_name(s))
            .sorted()
            .format(", "),
        base_identities
            .iter()
            .format("\n"),
        base_prover_functions.iter().format("\n")
    );

    ExtractionOutput {
        machines,
        base_parts: MachineParts::new(
            fixed,
            Default::default(),
            base_identities,
            remaining_witnesses,
            base_prover_functions,
        ),
    }
}

#[derive(Default)]
/// Keeps track of the global set of publics that are referenced by the machine's identities.
struct PublicsTracker<'a>(BTreeSet<&'a String>);

impl<'a> PublicsTracker<'a> {
    /// Given a machine's identities, add all publics that are referenced by them.
    /// Panics if a public is referenced by more than one machine.
    fn add_all<T>(
        &mut self,
        identities: &[&'a powdr_ast::analyzed::Identity<T>],
    ) -> Result<(), String> {
        let referenced_publics = identities
            .iter()
            .flat_map(|id| id.all_children())
            .filter_map(|expr| match expr {
                Expression::PublicReference(public_name) => Some(public_name),
                _ => None,
            })
            .collect();
        let intersection = self
            .0
            .intersection(&referenced_publics)
            .collect::<BTreeSet<_>>();
        if !intersection.is_empty() {
            let intersection_list = intersection.iter().format(", ");
            return Err(format!(
                "Publics are referenced by more than one machine: {intersection_list}",
            ));
        }
        self.0.extend(referenced_publics);
        Ok(())
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
    } else if let Some(machine) = DoubleSortedWitnesses16::try_new(
        name_with_type("DoubleSortedWitnesses16"),
        fixed_data,
        &machine_parts,
    ) {
        log::debug!("Detected machine: memory16");
        KnownMachine::DoubleSortedWitnesses16(machine)
    } else if let Some(machine) = DoubleSortedWitnesses32::try_new(
        name_with_type("DoubleSortedWitnesses32"),
        fixed_data,
        &machine_parts,
    ) {
        log::debug!("Detected machine: memory32");
        KnownMachine::DoubleSortedWitnesses32(machine)
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
        let latch = machine_parts.connections
            .values()
            .fold(None, |existing_latch, identity| {
                let current_latch = &identity
                    .right
                    .selector;
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
            match i {
                Identity::Polynomial(i) => {
                    // Any current witness in the identity adds all other witnesses.
                    let in_identity = &refs_in_expression(&i.expression).collect() & all_witnesses;
                    if in_identity.intersection(&witnesses).next().is_some() {
                        witnesses.extend(in_identity);
                    }
                }
                Identity::Lookup(LookupIdentity { left, right, .. })
                | Identity::Permutation(PermutationIdentity { left, right, .. })
                | Identity::PhantomLookup(PhantomLookupIdentity { left, right, .. })
                | Identity::PhantomPermutation(PhantomPermutationIdentity {
                    left, right, ..
                }) => {
                    // If we already have witnesses on the LHS, include the LHS,
                    // and vice-versa, but not across the "sides".
                    let in_lhs = &refs_in_selected_expressions(left) & all_witnesses;
                    let in_rhs = &refs_in_selected_expressions(right) & all_witnesses;
                    if in_lhs.intersection(&witnesses).next().is_some() {
                        witnesses.extend(in_lhs);
                    } else if in_rhs.intersection(&witnesses).next().is_some() {
                        witnesses.extend(in_rhs);
                    }
                }
                Identity::Connect(..) => {
                    unimplemented!()
                }
            };
        }
        if witnesses.len() == count {
            return witnesses;
        }
    }
}

/// Extracts all references to names from selected expressions.
fn refs_in_selected_expressions<T>(sel_expr: &SelectedExpressions<T>) -> HashSet<PolyID> {
    sel_expr
        .children()
        .flat_map(|e| refs_in_expression(e))
        .collect()
}

/// Extracts all references to names from the "left" side of an identity. This is the left selected expressions for connecting identities, and everything for other identities.
fn refs_in_identity_left<T>(identity: &Identity<T>) -> HashSet<PolyID> {
    match identity {
        Identity::Lookup(LookupIdentity { left, .. })
        | Identity::PhantomLookup(PhantomLookupIdentity { left, .. })
        | Identity::Permutation(PermutationIdentity { left, .. })
        | Identity::PhantomPermutation(PhantomPermutationIdentity { left, .. }) => {
            refs_in_selected_expressions(left)
        }
        Identity::Polynomial(i) => refs_in_expression(&i.expression).collect(),
        Identity::Connect(i) => i
            .left
            .iter()
            .chain(&i.right)
            .flat_map(refs_in_expression)
            .collect(),
    }
}

fn refs_in_expression<T>(expr: &Expression<T>) -> impl Iterator<Item = PolyID> + '_ {
    expr.all_children().filter_map(|e| match e {
        Expression::Reference(p) => Some(p.poly_id),
        _ => None,
    })
}

// This only discovers direct references in the expression
// and ignores e.g. called functions, but it will work for now.
fn refs_in_parsed_expression(expr: &analyzed::Expression) -> impl Iterator<Item = &String> + '_ {
    expr.all_children().filter_map(|e| match e {
        parsed::Expression::Reference(_, Reference::Poly(PolynomialReference { name, .. })) => {
            Some(name)
        }
        _ => None,
    })
}
