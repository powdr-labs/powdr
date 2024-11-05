use std::collections::{BTreeMap, BTreeSet, HashSet};

use itertools::Itertools;
use powdr_ast::analyzed::AlgebraicExpression;
use powdr_ast::analyzed::AlgebraicReference;
use powdr_ast::analyzed::LookupIdentity;
use powdr_ast::analyzed::PermutationIdentity;
use powdr_ast::analyzed::PhantomLookupIdentity;
use powdr_ast::analyzed::PhantomPermutationIdentity;
use powdr_number::DegreeType;

use super::block_machine::BlockMachine;
use super::double_sorted_witness_machine_16::DoubleSortedWitnesses16;
use super::double_sorted_witness_machine_32::DoubleSortedWitnesses32;
use super::fixed_lookup_machine::FixedLookup;
use super::sorted_witness_machine::SortedWitnesses;
use super::FixedData;
use super::KnownMachine;
use crate::witgen::machines::Connection;
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

    let phantom_lookups = identities
        .iter()
        .filter_map(|identity| match identity {
            Identity::PhantomLookup(phantom_lookup) => Some(phantom_lookup.clone()),
            _ => None,
        })
        .collect::<Vec<_>>();

    // Compute mapping: <identity ID> -> <multiplicity column ID>
    let identity_id_to_multiplicity = phantom_lookups
        .iter()
        .map(|id| match id.multiplicity {
            AlgebraicExpression::Reference(AlgebraicReference { poly_id, .. }) => (id.id, poly_id),
            _ => {
                unimplemented!(
                    "Only simple references are supported, got: {}",
                    id.multiplicity
                )
            }
        })
        .collect::<BTreeMap<_, _>>();

    // Multiplicity columns are not connected to any machine, because we removed identities that reference
    // challenges. We remove them here, so that they don't end up in the main machine.
    for multiplicity_id in identity_id_to_multiplicity.values() {
        remaining_witnesses.remove(multiplicity_id);
    }

    for id in &identities {
        // Extract all witness columns in the RHS of the lookup.
        let lookup_witnesses = match id {
            Identity::Lookup(LookupIdentity { right, .. })
            | Identity::PhantomLookup(PhantomLookupIdentity { right, .. })
            | Identity::Permutation(PermutationIdentity { right, .. })
            | Identity::PhantomPermutation(PhantomPermutationIdentity { right, .. }) => {
                &refs_in_selected_expressions(right) & (&remaining_witnesses)
            }
            _ => Default::default(),
        };
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
                // For lookups, any lookup calling from the current machine belongs
                // to the machine; lookups to the machine do not.
                let all_refs = &refs_in_identity_left(i) & (&all_witnesses);
                !all_refs.is_empty() && all_refs.is_subset(&machine_witnesses)
            });
        base_identities = remaining_identities;
        remaining_witnesses = &remaining_witnesses - &machine_witnesses;

        publics.add_all(machine_identities.as_slice()).unwrap();

        // Identities that call into the current machine
        let connections = identities
            .iter()
            .filter_map(|i| {
                let id = i.id();
                // identify potential connecting identities
                let i = Connection::try_from(*i).ok()?;

                // check if the identity connects to the current machine
                refs_in_selected_expressions(i.right)
                    .intersection(&machine_witnesses)
                    .next()
                    .is_some()
                    .then_some((id, i))
            })
            .collect::<BTreeMap<_, _>>();
        assert!(connections.contains_key(&id.id()));

        let prover_functions = prover_functions
            .iter()
            .copied()
            .enumerate()
            .filter(|(_, pf)| {
                let refs = refs_in_parsed_expression(pf)
                    .unique()
                    .filter_map(|n| fixed.column_by_name.get(n).cloned())
                    .collect::<HashSet<_>>();
                refs.intersection(&machine_witnesses).next().is_some()
            })
            .collect::<Vec<(_, &analyzed::Expression)>>();

        log::trace!(
            "\nExtracted a machine with the following witnesses:\n{}\n identities:\n{}\n connecting identities:\n{}\n and prover functions:\n{}",
            machine_witnesses
                .iter()
                .map(|s| fixed.column_name(s))
                .sorted()
                .format(", "),
            machine_identities
                .iter()
                .format("\n"),
            connections
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

        let identity_id_to_multiplicity_local = connections
            .keys()
            .filter_map(|id| {
                identity_id_to_multiplicity
                    .get(id)
                    .cloned()
                    .map(|m| (*id, m))
            })
            .collect::<BTreeMap<_, _>>();

        let machine_parts = MachineParts::new(
            fixed,
            connections,
            machine_identities,
            machine_witnesses,
            identity_id_to_multiplicity_local,
            prover_functions.iter().map(|&(_, pf)| pf).collect(),
        );

        machines.push(build_machine(fixed, machine_parts, name_with_type));
    }
    publics.add_all(base_identities.as_slice()).unwrap();

    // Always add a fixed lookup machine.
    // Note that this machine comes last, because some machines do a fixed lookup
    // in their take_witness_col_values() implementation.
    // TODO: We should also split this up and have several instances instead.

    // Compute sizes of fixed lookup multiplicity columns.
    let fixed_lookup_machine_sizes = phantom_lookups
        .iter()
        .filter_map(|id| {
            // Phantom lookups that point to FixedLookup
            let refs = refs_in_selected_expressions(&id.right);
            (&refs & &all_witnesses).is_empty().then_some((id.id, refs))
        })
        .map(|(identity_id, fixed_columns)| {
            let size = fixed_columns
                .iter()
                .map(|fixed_col| {
                    // Get unique size for fixed column
                    fixed.fixed_cols[fixed_col]
                        .values
                        .get_uniquely_sized()
                        .unwrap()
                        .len() as DegreeType
                })
                .unique()
                .exactly_one()
                .expect("All fixed columns on the same RHS must have the same size");
            let poly_id = identity_id_to_multiplicity[&identity_id];
            (poly_id, size)
        })
        .collect();
    let fixed_lookup = FixedLookup::new(
        fixed.global_range_constraints().clone(),
        identities.clone(),
        fixed,
        fixed_lookup_machine_sizes,
        identity_id_to_multiplicity,
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
            Default::default(),
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
