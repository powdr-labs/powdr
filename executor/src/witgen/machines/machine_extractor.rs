use std::collections::HashMap;
use std::collections::{BTreeMap, BTreeSet, HashSet};

use itertools::Itertools;
use powdr_ast::analyzed::AlgebraicReference;
use powdr_ast::analyzed::AlgebraicReferenceThin;
use powdr_ast::analyzed::PolynomialType;

use super::block_machine::BlockMachine;
use super::double_sorted_witness_machine_16::DoubleSortedWitnesses16;
use super::double_sorted_witness_machine_32::DoubleSortedWitnesses32;
use super::fixed_lookup_machine::FixedLookup;
use super::sorted_witness_machine::SortedWitnesses;
use super::FixedData;
use super::KnownMachine;
use super::Machine;
use crate::witgen::data_structures::identity::Identity;
use crate::witgen::machines::dynamic_machine::DynamicMachine;
use crate::witgen::machines::second_stage_machine::SecondStageMachine;
use crate::witgen::machines::{write_once_memory::WriteOnceMemory, MachineParts};

use powdr_ast::analyzed::{
    self, AlgebraicExpression as Expression, PolyID, PolynomialReference, Reference,
};
use powdr_ast::parsed::{self, visitor::AllChildren};
use powdr_number::FieldElement;

pub struct MachineExtractor<'a, T: FieldElement> {
    fixed: &'a FixedData<'a, T>,
}

impl<'a, T: FieldElement> MachineExtractor<'a, T> {
    pub fn new(fixed: &'a FixedData<'a, T>) -> Self {
        Self { fixed }
    }

    /// Finds machines in the witness columns and identities and returns a list of machines.
    /// The first returned machine is the "main machine", i.e. a machine that has no incoming connections.
    pub fn split_out_machines(&self) -> Vec<KnownMachine<'a, T>> {
        // Ignore prover functions that reference columns of later stages.
        let all_witnesses = self.fixed.witness_cols.keys().collect::<HashSet<_>>();
        let current_stage_witnesses = self
            .fixed
            .witnesses_until_current_stage()
            .collect::<HashSet<_>>();
        let later_stage_witness_names = all_witnesses
            .difference(&current_stage_witnesses)
            .map(|w| self.fixed.column_name(w))
            .collect::<HashSet<_>>();
        let prover_functions = self
            .fixed
            .analyzed
            .prover_functions
            .iter()
            .filter(|pf| {
                !refs_in_parsed_expression(pf)
                    .unique()
                    .any(|n| later_stage_witness_names.contains(n.as_str()))
            })
            .collect::<Vec<&analyzed::Expression>>();

        if self.fixed.stage() > 0 {
            let machine_parts = MachineParts::new(
                self.fixed,
                Default::default(),
                self.fixed.identities.iter().collect(),
                self.fixed.witness_cols.keys().collect::<HashSet<_>>(),
                self.fixed
                    .analyzed
                    .intermediate_columns
                    .iter()
                    .map(|(name, (s, _))| (s.into(), name.clone()))
                    .collect(),
                prover_functions,
            );

            return vec![KnownMachine::SecondStageMachine(SecondStageMachine::new(
                "Bus Machine".to_string(),
                self.fixed,
                machine_parts,
            ))];
        }
        let mut machines: Vec<KnownMachine<T>> = vec![];

        let mut publics = PublicsTracker::default();
        let mut remaining_witnesses = current_stage_witnesses.clone();
        let mut base_identities = self.fixed.identities.iter().collect::<Vec<_>>();
        let mut extracted_prover_functions = HashSet::new();
        let mut id_counter = 0;

        let mut fixed_lookup_receives = BTreeMap::new();

        for bus_receive in self.fixed.bus_receives.values() {
            // If the RHS only consists of fixed columns, record the connection and continue.
            if FixedLookup::is_responsible(bus_receive) {
                assert!(fixed_lookup_receives
                    .insert(bus_receive.bus_id, bus_receive)
                    .is_none());
                if let Some(multiplicity) = &bus_receive.multiplicity {
                    let poly_id = match multiplicity {
                        Expression::Reference(reference) => reference.poly_id,
                        _ => panic!(
                            "For fixed lookup, expected simple multiplicity, got: {multiplicity}"
                        ),
                    };
                    remaining_witnesses.remove(&poly_id);
                }
                continue;
            }

            // Extract all witness columns in the bus receive.
            let lookup_witnesses =
                &self.fixed.polynomial_references(bus_receive) & (&remaining_witnesses);
            if lookup_witnesses.is_empty() {
                // Skip connections to machines that were already created or point to FixedLookup.
                continue;
            }

            // Recursively extend the set to all witnesses connected through identities that preserve
            // a fixed row relation.
            let machine_witnesses =
                self.all_row_connected_witnesses(lookup_witnesses, &remaining_witnesses);

            // Split identities into those that only concern the machine
            // witnesses and those that concern any other witness.
            let (machine_identities, remaining_identities): (Vec<_>, _) =
                base_identities.iter().cloned().partition(|i| {
                    // The identity's left side has at least one machine witness, but
                    // all referenced witnesses are machine witnesses.
                    let all_refs =
                        &self.fixed.polynomial_references(*i) & (&current_stage_witnesses);
                    !all_refs.is_empty() && all_refs.is_subset(&machine_witnesses)
                });
            base_identities = remaining_identities;
            remaining_witnesses = &remaining_witnesses - &machine_witnesses;

            publics.add_all(machine_identities.as_slice()).unwrap();

            let machine_intermediates = intermediates_in_identities(
                &machine_identities,
                &self.fixed.intermediate_definitions,
            );

            // Connections that call into the current machine
            let machine_receives = self
                .fixed
                .bus_receives
                .values()
                .filter_map(|bus_receive| {
                    // check if the identity connects to the current machine
                    self.fixed
                        .polynomial_references(bus_receive)
                        .intersection(&machine_witnesses)
                        .next()
                        .is_some()
                        .then_some((bus_receive.bus_id, bus_receive))
                })
                .collect::<BTreeMap<_, _>>();
            assert!(machine_receives.contains_key(&bus_receive.bus_id));

            let prover_functions = prover_functions
                .iter()
                .copied()
                .enumerate()
                .filter(|(_, pf)| {
                    let refs = refs_in_parsed_expression(pf)
                        .unique()
                        .flat_map(|n| {
                            self.fixed.try_column_by_name(n).into_iter().chain(
                                // The reference might be an array, in which case it wouldn't
                                // be in the list of columns. So we try the first element as well.
                                self.fixed
                                    .try_column_by_name(&format!("{n}[0]"))
                                    .into_iter(),
                            )
                        })
                        .collect::<HashSet<_>>();
                    refs.intersection(&machine_witnesses).next().is_some()
                })
                .collect::<Vec<(_, &analyzed::Expression)>>();

            let machine_parts = MachineParts::new(
                self.fixed,
                machine_receives,
                machine_identities,
                machine_witnesses,
                machine_intermediates,
                prover_functions.iter().map(|&(_, pf)| pf).collect(),
            );

            for (i, pf) in &prover_functions {
                if !extracted_prover_functions.insert(*i) {
                    log::warn!("Prover function was assigned to multiple machines:\n{pf}");
                }
            }

            let name = suggest_machine_name(&machine_parts);
            let id = id_counter;
            id_counter += 1;
            let name_with_type = |t: &str| format!("Secondary machine {id}: {name} ({t})");

            machines.push(build_machine(self.fixed, machine_parts, name_with_type));
        }
        publics.add_all(base_identities.as_slice()).unwrap();

        // Always add a fixed lookup machine.
        // Note that this machine comes last, because some machines do a fixed lookup
        // in their take_witness_col_values() implementation.
        // TODO: We should also split this up and have several instances instead.
        let fixed_lookup = FixedLookup::new(
            self.fixed.global_range_constraints().clone(),
            self.fixed,
            fixed_lookup_receives,
        );

        machines.push(KnownMachine::FixedLookup(fixed_lookup));

        // Use the remaining prover functions as base prover functions.
        let base_prover_functions = prover_functions
            .iter()
            .enumerate()
            .filter_map(|(i, &pf)| (!extracted_prover_functions.contains(&i)).then_some(pf))
            .collect::<Vec<_>>();

        // In the remaining witness, we might still have some multiplicity columns
        // of fixed lookups, because they are not referenced by any "normal"
        // first-stage identities. As the main machine should not be on the
        // receiving end of a lookup, we remove any multiplicity columns here.
        let multiplicity_columns = self
            .fixed
            .bus_receives
            .values()
            .filter_map(|bus_receive| {
                bus_receive.multiplicity.as_ref().and_then(|m| match m {
                    Expression::Reference(reference) => Some(reference.poly_id),
                    _ => None,
                })
            })
            .collect::<HashSet<_>>();
        let main_witnesses = remaining_witnesses
            .difference(&multiplicity_columns)
            .cloned()
            .collect::<HashSet<_>>();
        let main_intermediates =
            intermediates_in_identities(&base_identities, &self.fixed.intermediate_definitions);

        log::trace!(
            "\nThe base machine contains the following witnesses:\n{}\n identities:\n{}\n and prover functions:\n{}",
            main_witnesses
                .iter()
                .map(|s| self.fixed.column_name(s))
                .sorted()
                .format(", "),
            base_identities.iter().format("\n"),
            base_prover_functions.iter().format("\n")
        );

        let base_parts = MachineParts::new(
            self.fixed,
            Default::default(),
            base_identities,
            main_witnesses,
            main_intermediates,
            base_prover_functions,
        );

        if let Some(main_machine) = build_main_machine(self.fixed, base_parts) {
            std::iter::once(main_machine).chain(machines).collect()
        } else {
            if !machines.is_empty() {
                log::error!("No main machine was extracted, but secondary machines were. Does the system have a cycle?");
            }
            vec![]
        }
    }

    /// Extends a set of witnesses to the full set of row-connected witnesses.
    /// Two witnesses are row-connected if they are part of a polynomial identity
    /// or part of the same side of a lookup.
    fn all_row_connected_witnesses(
        &self,
        mut witnesses: HashSet<PolyID>,
        all_witnesses: &HashSet<PolyID>,
    ) -> HashSet<PolyID> {
        loop {
            let count = witnesses.len();
            let references = self
                .fixed
                .identities
                .iter()
                .map(|i| self.fixed.polynomial_references(i))
                .chain(
                    self.fixed
                        .bus_receives
                        .values()
                        .map(|bus_receive| self.fixed.polynomial_references(bus_receive)),
                );
            for r in references {
                // Any current witness in the identity adds all other witnesses.
                let in_identity = &r & all_witnesses;
                if in_identity.intersection(&witnesses).next().is_some() {
                    witnesses.extend(in_identity);
                }
            }
            if witnesses.len() == count {
                return witnesses;
            }
        }
    }
}

fn extract_namespace(name: &str) -> &str {
    name.split("::").next().unwrap()
}

fn log_extracted_machine<T: FieldElement>(name: &str, parts: &MachineParts<'_, T>) {
    let namespaces = parts
        .witnesses
        .iter()
        .map(|s| extract_namespace(parts.column_name(s)))
        .collect::<BTreeSet<_>>();
    let exactly_one_namespace = namespaces.len() == 1;
    let log_level = if exactly_one_namespace {
        log::Level::Trace
    } else {
        log::Level::Warn
    };
    log::log!(
        log_level,
        "\nExtracted a machine {name} with the following witnesses:\n{}\n identities:\n{}\n bus receives:\n{}\n and prover functions:\n{}",
        parts.witnesses
            .iter()
            .map(|s|parts.column_name(s))
            .sorted()
            .format(", "),
        parts.identities
            .iter()
            .format("\n"),
        parts.bus_receives
            .values()
            .format("\n"),
        parts.prover_functions
            .iter()
            .format("\n")
    );
    if !exactly_one_namespace {
        log::warn!("The witnesses of the machine are in different namespaces: {namespaces:?}");
        log::warn!("In theory, witgen ignores namespaces, but in practice, this often means that something has gone wrong with the machine extraction.");
    }
}

fn suggest_machine_name<T: FieldElement>(parts: &MachineParts<'_, T>) -> String {
    let first_witness = parts.witnesses.iter().next().unwrap();
    let first_witness_name = parts.column_name(first_witness);
    let namespace = first_witness_name
        .rfind("::")
        .map(|idx| &first_witness_name[..idx]);

    // For machines compiled using Powdr ASM we'll always have a namespace, but as a last
    // resort we'll use the first witness name.
    namespace.unwrap_or(first_witness_name).to_string()
}

#[derive(Default)]
/// Keeps track of the global set of publics that are referenced by the machine's identities.
struct PublicsTracker<'a>(BTreeSet<&'a String>);

impl<'a> PublicsTracker<'a> {
    /// Given a machine's identities, add all publics that are referenced by them.
    /// Panics if a public is referenced by more than one machine.
    fn add_all<T>(&mut self, identities: &[&'a Identity<T>]) -> Result<(), String> {
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

fn build_main_machine<'a, T: FieldElement>(
    fixed_data: &'a FixedData<'a, T>,
    machine_parts: MachineParts<'a, T>,
) -> Option<KnownMachine<'a, T>> {
    (!machine_parts.witnesses.is_empty())
        .then(|| build_machine(fixed_data, machine_parts, |t| format!("Main machine ({t})")))
}

fn build_machine<'a, T: FieldElement>(
    fixed_data: &'a FixedData<'a, T>,
    machine_parts: MachineParts<'a, T>,
    name_with_type: impl Fn(&str) -> String,
) -> KnownMachine<'a, T> {
    let machine = if let Some(machine) =
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
        log::debug!("Detected machine: Dynamic machine.");
        // If there is a connection to this machine, all connections must have the same latch.
        // If there is no connection to this machine, it is the main machine and there is no latch.
        let latch = machine_parts.bus_receives
            .values()
            .fold(None, |existing_latch, receive| {
                let current_latch = &receive
                    .selected_payload
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
            });
        KnownMachine::DynamicMachine(DynamicMachine::new(
            name_with_type("Dynamic"),
            fixed_data,
            machine_parts.clone(),
            latch,
        ))
    };

    log_extracted_machine(machine.name(), &machine_parts);
    machine
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

fn try_as_intermediate_ref<T: FieldElement>(expr: &Expression<T>) -> Option<(PolyID, String)> {
    match expr {
        Expression::Reference(AlgebraicReference { poly_id, name, .. }) => {
            (poly_id.ptype == PolynomialType::Intermediate).then(|| (*poly_id, name.clone()))
        }
        _ => None,
    }
}

/// Returns all intermediate columns referenced in the identities as a map to their name.
/// Follows intermediate references recursively.
fn intermediates_in_identities<T: FieldElement>(
    identities: &[&Identity<T>],
    intermediate_definitions: &BTreeMap<AlgebraicReferenceThin, Expression<T>>,
) -> HashMap<PolyID, String> {
    let mut queue = identities
        .iter()
        .flat_map(|id| id.all_children())
        .filter_map(try_as_intermediate_ref)
        .collect::<BTreeSet<_>>();
    let mut intermediates = HashMap::new();
    while let Some((poly_id, name)) = queue.pop_first() {
        intermediates.insert(poly_id, name.clone());
        for (ref_id, ref_name) in intermediate_definitions[&poly_id.into()]
            .all_children()
            .filter_map(try_as_intermediate_ref)
        {
            if intermediates.insert(ref_id, ref_name.clone()).is_none() {
                queue.insert((ref_id, ref_name));
            }
        }
    }
    intermediates
}
