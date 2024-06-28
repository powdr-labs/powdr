use std::{
    collections::{BTreeMap, BTreeSet},
    ops::ControlFlow,
    str::FromStr,
};

use powdr_ast::{
    analyzed::{AlgebraicExpression, Analyzed, Identity, IdentityKind, StatementIdentifier},
    parsed::{
        asm::{AbsoluteSymbolPath, SymbolPath},
        visitor::{ExpressionVisitable, VisitOrder},
    },
};
use powdr_number::FieldElement;

use super::merged_machines::MergedMachines;

pub(crate) fn get_namespace(name: &str) -> String {
    let mut namespace = AbsoluteSymbolPath::default().join(SymbolPath::from_str(name).unwrap());
    namespace.pop().unwrap();
    namespace.relative_to(&Default::default()).to_string()
}

fn referenced_namespaces<F: FieldElement>(
    identity: &Identity<AlgebraicExpression<F>>,
) -> BTreeSet<String> {
    let mut namespaces = BTreeSet::new();
    identity.visit_expressions(
        &mut (|expr| {
            match expr {
                AlgebraicExpression::Reference(reference) => {
                    namespaces.insert(get_namespace(&reference.name));
                }
                AlgebraicExpression::PublicReference(_) => unimplemented!(),
                AlgebraicExpression::Challenge(_) => {}
                AlgebraicExpression::Number(_) => {}
                AlgebraicExpression::BinaryOperation(_) => {}
                AlgebraicExpression::UnaryOperation(_) => {}
            }
            ControlFlow::Continue::<()>(())
        }),
        VisitOrder::Pre,
    );

    namespaces
}

pub(crate) fn split_pil<F: FieldElement>(pil: Analyzed<F>) -> BTreeMap<String, Analyzed<F>> {
    let mut current_namespace = String::new();

    let mut statements_by_namespace: BTreeMap<String, Vec<StatementIdentifier>> = BTreeMap::new();
    let mut merged_machines = MergedMachines::new();
    for statement in pil.source_order.clone() {
        let statement = match &statement {
            StatementIdentifier::Definition(name)
            | StatementIdentifier::PublicDeclaration(name) => {
                let new_namespace = get_namespace(name);
                current_namespace = new_namespace;
                Some(statement)
            }
            StatementIdentifier::Identity(i) => {
                let identity = &pil.identities[*i];
                let namespaces = referenced_namespaces(identity);

                match namespaces.len() {
                    0 => panic!("Identity references no namespace: {identity}"),
                    1 => {
                        assert!(namespaces.iter().next().unwrap() == &current_namespace);
                        Some(statement)
                    }
                    _ => match identity.kind {
                        IdentityKind::Plookup | IdentityKind::Permutation => {
                            log::debug!("Skipping connecting identity: {identity}");
                            None
                        }
                        _ => {
                            log::debug!("Identity references multiple namespaces: {identity}");
                            log::debug!("=> Merging namespaces: {:?}", namespaces);
                            let mut namespace_iter = namespaces.into_iter();
                            let first_namespace = namespace_iter.next().unwrap();
                            for namespace in namespace_iter {
                                merged_machines.merge(first_namespace.clone(), namespace);
                            }
                            Some(statement)
                        }
                    },
                }
            }
        };

        if let Some(statement) = statement {
            statements_by_namespace
                .entry(current_namespace.clone())
                .or_default()
                .push(statement);
        }
    }

    let merged_machines = merged_machines.merged_machines();
    let namespace_to_machine_name = merged_machines
        .into_iter()
        .flat_map(|machines| {
            let machine_name = machines.clone().into_iter().collect::<Vec<_>>().join("_");
            machines
                .into_iter()
                .map(move |machine| (machine, machine_name.clone()))
        })
        .collect::<BTreeMap<_, _>>();
    let mut statements_by_machine: BTreeMap<String, Vec<StatementIdentifier>> = BTreeMap::new();
    for (namespace, statements) in statements_by_namespace {
        let machine_name = namespace_to_machine_name
            .get(&namespace)
            .unwrap_or(&namespace)
            .clone();
        statements_by_machine
            .entry(machine_name)
            .or_default()
            .extend(statements);
    }

    statements_by_machine
        .into_iter()
        .filter_map(|(machine_name, statements)| {
            // HACK: Replace unreferenced identities with 0 = 0
            let referenced_identities = statements
                .iter()
                .filter_map(|statement| match statement {
                    StatementIdentifier::Identity(i) => Some(*i as u64),
                    _ => None,
                })
                .collect::<BTreeSet<_>>();
            if referenced_identities.is_empty() {
                // This can happen if a hint references some std module,
                // but the module is empty.
                return None;
            }
            let identities = pil
                .identities
                .iter()
                .enumerate()
                .map(|(identity_index, identity)| {
                    if referenced_identities.contains(&(identity_index as u64)) {
                        identity.clone()
                    } else {
                        Identity::from_polynomial_identity(
                            identity.id,
                            identity.source.clone(),
                            AlgebraicExpression::Number(F::zero()),
                        )
                    }
                })
                .collect();

            let pil = Analyzed {
                source_order: statements,
                identities,
                ..pil.clone()
            };

            // TODO: Reference issue
            // let parsed_string = powdr_parser::parse(None, &pil.to_string()).unwrap();
            // let pil = powdr_pil_analyzer::analyze_ast(parsed_string);

            Some((machine_name.to_string(), pil))
        })
        .collect()
}

pub(crate) fn select_machine_columns<F: FieldElement>(
    witness: &[(String, Vec<F>)],
    machine: &str,
) -> Vec<(String, Vec<F>)> {
    witness
        .iter()
        .filter(|(name, _)| get_namespace(name) == machine)
        .cloned()
        .collect::<Vec<_>>()
}
