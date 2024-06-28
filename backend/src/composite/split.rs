use std::{collections::BTreeMap, ops::ControlFlow, str::FromStr};

use powdr_ast::{
    analyzed::{AlgebraicExpression, Analyzed, Identity, IdentityKind, StatementIdentifier},
    parsed::{
        asm::{AbsoluteSymbolPath, SymbolPath},
        visitor::{ExpressionVisitable, VisitOrder},
    },
};
use powdr_number::FieldElement;

pub(crate) fn get_namespace(name: &str) -> String {
    let mut namespace = AbsoluteSymbolPath::default().join(SymbolPath::from_str(name).unwrap());
    namespace.pop().unwrap();
    namespace.relative_to(&Default::default()).to_string()
}

fn references_other_namespace<F: FieldElement>(
    identity: &Identity<AlgebraicExpression<F>>,
    current_namespace: &str,
) -> bool {
    let mut references_other_namespace = false;
    identity.visit_expressions(
        &mut (|expr| {
            match expr {
                AlgebraicExpression::Reference(reference) => {
                    let namespace = get_namespace(&reference.name);
                    references_other_namespace |=
                        (namespace != current_namespace) && !namespace.starts_with("std::");
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

    references_other_namespace
}

pub(crate) fn split_pil<F: FieldElement>(pil: Analyzed<F>) -> BTreeMap<String, Analyzed<F>> {
    let mut current_namespace = String::new();

    let mut statements_by_namespace: BTreeMap<String, Vec<StatementIdentifier>> = BTreeMap::new();
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
                let references_other_namespace =
                    references_other_namespace(identity, &current_namespace);

                if references_other_namespace {
                    match identity.kind {
                        IdentityKind::Plookup | IdentityKind::Permutation => {}
                        _ => panic!("Identity references other namespace: {identity}"),
                    };
                    None
                } else {
                    Some(statement)
                }
            }
        };

        assert!(!current_namespace.is_empty(), "Namespace not set");

        if let Some(statements) = statement {
            statements_by_namespace
                .entry(current_namespace.clone())
                .or_default()
                .push(statements);
        }
    }

    // Remove namespaces starting the `std::` and add them to all other namespaces
    let (std_namespaces, normal_namespaces): (BTreeMap<_, _>, BTreeMap<_, _>) =
        statements_by_namespace
            .into_iter()
            .partition(|(k, _)| k.starts_with("std::"));
    let statements_by_namespace = normal_namespaces
        .into_iter()
        .map(|(namespace, mut statements)| {
            statements.extend(
                std_namespaces
                    .clone()
                    .into_iter()
                    .flat_map(|(_, statements)| statements),
            );
            (namespace, statements)
        })
        .collect::<BTreeMap<_, _>>();

    statements_by_namespace
        .into_iter()
        .map(|(machine_name, statements)| {
            let pil = Analyzed {
                source_order: statements,
                ..pil.clone()
            };

            let parsed_string = powdr_parser::parse(None, &pil.to_string()).unwrap();
            let pil = powdr_pil_analyzer::analyze_ast(parsed_string);
            (machine_name.to_string(), pil)
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
