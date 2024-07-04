use std::{
    collections::{BTreeMap, BTreeSet},
    ops::ControlFlow,
    str::FromStr,
};

use powdr_ast::{
    analyzed::{AlgebraicExpression, Analyzed, IdentityKind, StatementIdentifier, Symbol},
    parsed::{
        asm::{AbsoluteSymbolPath, SymbolPath},
        visitor::{ExpressionVisitable, VisitOrder},
    },
};
use powdr_number::FieldElement;

/// Splits a PIL into multiple PILs, one for each "machine".
/// The rough algorithm is as follows:
/// 1. The PIL is split into namespaces
/// 2. Any lookups or permutations that reference multiple namespaces are removed.
pub(crate) fn split_pil<F: FieldElement>(pil: Analyzed<F>) -> BTreeMap<String, Analyzed<F>> {
    let statements_by_machine = split_by_namespace(&pil);

    // Merge std and empty namespaces into the other namespaces
    let (std_statements, statements_by_machine) = statements_by_machine
        .into_iter()
        .partition::<BTreeMap<_, _>, _>(|(namespace, _)| {
            namespace.starts_with("std::") || namespace.is_empty()
        });
    let std_statements = std_statements
        .into_iter()
        .flat_map(|(_, statements)| statements)
        .collect::<Vec<_>>();
    let statements_by_machine = statements_by_machine
        .into_iter()
        .map(|(namespace, mut statements)| {
            statements.extend(std_statements.iter().cloned());
            (namespace, statements)
        })
        .collect::<BTreeMap<_, _>>();

    statements_by_machine
        .into_iter()
        .map(|(machine_name, statements)| {
            (machine_name, build_machine_pil(pil.clone(), statements))
        })
        .collect()
}

/// Given a set of columns and a set of polynomial symbols, returns the columns that correspond to the symbols.
pub(crate) fn select_machine_columns<'a, F: FieldElement>(
    columns: &[(String, Vec<F>)],
    symbols: impl Iterator<Item = &'a Symbol>,
) -> Vec<(String, Vec<F>)> {
    let names = symbols
        .flat_map(|symbol| symbol.array_elements().map(|(name, _)| name))
        .collect::<BTreeSet<_>>();
    columns
        .iter()
        .filter(|(name, _)| names.contains(name))
        .cloned()
        .collect::<Vec<_>>()
}

/// From a symbol name, get the namespace of the symbol.
fn extract_namespace(name: &str) -> String {
    let mut namespace = AbsoluteSymbolPath::default().join(SymbolPath::from_str(name).unwrap());
    namespace.pop().unwrap();
    namespace.relative_to(&Default::default()).to_string()
}

/// From an identity, get the namespaces of the symbols it references.
fn referenced_namespaces<F: FieldElement>(
    expression_visitable: &impl ExpressionVisitable<AlgebraicExpression<F>>,
) -> BTreeSet<String> {
    let mut namespaces = BTreeSet::new();
    expression_visitable.visit_expressions(
        &mut (|expr| {
            match expr {
                AlgebraicExpression::Reference(reference) => {
                    namespaces.insert(extract_namespace(&reference.name));
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

/// Organizes the PIL statements by namespace:
/// - Any definition or public declaration belongs to the namespace of the symbol.
/// - Lookups and permutations that reference multiple namespaces removed.
///
/// Returns:
/// - statements_by_namespace: A map from namespace to the statements in that namespace.
fn split_by_namespace<F: FieldElement>(
    pil: &Analyzed<F>,
) -> BTreeMap<String, Vec<StatementIdentifier>> {
    pil.source_order
        .iter()
        // split, filtering out some statements
        .filter_map(|statement| match &statement {
            StatementIdentifier::Definition(name)
            | StatementIdentifier::PublicDeclaration(name) => {
                let namespace = extract_namespace(name);
                // add `statement` to `namespace`
                Some((namespace, statement))
            }
            StatementIdentifier::Identity(i) => {
                let identity = &pil.identities[*i];
                let namespaces = referenced_namespaces(identity);

                match namespaces.len() {
                    0 => panic!("Identity references no namespace: {identity}"),
                    // add this identity to the only referenced namespace
                    1 => Some((namespaces.into_iter().next().unwrap(), statement)),
                    _ => match identity.kind {
                        IdentityKind::Plookup | IdentityKind::Permutation => {
                            assert_eq!(
                                referenced_namespaces(&identity.left).len(),
                                1,
                                "LHS of identity references multiple namespaces: {identity}"
                            );
                            assert_eq!(
                                referenced_namespaces(&identity.right).len(),
                                1,
                                "RHS of identity references multiple namespaces: {identity}"
                            );
                            log::debug!("Skipping connecting identity: {identity}");
                            None
                        }
                        _ => {
                            panic!("Identity references multiple namespaces: {identity}");
                        }
                    },
                }
            }
        })
        // collect into a map
        .fold(Default::default(), |mut acc, (namespace, statement)| {
            acc.entry(namespace).or_default().push(statement.clone());
            acc
        })
}

/// Given a PIL and a list of statements, returns a new PIL that only contains the
/// given subset of statements.
/// Returns None if there are no identities in the subset of statements.
fn build_machine_pil<F: FieldElement>(
    pil: Analyzed<F>,
    statements: Vec<StatementIdentifier>,
) -> Analyzed<F> {
    let pil = Analyzed {
        source_order: statements,
        ..pil
    };
    let parsed_string = powdr_parser::parse(None, &pil.to_string()).unwrap();
    powdr_pil_analyzer::analyze_ast(parsed_string)
}
