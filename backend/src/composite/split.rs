use std::{
    collections::{BTreeMap, BTreeSet},
    ops::ControlFlow,
    str::FromStr,
};

use powdr_ast::analyzed::SelectedExpressions;
use powdr_ast::{
    analyzed::{
        AlgebraicExpression, Analyzed, Identity, IdentityKind, StatementIdentifier, Symbol,
    },
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

    statements_by_machine
        .into_iter()
        .filter_map(|(machine_name, statements)| {
            build_machine_pil(pil.clone(), statements).map(|pil| (machine_name, pil))
        })
        .collect()
}

/// Given a set of columns and a set of symbols, returns the columns that correspond to the symbols.
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
fn get_namespace(name: &str) -> String {
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

/// Organizes the PIL statements by namespace:
/// - Any definition or public declaration belongs to the namespace of the symbol.
/// - Lookups and permutations that reference multiple namespaces removed.
///
/// Returns:
/// - statements_by_namespace: A map from namespace to the statements in that namespace.
fn split_by_namespace<F: FieldElement>(
    pil: &Analyzed<F>,
) -> BTreeMap<String, Vec<StatementIdentifier>> {
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
                let namespaces = referenced_namespaces(identity);

                match namespaces.len() {
                    0 => panic!("Identity references no namespace: {identity}"),
                    1 => {
                        assert!(namespaces.iter().next().unwrap() == &current_namespace);
                        Some(statement)
                    }
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
        };

        if let Some(statement) = statement {
            statements_by_namespace
                .entry(current_namespace.clone())
                .or_default()
                .push(statement);
        }
    }
    statements_by_namespace
}

/// Given a PIL and a list of statements, returns a new PIL that only contains the
/// given subset of statements.
/// Returns None if there are no identities in the subset of statements.
fn build_machine_pil<F: FieldElement>(
    pil: Analyzed<F>,
    statements: Vec<StatementIdentifier>,
) -> Option<Analyzed<F>> {
    // TODO: After #1488 is fixed, we can implement this like so:
    // let pil = Analyzed {
    //     source_order: statements,
    //     ..pil.clone()
    // };
    // let parsed_string = powdr_parser::parse(None, &pil.to_string()).unwrap();
    // let pil = powdr_pil_analyzer::analyze_ast(parsed_string);

    // HACK: Replace unreferenced identities with 0 = 0, to avoid having to re-assign IDs.
    let identities = statements
        .iter()
        .filter_map(|statement| match statement {
            StatementIdentifier::Identity(i) => Some(*i as u64),
            _ => None,
        })
        .collect::<BTreeSet<_>>();
    if identities.is_empty() {
        // This can happen if a hint references some std module,
        // but the module is empty.
        return None;
    }
    let identities = pil
        .identities
        .iter()
        .enumerate()
        .map(|(identity_index, identity)| {
            if identities.contains(&(identity_index as u64)) {
                identity.clone()
            } else {
                Identity::<SelectedExpressions<AlgebraicExpression<F>>>::from_polynomial_identity(
                    identity.id,
                    identity.source.clone(),
                    AlgebraicExpression::Number(F::zero()),
                )
            }
        })
        .collect();

    Some(Analyzed {
        source_order: statements,
        identities,
        ..pil
    })
}
