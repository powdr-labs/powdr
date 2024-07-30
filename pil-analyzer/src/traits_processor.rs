use std::collections::HashMap;

use powdr_ast::{
    analyzed::{Expression, FunctionValueDefinition, Symbol},
    parsed::{
        types::{TupleType, Type},
        TraitImplementation,
    },
};
use powdr_parser_util::SourceRef;

use crate::{pil_analyzer::Driver, type_inference::unify_traits_types, AnalysisDriver};

/// Checks for overlapping trait implementations in the current `PILAnalyzer` instance.
///
/// This method iterates through all the trait implementations.
/// For each implementation, it checks that there are no traits with overlapping type vars and the same name between them.
/// It also checks that the number of type variables in the implementation matches
/// the number of type variables in the corresponding trait declaration.
pub fn check_traits_overlap(
    implementations: &HashMap<String, Vec<(SourceRef, TraitImplementation<Expression>)>>,
    definitions: &HashMap<String, (Symbol, Option<FunctionValueDefinition>)>,
    driver: Driver,
) {
    for implementations in implementations.values() {
        for (i, (sr1, impl1)) in implementations.iter().enumerate() {
            let Type::Tuple(TupleType { items: types1 }) = &impl1.type_scheme.ty else {
                panic!("Type from trait scheme is not a tuple.")
            };
            let absolute_name = driver.resolve_decl(impl1.name.name());

            let trait_decl = definitions
                .get(&absolute_name)
                .unwrap_or_else(|| panic!("Trait {absolute_name} not found"))
                .1
                .as_ref()
                .unwrap_or_else(|| panic!("Trait definition for {absolute_name} not found"));

            let trait_decl = match trait_decl {
                FunctionValueDefinition::TraitDeclaration(trait_decl) => trait_decl,
                _ => unreachable!("Invalid trait declaration"),
            };

            if types1.len() != trait_decl.type_vars.len() {
                panic!(
                    "{}",
                    sr1.with_error(format!(
                        "Trait {} has {} type vars, but implementation has {}",
                        absolute_name,
                        trait_decl.type_vars.len(),
                        types1.len(),
                    ))
                );
            }

            for (sr2, impl2) in implementations
                .iter()
                .skip(i + 1)
                .filter_map(|stmt2| match stmt2 {
                    (sr2, impl_) if impl_.name == impl1.name => Some((sr2, impl_)),
                    _ => None,
                })
            {
                let Type::Tuple(TupleType { items: types2 }) = &impl2.type_scheme.ty else {
                    panic!("Type from trait scheme is not a tuple.")
                };

                if types2.len() != trait_decl.type_vars.len() {
                    panic!(
                        "{}",
                        sr2.with_error(format!(
                            "Trait {} has {} type vars, but implementation has {}",
                            driver.resolve_decl(impl2.name.name()),
                            trait_decl.type_vars.len(),
                            types2.len(),
                        ))
                    );
                }

                if types1.len() != types2.len() {
                    panic!(
                        "{}",
                        sr1.with_error(format!(
                            "Impl types have different lengths: {} with {} vs {} with {}",
                            driver.resolve_decl(impl1.name.name()),
                            types1.len(),
                            driver.resolve_decl(impl2.name.name()),
                            types2.len()
                        ))
                    );
                }

                unify_traits_types(impl1.type_scheme.ty.clone(), impl2.type_scheme.ty.clone())
                    .map_err(|err| sr1.with_error(format!("Impls for {absolute_name}: {err}")))
                    .unwrap()
            }
        }
    }
}
