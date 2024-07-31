use std::collections::HashMap;

use powdr_ast::{
    analyzed::{Expression, FunctionValueDefinition, Symbol},
    parsed::{
        types::{TupleType, Type},
        TraitImplementation,
    },
};

use crate::{pil_analyzer::Driver, type_unifier::Unifier, AnalysisDriver};

/// Checks for overlapping trait implementations in the current `PILAnalyzer` instance.
///
/// This method iterates through all the trait implementations.
/// For each implementation, it checks that there are no traits with overlapping type vars and the same name between them.
/// It also checks that the number of type variables in the implementation matches
/// the number of type variables in the corresponding trait declaration.
pub fn check_traits_overlap(
    implementations: &HashMap<String, Vec<TraitImplementation<Expression>>>,
    definitions: &HashMap<String, (Symbol, Option<FunctionValueDefinition>)>,
    driver: Driver,
) {
    validate_impl_definitions(implementations, definitions, driver);
    ensure_unique_impls(implementations, driver);
}

/// Validates the trait implementation definitions in the given `implementations` map against the trait
/// declarations in the `definitions` map.
fn validate_impl_definitions(
    implementations: &HashMap<String, Vec<TraitImplementation<Expression>>>,
    definitions: &HashMap<String, (Symbol, Option<FunctionValueDefinition>)>,
    driver: Driver,
) {
    for impls in implementations.values() {
        for trait_impl in impls.iter() {
            let Type::Tuple(TupleType { items: types }) = &trait_impl.type_scheme.ty else {
                panic!("Type from trait scheme is not a tuple.")
            };
            let absolute_name = driver.resolve_decl(trait_impl.name.name());

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

            if types.len() != trait_decl.type_vars.len() {
                panic!(
                    "{}",
                    trait_impl.source_ref.with_error(format!(
                        "Trait {} has {} type vars, but implementation has {}",
                        absolute_name,
                        trait_decl.type_vars.len(),
                        types.len(),
                    ))
                );
            }

            for var in trait_impl.type_scheme.vars.vars() {
                if !types
                    .iter()
                    .any(|t| matches!(t, Type::NamedType(v, _) if v.name() == var))
                {
                    panic!(
                        "{}",
                        trait_impl.source_ref.with_error(format!(
                            "Impl {absolute_name} has a type var {var} that is not defined in the type tuple",
                        ))
                    );
                }
            }
        }
    }
}

/// Ensures that there are no overlapping trait implementations in the given `implementations` map.
///
/// This function iterates through all the trait implementations comparing them with each other and checks that:
/// - The number of type variables between implementations is the same.
/// - There are no traits with overlapping type variables and the same name between the implementations.
fn ensure_unique_impls(
    implementations: &HashMap<String, Vec<TraitImplementation<Expression>>>,
    driver: Driver,
) {
    for implementations in implementations.values() {
        for (i, impl1) in implementations.iter().enumerate() {
            for impl2 in implementations.iter().skip(i + 1) {
                let Type::Tuple(TupleType { items: types1 }) = &impl1.type_scheme.ty else {
                    panic!("Type from trait scheme is not a tuple.")
                };
                let Type::Tuple(TupleType { items: types2 }) = &impl2.type_scheme.ty else {
                    panic!("Type from trait scheme is not a tuple.")
                };

                if types1.len() != types2.len() {
                    panic!(
                        "{}",
                        impl1.source_ref.with_error(format!(
                            "Impl types have different lengths: {} with {} vs {} with {}",
                            driver.resolve_decl(impl1.name.name()),
                            types1.len(),
                            driver.resolve_decl(impl2.name.name()),
                            types2.len()
                        ))
                    );
                }

                unify_traits_types(impl1.type_scheme.ty.clone(), impl2.type_scheme.ty.clone())
                    .map_err(|err| {
                        impl1.source_ref.with_error(format!(
                            "Impls for {}: {err}",
                            driver.resolve_decl(impl1.name.name())
                        ))
                    })
                    .unwrap()
            }
        }
    }
}

fn unify_traits_types(ty1: Type, ty2: Type) -> Result<(), String> {
    match Unifier::default().unify_types(ty1.clone(), ty2.clone()) {
        Ok(_) => Err(format!("Types {ty1} and {ty2} overlap")),
        Err(_) => Ok(()),
    }
}
