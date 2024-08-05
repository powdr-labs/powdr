use std::collections::HashMap;

use powdr_ast::{
    analyzed::{Expression, FunctionValueDefinition, Symbol},
    parsed::{
        types::{TupleType, Type},
        TraitDeclaration, TraitImplementation,
    },
};

use crate::type_unifier::Unifier;

/// Checks for overlapping trait implementations in the current `PILAnalyzer` instance.
///
/// This method iterates through all the trait implementations.
/// For each implementation, it checks that there are no traits with overlapping type vars and the same name between them.
/// Overlapping occurs when there are two implementations of the same trait whose trait types can be unified.
///
/// It also checks that the number of type variables in the implementation matches
/// the number of type variables in the corresponding trait declaration.
pub fn check_traits_overlap(
    implementations: &HashMap<String, Vec<TraitImplementation<Expression>>>,
    definitions: &HashMap<String, (Symbol, Option<FunctionValueDefinition>)>,
) {
    for trait_impls in implementations.values() {
        // All the impls in trait_impls are of the same trait declaration.
        let trait_name = trait_impls[0].name.clone();
        let trait_decl = definitions
            .get(&trait_name)
            .unwrap_or_else(|| panic!("Trait {trait_name} not found"))
            .1
            .as_ref()
            .unwrap_or_else(|| panic!("Trait definition for {trait_name} not found"));

        let trait_decl = if let FunctionValueDefinition::TraitDeclaration(trait_decl) = trait_decl {
            trait_decl
        } else {
            panic!("Invalid trait declaration");
        };

        validate_impl_definitions(trait_impls, trait_decl);
        ensure_unique_impls(trait_impls);
    }
}

/// Validates the trait implementation definitions in the given `implementations` map against the trait
/// declarations in the `definitions` map.
fn validate_impl_definitions(
    implementations: &[TraitImplementation<Expression>],
    trait_decl: &TraitDeclaration,
) {
    for trait_impl in implementations {
        let Type::Tuple(TupleType { items: types }) = &trait_impl.type_scheme.ty else {
            panic!("Type from trait scheme is not a tuple.")
        };
        let trait_name = trait_impl.name.clone();

        if types.len() != trait_decl.type_vars.len() {
            panic!(
                "{}",
                trait_impl.source_ref.with_error(format!(
                    "Trait {} has {} type parameters, but implementation has {}",
                    trait_name,
                    trait_decl.type_vars.len(),
                    types.len(),
                ))
            );
        }

        let type_vars_in_tuple: Vec<_> =
            types.iter().flat_map(|t| t.contained_type_vars()).collect();

        let type_vars_in_scheme: Vec<_> = trait_impl.type_scheme.ty.contained_type_vars().collect();

        for var in type_vars_in_scheme {
            if !type_vars_in_tuple.contains(&var) {
                panic!(
                    "{}",
                    trait_impl.source_ref.with_error(format!(
                        "Impl {trait_name} introduces a type variable {var} that is not used",
                    ))
                );
            }
        }
    }
}

/// Ensures that there are no overlapping trait implementations in the given `implementations` map.
///
/// This function iterates through all the trait implementations comparing them with each other and checks that
/// there are no traits with overlapping type variables.
fn ensure_unique_impls(implementations: &[TraitImplementation<Expression>]) {
    for (i, impl1) in implementations.iter().enumerate() {
        for impl2 in implementations.iter().skip(i + 1) {
            unify_traits_types(impl1.type_scheme.ty.clone(), impl2.type_scheme.ty.clone())
                .map_err(|err| {
                    impl1
                        .source_ref
                        .with_error(format!("Impls for {}: {err}", impl1.name))
                })
                .unwrap()
        }
    }
}

fn unify_traits_types(ty1: Type, ty2: Type) -> Result<(), String> {
    match Unifier::default().unify_types(ty1.clone(), ty2.clone()) {
        Ok(_) => Err(format!("Types {ty1} and {ty2} overlap")),
        Err(_) => Ok(()),
    }
}
