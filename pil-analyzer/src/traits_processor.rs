use core::panic;
use std::collections::{HashMap, HashSet};

use powdr_ast::{
    analyzed::{
        Expression, FunctionValueDefinition, PolynomialReference, Reference, Symbol,
        TypedExpression,
    },
    parsed::{
        types::{TupleType, Type, TypeScheme},
        FunctionCall, NamedExpression, TraitDeclaration, TraitFunction, TraitImplementation,
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
    implementations: &mut HashMap<String, Vec<TraitImplementation<Expression>>>,
    definitions: &HashMap<String, (Symbol, Option<FunctionValueDefinition>)>,
    unifier: &mut Unifier,
) {
    for trait_impls in implementations.values_mut() {
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
        ensure_unique_impls(trait_impls, unifier);
    }
}

/// Validates the trait implementation definitions in the given `implementations` map against the trait
/// declarations in the `definitions` map.
fn validate_impl_definitions(
    implementations: &[TraitImplementation<Expression>],
    trait_decl: &TraitDeclaration,
) {
    for trait_impl in implementations {
        let Type::Tuple(TupleType { items: mut types }) = trait_impl.type_scheme.ty.clone() else {
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

        let type_vars: HashSet<_> = trait_impl.type_scheme.vars.vars().collect();

        let type_vars_in_tuple: Vec<_> = types
            .iter_mut()
            .flat_map(|t| {
                t.map_to_type_vars(&type_vars);
                t.contained_type_vars()
            })
            .collect();

        let type_vars_in_scheme: Vec<_> = trait_impl.type_scheme.vars.vars().collect();

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
/// This function iterates through all the trait implementations comparing them with each other and ensure that
/// there are no traits with overlapping type variables.
fn ensure_unique_impls(
    implementations: &mut [TraitImplementation<Expression>],
    unifier: &mut Unifier,
) {
    for i in 0..implementations.len() {
        let type_vars: HashSet<_> = implementations[i].type_scheme.vars.vars().collect();
        implementations[i]
            .type_scheme
            .ty
            .map_to_type_vars(&type_vars);

        for j in (i + 1)..implementations.len() {
            let type_vars: HashSet<_> = implementations[j].type_scheme.vars.vars().collect();
            implementations[j]
                .type_scheme
                .ty
                .map_to_type_vars(&type_vars);

            unify_traits_types(
                unifier,
                implementations[i].type_scheme.clone(),
                implementations[j].type_scheme.clone(),
            )
            .map_err(|err| {
                implementations[i]
                    .source_ref
                    .with_error(format!("Impls for {}: {err}", implementations[i].name))
            })
            .unwrap()
        }
    }
}

fn unify_traits_types(
    unifier: &mut Unifier,
    ty1: TypeScheme,
    ty2: TypeScheme,
) -> Result<(), String> {
    let instantiated_ty1 = unifier.instantiate_scheme(ty1).0;
    let instantiated_ty2 = unifier.instantiate_scheme(ty2).0;

    match unifier.unify_types(instantiated_ty1.clone(), instantiated_ty2.clone()) {
        Ok(_) => Err(format!(
            "Types {instantiated_ty1} and {instantiated_ty2} overlap"
        )),
        Err(_) => Ok(()),
    }
}

pub fn traits_resolution(
    definitions: &mut HashMap<String, (Symbol, Option<FunctionValueDefinition>)>,
    implementations: &mut HashMap<String, Vec<TraitImplementation<Expression>>>,
) {
    for (name, def) in definitions.iter_mut() {
        if let Some(FunctionValueDefinition::Expression(TypedExpression { e, type_scheme })) =
            &mut def.1
        {
            if let Expression::FunctionCall(
                _,
                FunctionCall {
                    function,
                    arguments,
                    ref mut resolved_impl,
                },
            ) = e
            {
                if let Expression::Reference(
                    _,
                    Reference::Poly(PolynomialReference {
                        name: fname,
                        type_args,
                        ..
                    }),
                ) = function.as_ref()
                {
                    let mut parts: Vec<&str> = fname.split('.').collect();
                    let fname = parts.pop().unwrap_or("");
                    let trait_name = parts.join(".");
                    let impls = implementations.get(&trait_name).unwrap();
                    let trait_decl = definitions.get(&trait_name).unwrap().1.as_ref().unwrap();

                    *resolved_impl = unify_impls(fname, impls, trait_decl, type_scheme);
                }
            };
        }
    }
}

fn unify_impls(
    fname: &str,
    impls: &[TraitImplementation<Expression>],
    trait_decl: &FunctionValueDefinition,
    ftype_scheme: &Option<TypeScheme>,
) -> Option<TraitImplementation<Expression>> {
    let FunctionValueDefinition::TraitDeclaration(TraitDeclaration {
        name,
        type_vars,
        functions: trait_functions,
    }) = &trait_decl
    else {
        panic!("Expected trait declaration");
    };

    for i in impls {
        let TraitImplementation {
            name: _,
            source_ref: _,
            type_scheme,
            functions: impl_functions,
        } = i;

        let function_impl = impl_functions.iter().find(|f| f.name == fname);
        let function_decl = trait_functions.iter().find(|f| f.name == fname);

        match (function_impl, function_decl) {
            (Some(function_impl), Some(function_decl)) => {
                let NamedExpression { name, body } = function_impl;
                let TraitFunction { name: _, ty } = function_decl;
                println!("Unifying {name} with {ty}");
            }
            _ => panic!("Function {fname} not found"),
        }
    }

    None
}
