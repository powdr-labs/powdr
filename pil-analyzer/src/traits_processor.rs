use core::panic;
use std::collections::HashMap;

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

pub fn traits_resolution(
    implementations: &HashMap<String, Vec<TraitImplementation<Expression>>>,
    definitions: &mut HashMap<String, (Symbol, Option<FunctionValueDefinition>)>,
) {
    let mut updates = Vec::new();

    for (name, def) in definitions.iter() {
        if let Some(FunctionValueDefinition::Expression(TypedExpression {
            e:
                Expression::FunctionCall(
                    _,
                    FunctionCall {
                        function,
                        arguments: _,
                        ..
                    },
                ),
            ..
        })) = &def.1
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
                let (trait_name, fname) = split_trait_and_function(fname);
                if let (Some(impls), Some(trait_decl)) = (
                    implementations.get(&trait_name),
                    definitions.get(&trait_name).and_then(|d| d.1.as_ref()),
                ) {
                    let new_resolved_impl = unify_impls(&fname, impls, trait_decl);
                    updates.push((name.clone(), new_resolved_impl));
                }
            }
        }
    }

    // for (name, new_resolved_impl) in updates {
    //     if let Some(FunctionValueDefinition::Expression(TypedExpression {
    //         e: Expression::FunctionCall(_, FunctionCall { resolved_impl, .. }),
    //         ..
    //     })) = &mut definitions.get_mut(&name).unwrap().1
    //     {
    //         *resolved_impl = new_resolved_impl;
    //     }
    // }
}

fn split_trait_and_function(full_name: &str) -> (String, String) {
    // TODO GZ: we probably have a better way to do this
    let mut parts: Vec<&str> = full_name.rsplitn(2, '.').collect();
    let trait_name = parts.pop().unwrap_or("").to_string();
    let fname = parts.pop().unwrap_or("").to_string();
    (trait_name, fname)
}

fn unify_impls(
    fname: &str,
    impls: &[TraitImplementation<Expression>],
    trait_decl: &FunctionValueDefinition,
) -> Option<Box<Expression>> {
    let FunctionValueDefinition::TraitDeclaration(TraitDeclaration {
        name: _,
        type_vars,
        functions: trait_functions,
    }) = trait_decl
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

        let (function_impl, function_decl) = match (
            impl_functions.iter().find(|f| f.name == fname),
            trait_functions.iter().find(|f| f.name == fname),
        ) {
            (Some(impl_f), Some(decl_f)) => (impl_f, decl_f),
            _ => continue,
        };

        let TypeScheme {
            vars: _,
            ty: Type::Tuple(TupleType { items }),
        } = type_scheme
        else {
            panic!("Expected type scheme");
        };

        let type_args: HashMap<_, _> = type_vars
            .iter()
            .cloned()
            .zip(items.iter().cloned())
            .collect();

        let NamedExpression { name: _, body } = function_impl;
        let TraitFunction {
            name: _,
            ty: mut decl_ty,
        } = function_decl.clone();
        decl_ty.substitute_type_vars(&type_args);

        // infer type from body?
        match Unifier::new().unify_types(decl_ty.clone(), type_scheme.ty.clone()) {
            Ok(_) => {
                return Some(body.clone());
            }
            Err(err) => {
                panic!("Error: {err}");
            }
        };
    }

    None
}
