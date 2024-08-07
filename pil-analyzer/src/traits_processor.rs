use core::panic;
use std::collections::HashMap;

use powdr_ast::{
    analyzed::{
        Expression, FunctionValueDefinition, PolynomialReference, Reference, Symbol,
        TypedExpression,
    },
    parsed::{
        types::{FunctionType, TupleType, Type, TypeScheme},
        FunctionCall, Number, TraitDeclaration, TraitFunction, TraitImplementation,
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
                        arguments,
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
                    let new_resolved_impl = unify_impls(&fname, arguments, impls, trait_decl);
                    updates.push((name.clone(), new_resolved_impl));
                }
            }
        }
    }

    for (name, new_resolved_impl) in updates {
        if let Some(FunctionValueDefinition::Expression(TypedExpression {
            e: Expression::Reference(_, Reference::Poly(reference)),
            ..
        })) = &mut definitions.get_mut(&name).unwrap().1
        {
            reference.resolved_impl = new_resolved_impl;
        }
    }
}

fn derive_type(type_scheme: &TypeScheme, arguments: &[Expression]) -> Type {
    if arguments.is_empty() {
        Type::Function(FunctionType {
            params: vec![],
            value: Box::new(type_scheme.ty.clone()),
        })
    } else {
        let mut params = vec![];
        for arg in arguments.iter() {
            let arg_type = match arg {
                Expression::Number(_, Number { type_, .. }) => type_.clone(),
                _ => panic!("Expected number"),
            };
            params.push(arg_type.unwrap());
        }
        Type::Function(FunctionType {
            params,
            value: Box::new(type_scheme.ty.clone()),
        })
    }
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
    arguments: &[Expression],
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
            type_scheme:
                TypeScheme {
                    vars: _,
                    ty: Type::Tuple(TupleType { items }),
                },
            functions: impl_functions,
        } = i
        else {
            panic!("Invalid trait implementation");
        };

        let (function_impl, function_decl) = match (
            impl_functions.iter().find(|f| f.name == fname),
            trait_functions.iter().find(|f| f.name == fname),
        ) {
            (Some(impl_f), Some(decl_f)) => (impl_f, decl_f),
            _ => continue,
        };

        let type_args: HashMap<_, _> = type_vars
            .iter()
            .cloned()
            .zip(items.iter().cloned())
            .collect();

        let TraitFunction {
            name: _,
            ty: mut decl_ty,
        } = function_decl.clone();
        decl_ty.substitute_type_vars(&type_args);

        let derived_type = derive_type(&i.type_scheme, arguments);

        match Unifier::new().unify_types(decl_ty.clone(), derived_type) {
            Ok(_) => {
                return Some(function_impl.body.clone());
            }
            Err(err) => {
                panic!("Error: {err}");
            }
        };
    }

    None
}
