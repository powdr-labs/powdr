use core::panic;
use std::collections::{HashMap, HashSet};

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
                Reference::Poly(PolynomialReference { name: fname, .. }),
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
            // infer type of expression insteand?
            let arg_type = match arg {
                Expression::Number(_, Number { type_, .. }) => type_.clone(),
                // missing cases
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
    // TODO GZ: we probably have a better way to do this (SymbolPath insteand of String)
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

        // TODO GZ: type_args for type_scheme?
        let derived_type = derive_type(&i.type_scheme, arguments);

        match Unifier::new().unify_types(decl_ty.clone(), derived_type) {
            Ok(_) => {
                return Some(function_impl.body.clone());
            }
            Err(_) => {
                continue;
            }
        };
    }

    None
}

pub struct TraitsProcessor<'a> {
    definitions: &'a mut HashMap<String, (Symbol, Option<FunctionValueDefinition>)>,
    implementations: &'a HashMap<String, Vec<TraitImplementation<Expression>>>,
    type_args_stack: Vec<Vec<Type>>,
    visited: HashSet<String>,
    stack: Vec<String>,
}

impl<'a> TraitsProcessor<'a> {
    pub fn new(
        definitions: &'a mut HashMap<String, (Symbol, Option<FunctionValueDefinition>)>,
        implementations: &'a HashMap<String, Vec<TraitImplementation<Expression>>>,
    ) -> Self {
        Self {
            definitions,
            implementations,
            type_args_stack: Vec::new(),
            visited: HashSet::new(),
            stack: Vec::new(),
        }
    }

    fn traits_resolution2(&mut self) {
        let keys: Vec<String> = self.definitions.keys().cloned().collect();
        for name in keys {
            if !self.visited.contains(&name) {
                self.dfs_traits(&name);
            }
        }
    }

    fn dfs_traits(&mut self, current: &str) {
        self.visited.insert(current.to_string());
        self.stack.push(current.to_string());

        let next_name = {
            let current_def = self.definitions.get(current);
            match current_def {
                Some((
                    _,
                    Some(FunctionValueDefinition::Expression(TypedExpression {
                        e:
                            Expression::Reference(
                                _,
                                Reference::Poly(PolynomialReference {
                                    name,
                                    type_args: Some(types),
                                    ..
                                }),
                            ),
                        ..
                    })),
                )) => {
                    self.type_args_stack.push(types.clone());
                    Some(name.clone())
                }
                Some((_, Some(FunctionValueDefinition::TraitFunction(_, _)))) => {
                    self.resolve_trait_function(current);
                    None
                }
                _ => {
                    self.stack.pop();
                    if !self.type_args_stack.is_empty() {
                        self.type_args_stack.pop();
                    }
                    None
                }
            }
        };

        if let Some(name) = next_name {
            if !self.visited.contains(&name) {
                self.dfs_traits(&name);
            }
        }
    }

    fn resolve_trait_function(&mut self, name: &str) {
        if let Some(impls) = self.implementations.get(name) {
            let accumulated_type_args = self
                .type_args_stack
                .iter()
                .flatten()
                .cloned()
                .collect::<Vec<_>>();

            let matching_impl = self.find_matching_impl(impls, &accumulated_type_args);

            if let Some(matched_impl) = matching_impl {
                if let Some((_, Some(def))) = self.definitions.get_mut(self.stack.first().unwrap())
                {
                    if let FunctionValueDefinition::Expression(TypedExpression {
                        e: Expression::Reference(_, ref mut poly_ref),
                        ..
                    }) = def
                    {
                        if let Reference::Poly(poly) = poly_ref {
                            poly.resolved_impl = Some(Box::new(matched_impl.clone()));
                        }
                    }
                }
            }
        }

        self.stack.clear();
        self.type_args_stack.clear();
    }

    fn find_matching_impl(
        &self,
        impls: &[TraitImplementation<Expression>],
        type_args: &[Type],
    ) -> Option<Expression> {
        None
    }
}
