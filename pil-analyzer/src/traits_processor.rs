use core::panic;
use std::collections::{HashMap, HashSet};

use powdr_ast::{
    analyzed::{
        Expression, FunctionValueDefinition, PolynomialReference, Reference, Symbol,
        TypedExpression,
    },
    parsed::{types::Type, FunctionCall, TraitImplementation},
};

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

    pub fn traits_resolution(&mut self) {
        let keys = self
            .definitions
            .iter()
            .filter(|(_, (_, def))| {
                def.is_some()
                    && !matches!(
                        def.as_ref().unwrap(),
                        FunctionValueDefinition::TraitFunction(_, _)
                    )
            })
            .map(|(name, _)| name.clone())
            .collect::<Vec<_>>();
        for name in keys {
            if !self.visited.contains(&name) {
                self.dfs_traits(&name);

                self.visited.clear();
                self.stack.clear();
                self.type_args_stack.clear();
            }
        }
    }

    fn dfs_traits(&mut self, current: &str) {
        self.visited.insert(current.to_string());
        self.stack.push(current.to_string());

        let next_name = {
            let current_def = &self.definitions.get(current).unwrap().1;
            match current_def {
                Some(FunctionValueDefinition::Expression(TypedExpression {
                    e: Expression::FunctionCall(_, FunctionCall { function, .. }),
                    ..
                })) => match function.as_ref() {
                    Expression::Reference(
                        _,
                        Reference::Poly(PolynomialReference {
                            name,
                            type_args: Some(types),
                            ..
                        }),
                    ) => {
                        self.type_args_stack.push(types.clone());
                        Some(name.clone())
                    }
                    _ => None,
                },
                Some(FunctionValueDefinition::TraitFunction(_, _)) => {
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
        let (trait_name, func_name) = self.split_trait_and_function(name);
        if let Some(impls) = self.implementations.get(&trait_name) {
            let accumulated_type_args = self
                .type_args_stack
                .iter()
                .flatten()
                .cloned()
                .collect::<Vec<_>>();

            let stack_first = self.stack.first().unwrap().clone();
            let def = self
                .definitions
                .get_mut(&stack_first)
                .unwrap()
                .1
                .as_mut()
                .unwrap();

            self.process_function_call(&func_name, def)
        }
    }

    fn process_function_call(&self, func_name: &str, def: &mut FunctionValueDefinition) {
        if let FunctionValueDefinition::Expression(TypedExpression {
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
        }) = def
        {
            if let Some(matched_impl) = self.find_matching_impl(func_name, arguments) {
                match function.as_mut() {
                    Expression::Reference(_, Reference::Poly(reference)) => {
                        reference.resolved_impl = Some(matched_impl);
                    }
                    _ => panic!("Expected a reference"),
                }
            }
        }
    }

    fn split_trait_and_function(&self, full_name: &str) -> (String, String) {
        // TODO GZ: we probably have a better way to do this (SymbolPath insteand of String)
        let mut parts: Vec<&str> = full_name.rsplitn(2, '.').collect();
        let trait_name = parts.pop().unwrap_or("").to_string();
        let fname = parts.pop().unwrap_or("").to_string();
        (trait_name, fname)
    }

    fn find_matching_impl(
        &self,
        func_name: &str,
        arguments: &[Expression],
    ) -> Option<Box<Expression>> {
        None
    }
}
