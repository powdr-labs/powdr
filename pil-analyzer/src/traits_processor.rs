use core::panic;
use std::collections::HashMap;

use powdr_ast::{
    analyzed::{
        Expression, FunctionValueDefinition, Identity, PolynomialReference, Reference, Symbol,
        TypedExpression,
    },
    parsed::{
        types::Type, visitor::AllChildren, FunctionCall, SelectedExpressions, TraitImplementation,
    },
};

pub struct TraitsProcessor<'a> {
    definitions: &'a mut HashMap<String, (Symbol, Option<FunctionValueDefinition>)>,
    identities: &'a mut Vec<Identity<SelectedExpressions<Expression>>>,
    implementations: &'a HashMap<String, Vec<TraitImplementation<Expression>>>,
    type_args_stack: Vec<(String, Vec<Type>)>,
    stack: Vec<String>,
}

impl<'a> TraitsProcessor<'a> {
    pub fn new(
        definitions: &'a mut HashMap<String, (Symbol, Option<FunctionValueDefinition>)>,
        identities: &'a mut Vec<Identity<SelectedExpressions<Expression>>>,
        implementations: &'a HashMap<String, Vec<TraitImplementation<Expression>>>,
    ) -> Self {
        Self {
            definitions,
            identities,
            implementations,
            type_args_stack: Vec::new(),
            stack: Vec::new(),
        }
    }

    pub fn traits_resolution(&mut self) {
        let keys = self
            .definitions
            .iter()
            .filter(|(_, (_, def))| {
                !matches!(
                    def,
                    None | Some(FunctionValueDefinition::TraitFunction(_, _))
                )
            })
            .map(|(name, _)| name.clone())
            .collect::<Vec<_>>();
        for name in keys {
            self.resolve_trait(&name);

            self.stack.clear();
            self.type_args_stack.clear();
        }

        //for id in self.identities.iter() {
        //
        //}
    }

    fn resolve_trait(&mut self, current: &str) {
        let current_def = &self.definitions.get(current).unwrap().1;
        let refs_in_def = match current_def {
            Some(FunctionValueDefinition::Expression(TypedExpression { e, .. })) => e
                .all_children()
                .filter_map(|e| match e {
                    Expression::Reference(
                        _,
                        Reference::Poly(PolynomialReference {
                            name,
                            type_args: Some(types),
                            ..
                        }),
                    ) => Some((name.clone(), types.clone())),
                    _ => None,
                })
                .collect(),
            _ => vec![],
        };

        println!("refs_in_def: {:?}", refs_in_def);
        if refs_in_def.iter().any(|(name, _)| {
            let def = &self.definitions.get(name).unwrap().1;
            if let Some(FunctionValueDefinition::TraitFunction(_, _)) = def {
                true
            } else {
                false
            }
        }) {
            self.resolve_trait_function2(refs_in_def)
        }
    }

    fn resolve_trait_function2(&mut self, refs_in_def: Vec<(String, Vec<Type>)>) {}

    fn resolve_trait2(&mut self, current: &str) {
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
                        self.type_args_stack.push((name.clone(), types.clone()));
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
            self.resolve_trait(&name);
        }
    }

    fn resolve_trait_function(&mut self, name: &str) {
        let (trait_name, func_name) = self.split_trait_and_function(name);
        if let Some(impls) = self.implementations.get(&trait_name) {
            // let accumulated_type_args = self
            //     .type_args_stack
            //     .iter()
            //     .flatten()
            //     .cloned()
            //     .collect::<Vec<_>>();

            let stack_first = self.stack.first().unwrap().clone();
            let def = self
                .definitions
                .get_mut(&stack_first)
                .unwrap()
                .1
                .as_mut()
                .unwrap();

            //self.process_function_call(&func_name, def, impls); //, &accumulated_type_args);
        }
    }

    fn process_function_call(
        &self,
        func_name: &str,
        def: &mut FunctionValueDefinition,
        impls: &[TraitImplementation<Expression>],
        accumulated_type_args: &[Type],
    ) {
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
            if let Some(matched_impl_pos) = self.find_matching_impl(func_name, arguments) {
                match function.as_mut() {
                    Expression::Reference(_, Reference::Poly(reference)) => {
                        reference.resolved_impl_pos = Some(matched_impl_pos);
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

    fn find_matching_impl(&self, func_name: &str, arguments: &[Expression]) -> Option<usize> {
        None
    }
}
