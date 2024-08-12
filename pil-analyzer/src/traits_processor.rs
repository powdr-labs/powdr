use core::panic;
use std::{collections::HashMap, iter::once};

use itertools::Itertools;
use powdr_ast::{
    analyzed::{
        Expression, FunctionValueDefinition, Identity, PolynomialReference, Reference, Symbol,
        TypedExpression,
    },
    parsed::{types::Type, visitor::AllChildren, SelectedExpressions, TraitImplementation},
};

pub struct TraitsProcessor<'a> {
    definitions: &'a mut HashMap<String, (Symbol, Option<FunctionValueDefinition>)>,
    implementations: &'a HashMap<String, Vec<TraitImplementation<Expression>>>,
}

impl<'a> TraitsProcessor<'a> {
    pub fn new(
        definitions: &'a mut HashMap<String, (Symbol, Option<FunctionValueDefinition>)>,
        implementations: &'a HashMap<String, Vec<TraitImplementation<Expression>>>,
    ) -> Self {
        Self {
            definitions,
            implementations,
        }
    }

    pub fn traits_resolution(
        &mut self,
        identities: &mut [Identity<SelectedExpressions<Expression>>],
    ) {
        let keys: Vec<String> = self
            .definitions
            .iter()
            .filter(|(_, (_, def))| {
                !matches!(
                    def,
                    None | Some(FunctionValueDefinition::TraitFunction(_, _))
                )
            })
            .map(|(name, _)| name.clone())
            .collect();

        let refs_in_identities = self.collect_refs_in_identities(identities);

        for name in keys {
            self.resolve_trait(&name, &refs_in_identities);
        }
    }

    fn collect_refs_in_identities(
        &self,
        identities: &mut [Identity<SelectedExpressions<Expression>>],
    ) -> Vec<(String, Vec<Type>)> {
        identities
            .iter()
            .flat_map(|identity| {
                let Identity {
                    left:
                        SelectedExpressions {
                            selector: selector_left,
                            expressions: expressions_left,
                        },
                    right:
                        SelectedExpressions {
                            selector: selector_right,
                            expressions: expressions_right,
                        },
                    ..
                } = identity;

                selector_left
                    .iter()
                    .chain(once(expressions_left.as_ref()))
                    .chain(selector_right.iter())
                    .chain(once(expressions_right.as_ref()))
                    .flat_map(move |e| {
                        e.all_children().filter_map(move |e| match e {
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
                    })
            })
            .collect()
    }

    fn resolve_trait(&mut self, current: &str, refs_in_identities: &[(String, Vec<Type>)]) {
        let current_def = &self.definitions.get(current).unwrap().1;
        let refs_in_def = self.collect_refs(current_def);
        let refs = refs_in_def.iter().chain(refs_in_identities.iter());

        for collected_ref in refs {
            let Some((trait_name, resolved_impl_pos)) =
                self.resolve_trait_function(collected_ref.clone())
            else {
                continue;
            };

            if let Some(FunctionValueDefinition::Expression(TypedExpression { e: expr, .. })) =
                self.definitions.get_mut(current).unwrap().1.as_mut()
            {
                update_reference(&trait_name, expr, &resolved_impl_pos);
            }
        }
    }

    fn collect_refs(
        &self,
        current_def: &Option<FunctionValueDefinition>,
    ) -> Vec<(String, Vec<Type>)> {
        match current_def {
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
        }
    }

    fn resolve_trait_function(
        &self,
        collected_ref: (String, Vec<Type>),
    ) -> Option<(String, HashMap<String, Box<Expression>>)> {
        let mut resolved_impl_pos = HashMap::new();
        let Some(FunctionValueDefinition::TraitFunction(ref trait_decl, ref trait_fn)) =
            self.definitions.get(&collected_ref.0).unwrap().1
        else {
            return None;
        };

        if let Some(impls) = self.implementations.get(&trait_decl.name) {
            for impl_ in impls.iter() {
                let Some(impl_fn) = impl_.function_by_name(&trait_fn.name) else {
                    panic!(
                        "Could not find function {} for {}",
                        trait_fn.name, trait_decl.name
                    );
                };

                resolved_impl_pos.insert(
                    collected_ref.1.iter().format(",").to_string(),
                    impl_fn.body.clone(),
                );
            }
        }

        Some((
            format!("{}::{}", trait_decl.name.replace('.', "::"), trait_fn.name),
            resolved_impl_pos,
        ))
    }
}

fn update_reference(
    ref_name: &str,
    expr: &mut Expression,
    resolved_impl_pos: &HashMap<String, Box<Expression>>,
) {
    fn process_expr(
        ref_name: &str,
        c: &mut Expression,
        resolved_impl_pos: &HashMap<String, Box<Expression>>,
    ) {
        if let Expression::Reference(
            sr,
            Reference::Poly(PolynomialReference {
                name,
                poly_id,
                type_args,
                ..
            }),
        ) = c
        {
            if name == ref_name {
                *c = Expression::Reference(
                    sr.clone(),
                    Reference::Poly(PolynomialReference {
                        name: name.clone(),
                        type_args: type_args.clone(),
                        poly_id: *poly_id,
                        resolved_impls: resolved_impl_pos.clone(),
                    }),
                );
            }
        }

        for child in c.children_mut() {
            process_expr(ref_name, child, resolved_impl_pos);
        }
    }

    process_expr(ref_name, expr, resolved_impl_pos);
}
