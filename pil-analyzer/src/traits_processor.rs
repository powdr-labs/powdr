use core::panic;
use std::collections::HashMap;

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
    identities: &'a mut Vec<Identity<SelectedExpressions<Expression>>>,
    implementations: &'a HashMap<String, Vec<TraitImplementation<Expression>>>,
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
        }

        let refs_in_identity: Vec<(String, Vec<Type>)> = self
            .identities
            .iter()
            .flat_map(|id| {
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
                } = id;

                selector_left
                    .iter()
                    .chain(std::iter::once(expressions_left.as_ref()))
                    .chain(selector_right.iter())
                    .chain(std::iter::once(expressions_right.as_ref()))
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
                    .collect::<Vec<_>>()
            })
            .collect();
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

        for collected_ref in refs_in_def {
            let (trait_name, resolved_impl_pos) =
                self.resolve_trait_function(collected_ref.clone());

            let to_update = &mut self
                .definitions
                .get_mut(current)
                .unwrap()
                .1
                .as_mut()
                .unwrap();

            if let FunctionValueDefinition::Expression(TypedExpression { e: expr, .. }) = to_update
            {
                update_reference(&trait_name, &collected_ref.1, expr, &resolved_impl_pos);
            } else {
                unreachable!();
            }
        }
    }

    fn resolve_trait_function(
        &mut self,
        collected_ref: (String, Vec<Type>),
    ) -> (String, HashMap<String, Box<Expression>>) {
        let mut resolved_impl_pos = HashMap::new();
        let Some(FunctionValueDefinition::TraitFunction(ref trait_decl, ref mut trait_fn)) =
            self.definitions.get_mut(&collected_ref.0).unwrap().1
        else {
            return ("".to_string(), resolved_impl_pos); //TODO GZ: Error
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

        (
            format!("{}::{}", trait_decl.name.replace('.', "::"), trait_fn.name), // TODO GZ: we probably have a better way to do this
            resolved_impl_pos,
        )
    }
}

fn update_reference(
    ref_name: &str,
    type_args: &Vec<Type>,
    expr: &mut Expression,
    resolved_impl_pos: &HashMap<String, Box<Expression>>,
) {
    fn process_expr(
        ref_name: &str,
        type_args: &Vec<Type>,
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
                        poly_id: poly_id.clone(),
                        resolved_impls: resolved_impl_pos.clone(),
                    }),
                );
            }
        }

        for child in c.children_mut() {
            process_expr(ref_name, type_args, child, resolved_impl_pos);
        }
    }

    for c in expr.children_mut() {
        process_expr(ref_name, type_args, c, resolved_impl_pos);
    }
}
