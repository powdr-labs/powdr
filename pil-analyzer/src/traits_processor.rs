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

        for name in keys {
            self.resolve_trait(&name);
        }

        self.resolve_trait_identities();
    }

    fn resolve_trait_identities(&mut self) {
        let mut updates = Vec::new();
        for (index, identity) in self.identities.iter().enumerate() {
            let refs_in_identity = self.collect_refs_in_identity(identity);
            for collected_ref in refs_in_identity {
                let (trait_name, resolved_impl_pos) =
                    self.resolve_trait_function(collected_ref.clone());
                updates.push((index, trait_name, collected_ref.1, resolved_impl_pos));
            }
        }

        for (index, trait_name, type_args, resolved_impl_pos) in updates {
            let identity = &mut self.identities[index];
            TraitsProcessor::update_references_in_identity(
                identity,
                &trait_name,
                &type_args,
                &resolved_impl_pos,
            );
        }
    }

    fn collect_refs_in_identity(
        &self,
        identity: &Identity<SelectedExpressions<Expression>>,
    ) -> Vec<(String, Vec<Type>)> {
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
            .collect()
    }

    fn update_references_in_identity(
        identity: &mut Identity<SelectedExpressions<Expression>>,
        trait_name: &str,
        type_args: &[Type],
        resolved_impl_pos: &HashMap<String, Box<Expression>>,
    ) {
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

        let to_update = selector_left
            .iter_mut()
            .chain(std::iter::once(expressions_left.as_mut()))
            .chain(selector_right.iter_mut())
            .chain(std::iter::once(expressions_right.as_mut()));

        for expr in to_update {
            update_reference(trait_name, type_args, expr, resolved_impl_pos);
        }
    }

    fn resolve_trait(&mut self, current: &str) {
        let current_def = &self.definitions.get(current).unwrap().1;
        let refs_in_def = self.collect_refs_in_def(current_def);

        for collected_ref in refs_in_def {
            let (trait_name, resolved_impl_pos) =
                self.resolve_trait_function(collected_ref.clone());

            if let Some(FunctionValueDefinition::Expression(TypedExpression { e: expr, .. })) =
                self.definitions.get_mut(current).unwrap().1.as_mut()
            {
                update_reference(&trait_name, &collected_ref.1, expr, &resolved_impl_pos);
            }
        }
    }

    fn collect_refs_in_def(
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
    ) -> (String, HashMap<String, Box<Expression>>) {
        let mut resolved_impl_pos = HashMap::new();
        let Some(FunctionValueDefinition::TraitFunction(ref trait_decl, ref trait_fn)) =
            self.definitions.get(&collected_ref.0).unwrap().1
        else {
            return ("".to_string(), resolved_impl_pos);
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
            format!("{}::{}", trait_decl.name.replace('.', "::"), trait_fn.name),
            resolved_impl_pos,
        )
    }
}

fn update_reference(
    ref_name: &str,
    type_args: &[Type],
    expr: &mut Expression,
    resolved_impl_pos: &HashMap<String, Box<Expression>>,
) {
    fn process_expr(
        ref_name: &str,
        type_args: &[Type],
        c: &mut Expression,
        resolved_impl_pos: &HashMap<String, Box<Expression>>,
    ) {
        if let Expression::Reference(
            sr,
            Reference::Poly(PolynomialReference {
                name,
                poly_id,
                type_args: current_type_args,
                ..
            }),
        ) = c
        {
            if name == ref_name {
                *c = Expression::Reference(
                    sr.clone(),
                    Reference::Poly(PolynomialReference {
                        name: name.clone(),
                        type_args: current_type_args.clone(),
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

    process_expr(ref_name, type_args, expr, resolved_impl_pos);
}
