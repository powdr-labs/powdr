use std::{
    collections::{BTreeMap, HashMap},
    iter::once,
};

use powdr_ast::{
    analyzed::{
        Expression, FunctionValueDefinition, Identity, PolynomialReference, Reference, Symbol,
        TypedExpression,
    },
    parsed::{types::Type, visitor::AllChildren, SelectedExpressions, TraitImplementation},
};

pub struct TraitsResolver<'a> {
    definitions: &'a mut HashMap<String, (Symbol, Option<FunctionValueDefinition>)>,
    identities: &'a mut Vec<Identity<SelectedExpressions<Expression>>>,
}

impl<'a> TraitsResolver<'a> {
    pub fn new(
        definitions: &'a mut HashMap<String, (Symbol, Option<FunctionValueDefinition>)>,
        identities: &'a mut Vec<Identity<SelectedExpressions<Expression>>>,
    ) -> Self {
        Self {
            definitions,
            identities,
        }
    }

    pub fn resolve_traits(
        &'a mut self,
        trait_impls: &HashMap<String, Vec<TraitImplementation<Expression>>>,
    ) {
        let references_to_update = self.collect_references_to_update();

        for reference in references_to_update {
            let PolynomialReference {
                name, type_args, ..
            } = reference.clone();

            let types = type_args.unwrap();

            Self::resolve_and_update_reference(reference, types, trait_impls, &name);
        }
    }

    fn collect_references_to_update(
        &'a mut self,
    ) -> impl Iterator<Item = &mut PolynomialReference> {
        self.definitions
            .iter_mut()
            .flat_map(|(_, (_, def))| match def {
                Some(FunctionValueDefinition::Expression(TypedExpression { e: expr, .. })) => expr
                    .all_children_mut()
                    .filter_map(|e| match e {
                        Expression::Reference(
                            _,
                            Reference::Poly(
                                poly @ PolynomialReference {
                                    type_args: Some(_), ..
                                },
                            ),
                        ) => Some(poly),
                        _ => None,
                    })
                    .collect::<Vec<_>>(),
                _ => Vec::new(),
            })
            .chain(self.identities.iter_mut().flat_map(|identity| {
                identity
                    .left
                    .selector
                    .iter_mut()
                    .chain(once(identity.left.expressions.as_mut()))
                    .chain(identity.right.selector.iter_mut())
                    .chain(once(identity.right.expressions.as_mut()))
                    .flat_map(|expr| {
                        expr.all_children_mut()
                            .filter_map(|e| match e {
                                Expression::Reference(
                                    _,
                                    Reference::Poly(
                                        poly @ PolynomialReference {
                                            type_args: Some(_), ..
                                        },
                                    ),
                                ) => Some(poly),
                                _ => None,
                            })
                            .collect::<Vec<_>>()
                    })
            }))
            .into_iter()
    }

    fn resolve_and_update_reference(
        reference: &mut PolynomialReference,
        types: Vec<Type>,
        trait_impls: &HashMap<String, Vec<TraitImplementation<Expression>>>,
        name: &str,
    ) {
        let mut resolved_impl_pos: BTreeMap<Vec<Type>, usize> = BTreeMap::new();

        let (trait_decl_name, trait_fn_name) = name.split_once("::").unwrap(); // TODO improve this
        if let Some(impls) = trait_impls.get(trait_decl_name) {
            for (index, impl_) in impls.iter().enumerate() {
                if impl_.function_by_name(trait_fn_name).is_some() {
                    resolved_impl_pos.insert(types.to_vec(), index);
                }
            }
        }

        let full_name = format!("{}::{}", trait_decl_name, trait_fn_name);
        Self::update_reference(&full_name, reference, &resolved_impl_pos, name);
    }

    fn update_reference(
        ref_name: &str,
        c: &mut PolynomialReference,
        resolved_impl_pos: &BTreeMap<Vec<Type>, usize>,
        trait_name: &str,
    ) {
        let PolynomialReference {
            name,
            poly_id,
            type_args: current_type_args,
            ..
        } = c;
        if name == ref_name {
            *c = PolynomialReference {
                name: name.clone(),
                type_args: current_type_args.clone(),
                poly_id: *poly_id,
                resolved_impls: resolved_impl_pos.clone(),
                trait_name: Some(trait_name.to_string()),
            };
        }
    }
}

pub fn traits_resolution(
    definitions: &mut HashMap<String, (Symbol, Option<FunctionValueDefinition>)>,
    identities: &mut Vec<Identity<SelectedExpressions<Expression>>>,
    trait_impls: &HashMap<String, Vec<TraitImplementation<Expression>>>,
) {
    TraitsResolver::new(definitions, identities).resolve_traits(trait_impls);
}
