use std::collections::{BTreeMap, HashMap};

use powdr_ast::{
    analyzed::{
        Expression, FunctionValueDefinition, Identity, PolynomialReference, Reference, Symbol,
        TypedExpression,
    },
    parsed::{types::Type, visitor::ExpressionVisitable, SelectedExpressions, TraitImplementation},
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
        for (_, (_, def)) in self.definitions.iter_mut() {
            if let Some(FunctionValueDefinition::Expression(TypedExpression { e: expr, .. })) = def
            {
                expr.pre_visit_expressions_mut(&mut |e| {
                    if let Expression::Reference(
                        _,
                        Reference::Poly(
                            reference @ PolynomialReference {
                                type_args: Some(_), ..
                            },
                        ),
                    ) = e
                    {
                        Self::resolve_and_update_reference(reference, trait_impls);
                    }
                });
            }
        }

        for identity in self.identities.iter_mut() {
            let visit = |expr: &mut Expression| {
                expr.pre_visit_expressions_mut(&mut |e| {
                    if let Expression::Reference(
                        _,
                        Reference::Poly(
                            reference @ PolynomialReference {
                                type_args: Some(_), ..
                            },
                        ),
                    ) = e
                    {
                        Self::resolve_and_update_reference(reference, trait_impls);
                    }
                });
            };
            identity.left.selector.iter_mut().for_each(visit);
            visit(identity.left.expressions.as_mut());
            identity.right.selector.iter_mut().for_each(visit);
            visit(identity.right.expressions.as_mut());
        }
    }

    fn resolve_and_update_reference(
        reference: &mut PolynomialReference,
        trait_impls: &HashMap<String, Vec<TraitImplementation<Expression>>>,
    ) {
        let mut resolved_impl_pos: BTreeMap<Vec<Type>, usize> = BTreeMap::new();

        let name = reference.name.clone();
        let (trait_decl_name, trait_fn_name) = name.rsplit_once("::").unwrap();
        if let Some(impls) = trait_impls.get(trait_decl_name) {
            for (index, impl_) in impls.iter().enumerate() {
                if impl_.function_by_name(trait_fn_name).is_some() {
                    resolved_impl_pos.insert(reference.type_args.as_ref().unwrap().to_vec(), index);
                }
            }
        }

        let full_name = format!("{trait_decl_name}::{trait_fn_name}");
        Self::update_reference(&full_name, reference, &resolved_impl_pos, &name);
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
