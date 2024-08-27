use std::collections::HashMap;

use itertools::Itertools;
use powdr_ast::{
    analyzed::{
        Expression, FunctionValueDefinition, Identity, PolynomialReference, Reference, Symbol,
        TypedExpression,
    },
    parsed::{
        types::{TupleType, Type},
        visitor::AllChildren,
        SelectedExpressions, TraitImplementation,
    },
};

pub struct TraitsResolver<'a> {
    definitions: &'a HashMap<String, (Symbol, Option<FunctionValueDefinition>)>,
    identities: &'a Vec<Identity<SelectedExpressions<Expression>>>,
}

impl<'a> TraitsResolver<'a> {
    pub fn new(
        definitions: &'a HashMap<String, (Symbol, Option<FunctionValueDefinition>)>,
        identities: &'a Vec<Identity<SelectedExpressions<Expression>>>,
    ) -> Self {
        Self {
            definitions,
            identities,
        }
    }

    pub fn resolve_traits(
        &'a self,
        trait_impls: &HashMap<String, Vec<TraitImplementation<Expression>>>,
    ) -> HashMap<String, Expression> {
        let mut result = HashMap::new();

        let resolve_references = |expr: &Expression| {
            expr.all_children()
                .filter_map(|expr| {
                    if let Expression::Reference(
                        _,
                        Reference::Poly(
                            reference @ PolynomialReference {
                                type_args: Some(_), ..
                            },
                        ),
                    ) = expr
                    {
                        self.resolve_reference_trait(reference, trait_impls)
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>()
        };

        for (_, (_, def)) in self.definitions.iter() {
            if let Some(FunctionValueDefinition::Expression(TypedExpression { e: expr, .. })) = def
            {
                for (key, value) in resolve_references(expr) {
                    result.entry(key).or_insert(value);
                }
            }
        }

        for identity in self.identities.iter() {
            for selector in identity
                .left
                .selector
                .iter()
                .chain(identity.right.selector.iter())
            {
                for (key, value) in resolve_references(selector) {
                    result.entry(key).or_insert(value);
                }
            }
            for (key, value) in resolve_references(identity.left.expressions.as_ref()) {
                result.entry(key).or_insert(value);
            }
            for (key, value) in resolve_references(identity.right.expressions.as_ref()) {
                result.entry(key).or_insert(value);
            }
        }

        result
    }
    fn resolve_reference_trait(
        &self,
        reference: &PolynomialReference,
        trait_impls: &HashMap<String, Vec<TraitImplementation<Expression>>>,
    ) -> Option<(String, Expression)> {
        let name = reference.name.clone();
        let (trait_decl_name, trait_fn_name) = name.rsplit_once("::")?;
        if let Some(impls) = trait_impls.get(trait_decl_name) {
            for impl_ in impls.iter() {
                let types = impl_.type_scheme.ty.clone();
                let Type::Tuple(TupleType { items }) = types else {
                    unreachable!()
                };
                let type_args = reference.type_args.as_ref().unwrap().to_vec();

                if type_args == items {
                    let expr = impl_.function_by_name(trait_fn_name).unwrap();
                    let key = format!("{name}<{}>", type_args.iter().join(", "));
                    return Some((key.clone(), expr.body.as_ref().clone()));
                }
            }
        }

        None
    }
}

pub fn traits_resolution(
    definitions: &HashMap<String, (Symbol, Option<FunctionValueDefinition>)>,
    identities: &Vec<Identity<SelectedExpressions<Expression>>>,
    trait_impls: &HashMap<String, Vec<TraitImplementation<Expression>>>,
) -> HashMap<String, Expression> {
    TraitsResolver::new(definitions, identities).resolve_traits(trait_impls)
}
