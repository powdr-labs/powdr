use powdr_ast::{
    analyzed::{Expression, PolynomialReference},
    parsed::{
        types::{TupleType, Type},
        TraitImplementation,
    },
};
use std::{collections::HashMap, sync::Arc};

use crate::type_unifier::Unifier;

/// Resolves trait implementations for all definitions and identities.
/// Checks references for calls to trait implementations
/// and unifies them using their type args.
///
/// Returns a nested map of trait name to a map of type args to the expression.
pub fn traits_resolution(
    references: Vec<PolynomialReference>,
    trait_impls: &HashMap<String, Vec<TraitImplementation<Expression>>>,
) -> HashMap<String, HashMap<Vec<Type>, Arc<Expression>>> {
    TraitsResolver::new().resolve_traits(references, trait_impls)
}

type SolvedImpl = ((String, Vec<Type>), Arc<Expression>);

struct TraitsResolver {}

impl TraitsResolver {
    pub fn new() -> Self {
        Self {}
    }

    pub fn resolve_traits(
        &self,
        references: Vec<PolynomialReference>,
        trait_impls: &HashMap<String, Vec<TraitImplementation<Expression>>>,
    ) -> HashMap<String, HashMap<Vec<Type>, Arc<Expression>>> {
        let mut solved_impls = HashMap::new();

        for ref_poly in references {
            if let Some(solved) = self.resolve_trait_function_reference(&ref_poly, trait_impls) {
                let (key, type_args) = solved.0;
                solved_impls
                    .entry(key)
                    .or_insert_with(HashMap::new)
                    .insert(type_args, solved.1);
            }
        }

        solved_impls
    }

    fn resolve_trait_function_reference(
        &self,
        reference: &PolynomialReference,
        trait_impls: &HashMap<String, Vec<TraitImplementation<Expression>>>,
    ) -> Option<SolvedImpl> {
        let (trait_decl_name, trait_fn_name) = reference.name.rsplit_once("::")?;
        if let Some(impls) = trait_impls.get(trait_decl_name) {
            let type_args = reference.type_args.as_ref().unwrap().to_vec();
            for impl_ in impls.iter() {
                let Type::Tuple(TupleType { items: _ }) = impl_.type_scheme.ty else {
                    unreachable!()
                };

                let tuple_args = Type::Tuple(TupleType {
                    items: type_args.clone(),
                });

                let expr = impl_.function_by_name(trait_fn_name).unwrap();
                let mut unifier: Unifier = Default::default();

                let res = unifier.unify_types(tuple_args.clone(), impl_.type_scheme.ty.clone());
                match res {
                    Ok(()) => {
                        return Some(((reference.name.clone(), type_args), Arc::clone(&expr.body)))
                    }
                    Err(_) => {}
                }
            }
        }

        None
    }
}
