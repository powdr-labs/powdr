use powdr_ast::{
    analyzed::{Expression, PolynomialReference},
    parsed::{
        types::{TupleType, Type},
        TraitImplementation,
    },
};
use std::{collections::HashMap, sync::Arc};

use crate::type_unifier::Unifier;

type SolvedImpl = ((String, Vec<Type>), Arc<Expression>);

pub struct TraitsResolver {
    trait_impls: HashMap<String, Vec<TraitImplementation<Expression>>>,
    solved_impls: HashMap<String, HashMap<Vec<Type>, Arc<Expression>>>,
}

impl TraitsResolver {
    pub fn new(trait_impls: &HashMap<String, Vec<TraitImplementation<Expression>>>) -> Self {
        Self {
            trait_impls: trait_impls.clone(),
            solved_impls: HashMap::new(),
        }
    }

    pub fn resolve_trait(&mut self, ref_poly: &PolynomialReference) -> Result<(), String> {
        match self.resolve_trait_function_reference(ref_poly) {
            Some(((key, type_args), expr)) => {
                self.solved_impls
                    .entry(key)
                    .or_default()
                    .insert(type_args, expr);
                Ok(())
            }
            None => Err(format!("Impl not found for {ref_poly}")),
        }
    }

    pub fn solved_impls(self) -> HashMap<String, HashMap<Vec<Type>, Arc<Expression>>> {
        self.solved_impls
    }

    fn resolve_trait_function_reference(
        &self,
        reference: &PolynomialReference,
    ) -> Option<SolvedImpl> {
        let (trait_decl_name, trait_fn_name) = reference.name.rsplit_once("::")?;
        if let Some(impls) = self.trait_impls.get(trait_decl_name) {
            let type_args = reference.type_args.as_ref().unwrap().to_vec();
            for impl_ in impls.iter() {
                let Type::Tuple(TupleType { items: _ }) = impl_.type_scheme.ty else {
                    unreachable!()
                };

                let tuple_args = Type::Tuple(TupleType {
                    items: type_args.clone(),
                });

                assert!(tuple_args.is_concrete_type());

                let mut unifier: Unifier = Default::default();

                let res = unifier.unify_types(tuple_args.clone(), impl_.type_scheme.ty.clone());
                if let Ok(()) = res {
                    let expr = impl_.function_by_name(trait_fn_name).unwrap();
                    return Some(((reference.name.clone(), type_args), Arc::clone(&expr.body)));
                }
            }
        }

        None
    }
}
