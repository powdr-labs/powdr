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

/// TraitsResolver implements a trait resolver for polynomial references.
/// For each reference to a trait function with type arguments, it finds the corresponding
/// trait implementation and stores this association in a map that is returned.
pub struct TraitsResolver<'a> {
    trait_impls: &'a HashMap<String, Vec<TraitImplementation<Expression>>>,
    solved_impls: HashMap<String, HashMap<Vec<Type>, Arc<Expression>>>,
}

impl<'a> TraitsResolver<'a> {
    pub fn new(trait_impls: &'a HashMap<String, Vec<TraitImplementation<Expression>>>) -> Self {
        Self {
            trait_impls,
            solved_impls: HashMap::new(),
        }
    }

    /// Resolves a trait function reference for a given polynomial reference.
    /// If successful, it stores the resolved implementation to be returned via `solved_impls()`.
    pub fn resolve_trait_function_reference(
        &mut self,
        ref_poly: &PolynomialReference,
    ) -> Result<(), String> {
        if let Some(inner_map) = self.solved_impls.get(&ref_poly.name) {
            match &ref_poly.type_args {
                None => return Ok(()),
                Some(t_args) if inner_map.contains_key(t_args) => return Ok(()),
                _ => {}
            }
        }

        match self.resolve_trait(ref_poly) {
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

    /// Returns the solved implementations.
    pub fn solved_impls(self) -> HashMap<String, HashMap<Vec<Type>, Arc<Expression>>> {
        self.solved_impls
    }

    fn resolve_trait(&self, reference: &PolynomialReference) -> Option<SolvedImpl> {
        let (trait_decl_name, trait_fn_name) = reference.name.rsplit_once("::")?;
        if let Some(impls) = self.trait_impls.get(trait_decl_name) {
            let type_args = reference.type_args.as_ref().unwrap().to_vec();
            let tuple_args = Type::Tuple(TupleType {
                items: type_args.clone(),
            });
            for impl_ in impls.iter() {
                assert!(tuple_args.is_concrete_type());

                let mut unifier: Unifier = Default::default();

                let res = unifier.unify_types(tuple_args.clone(), impl_.type_scheme.ty.clone());
                if res.is_ok() {
                    let expr = impl_.function_by_name(trait_fn_name).unwrap();
                    return Some(((reference.name.clone(), type_args), Arc::clone(&expr.body)));
                }
            }
        }

        None
    }
}
