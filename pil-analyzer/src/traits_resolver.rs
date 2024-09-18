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
    trait_typevars_mapping: &'a HashMap<String, Vec<Type>>,
}

impl<'a> TraitsResolver<'a> {
    pub fn new(
        trait_impls: &'a HashMap<String, Vec<TraitImplementation<Expression>>>,
        trait_typevars_mapping: &'a HashMap<String, Vec<Type>>,
    ) -> Self {
        Self {
            trait_impls,
            solved_impls: HashMap::new(),
            trait_typevars_mapping,
        }
    }

    /// Resolves a trait function reference for a given polynomial reference.
    /// If successful, it stores the resolved implementation to be returned via `solved_impls()`.
    pub fn resolve_trait_function_reference(
        &mut self,
        ref_poly: &PolynomialReference,
    ) -> Result<(), String> {
        if ref_poly.type_args.is_none() {
            return Ok(());
        }
        let type_args = ref_poly.type_args.as_ref().unwrap();
        if let Some(inner_map) = self.solved_impls.get(&ref_poly.name) {
            if inner_map.contains_key(type_args) {
                return Ok(());
            }
        }

        let resolved_traits = self.resolve_trait(ref_poly);
        if !resolved_traits.is_empty() {
            for ((key, type_args), expr) in resolved_traits {
                self.solved_impls
                    .entry(key)
                    .or_default()
                    .insert(type_args, expr);
            }
            Ok(())
        } else {
            Err(format!("Impl not found for {ref_poly}"))
        }
    }

    /// Returns the solved implementations.
    pub fn solved_impls(self) -> HashMap<String, HashMap<Vec<Type>, Arc<Expression>>> {
        self.solved_impls
    }

    fn resolve_trait(&self, reference: &PolynomialReference) -> Vec<SolvedImpl> {
        let mut solved_impls = Vec::new();
        let (trait_decl_name, trait_fn_name) = match reference.name.rsplit_once("::") {
            Some(parts) => parts,
            None => return solved_impls,
        };

        if let Some(impls) = self.trait_impls.get(trait_decl_name) {
            let type_args = reference.type_args.as_ref().unwrap().to_vec();
            let tuple_args = Type::Tuple(TupleType {
                items: type_args.clone(),
            });

            if tuple_args.is_concrete_type() {
                for impl_ in impls.iter() {
                    let mut unifier: Unifier = Default::default();

                    let res = unifier.unify_types(tuple_args.clone(), impl_.type_scheme.ty.clone());
                    if res.is_ok() {
                        if let Some(expr) = impl_.function_by_name(trait_fn_name) {
                            solved_impls.push((
                                (reference.name.clone(), type_args.clone()),
                                Arc::clone(&expr.body),
                            ));
                        }
                    }
                }
            } else {
                for impl_ in impls.iter() {
                    let type_vars = self.trait_typevars_mapping.get(&reference.name);
                    match type_vars {
                        Some(type_vars) => {
                            for t in type_vars {
                                let tuple_args = Type::Tuple(TupleType {
                                    items: vec![t.clone()],
                                });

                                let mut unifier: Unifier = Default::default();

                                let res = unifier
                                    .unify_types(tuple_args.clone(), impl_.type_scheme.ty.clone());
                                if res.is_ok() {
                                    if let Some(expr) = impl_.function_by_name(trait_fn_name) {
                                        solved_impls.push((
                                            (reference.name.clone(), vec![t.clone()]),
                                            Arc::clone(&expr.body),
                                        ));
                                    }
                                }
                            }
                        }
                        None => {
                            continue;
                        }
                    }
                }
            }
        }

        solved_impls
    }
}
