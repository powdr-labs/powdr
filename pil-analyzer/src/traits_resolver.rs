use powdr_ast::{
    analyzed::{Expression, PolynomialReference},
    parsed::{
        types::{TupleType, Type},
        TraitImplementation,
    },
};
use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

use crate::type_unifier::Unifier;

/// Mapping from trait function name and (concrete) type arguments to the corresponding trait implementation.
pub type SolvedTraitImpls = HashMap<String, HashMap<Vec<Type>, Arc<Expression>>>;

/// TraitsResolver helps to find the implementation for a given trait function
/// and concrete type arguments.
pub struct TraitsResolver<'a> {
    /// All trait names, even if they have no implementation.
    traits: HashSet<&'a str>,
    /// List of implementations for all traits.
    trait_impls: HashMap<String, Vec<&'a TraitImplementation<Expression>>>,
    /// Map from trait function names and type arguments to the corresponding trait implementations.
    solved_impls: SolvedTraitImpls,
}

impl<'a> TraitsResolver<'a> {
    /// Creates a new instance of the resolver.
    /// The trait impls need to have a key for every trait name, even if it is not implemented at all.
    pub fn new(
        traits: HashSet<&'a str>,
        trait_impls: &'a [TraitImplementation<Expression>],
    ) -> Self {
        let mut impls_by_trait: HashMap<String, Vec<_>> = HashMap::new();
        for i in trait_impls {
            impls_by_trait
                .entry(i.name.to_string())
                .or_default()
                .push(i);
        }
        Self {
            traits,
            trait_impls: impls_by_trait,
            solved_impls: HashMap::new(),
        }
    }

    /// Resolves a trait function reference for a given polynomial reference.
    /// If successful, it stores the resolved implementation to be returned via `solved_impls()`.
    pub fn resolve_trait_function_reference(
        &mut self,
        reference: &PolynomialReference,
    ) -> Result<(), String> {
        let Some(type_args) = reference.type_args.as_ref() else {
            // Not generic, cannot be a trait function reference.
            return Ok(());
        };
        // Shortcut if have already done this.
        if let Some(inner_map) = self.solved_impls.get(&reference.name) {
            if inner_map.contains_key(type_args) {
                return Ok(());
            }
        }

        // Now we need to find out if this is a trait function at all or just a generic function.
        let Some((trait_decl_name, trait_fn_name)) = reference.name.rsplit_once("::") else {
            return Ok(());
        };
        if !self.traits.contains(trait_decl_name) {
            // Not a trait function.
            return Ok(());
        }
        let Some(trait_impls) = self.trait_impls.get(trait_decl_name) else {
            return Err(format!(
                "Could not find an implementation for the trait function {reference}"
            ));
        };

        match find_trait_implementation(trait_fn_name, type_args, trait_impls) {
            Some(expr) => {
                self.solved_impls
                    .entry(reference.name.clone())
                    .or_default()
                    .insert(type_args.clone(), expr);
                Ok(())
            }
            None => Err(format!(
                "Could not find an implementation for the trait function {reference}"
            )),
        }
    }

    /// Returns a map from all referenced trait functions and all their type arguments to the
    /// corresponding trait implementations.
    pub fn solved_impls(self) -> SolvedTraitImpls {
        self.solved_impls
    }
}

fn find_trait_implementation(
    function: &str,
    type_args: &[Type],
    implementations: &[&TraitImplementation<Expression>],
) -> Option<Arc<Expression>> {
    let tuple_args = Type::Tuple(TupleType {
        items: type_args.to_vec(),
    });
    assert!(tuple_args.is_concrete_type());

    implementations.iter().find_map(|impl_| {
        Unifier::default()
            .unify_types(tuple_args.clone(), impl_.type_scheme.ty.clone())
            .is_ok()
            .then(|| impl_.function_by_name(function).unwrap().body.clone())
    })
}
