use powdr_ast::{
    analyzed::{
        Expression, FunctionValueDefinition, PolynomialReference, SolvedTraitImpls, Symbol,
    },
    parsed::{
        types::{TupleType, Type, TypeScheme},
        TraitDeclaration, TraitImplementation,
    },
};
use powdr_parser_util::Error;
use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

use crate::type_unifier::Unifier;

/// TraitsResolver helps to find the implementation for a given trait function
/// and concrete type arguments.
pub struct TraitsResolver<'a> {
    /// All trait names, even if they have no implementation.
    traits: HashSet<&'a str>,
    /// List of implementations for all traits and their index in the list of trait impls.
    trait_impls: HashMap<String, Vec<(&'a TraitImplementation<Expression>, usize)>>,
    /// Index data structure that we are building up here.
    solved_impls: SolvedTraitImpls,
}

impl<'a> TraitsResolver<'a> {
    pub fn new(
        traits: HashSet<&'a str>,
        trait_impls: &'a [TraitImplementation<Expression>],
    ) -> Self {
        let mut impls_by_trait: HashMap<String, Vec<_>> = HashMap::new();
        for (i, impl_) in trait_impls.iter().enumerate() {
            impls_by_trait
                .entry(impl_.name.to_string())
                .or_default()
                .push((impl_, i));
        }

        Self {
            traits,
            trait_impls: impls_by_trait,
            solved_impls: Default::default(),
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
        if self
            .solved_impls
            .try_resolve_trait_function(&reference.name, type_args)
            .is_some()
        {
            return Ok(());
        }

        // Now we need to find out if this is a trait function at all or just a generic function.
        let Some((trait_decl_name, trait_fn_name)) = reference.name.rsplit_once("::") else {
            return Ok(());
        };
        if !self.traits.contains(trait_decl_name) {
            // Not a trait function.
            return Ok(());
        }
        let Some(trait_impls) = self.trait_impls.get_mut(trait_decl_name) else {
            return Err(format!(
                "Could not find an implementation for the trait function {reference} (trait is not implemented at all)"
            ));
        };

        match find_trait_implementation(trait_fn_name, type_args, trait_impls) {
            Some((expr, index)) => {
                self.solved_impls
                    .insert(reference.name.clone(), type_args.clone(), index, expr);
                Ok(())
            }
            None => Err(format!(
                "Could not find a matching implementation for the trait function {reference}"
            )),
        }
    }

    /// Returns a data structure that helps in mapping all referenced trait functions and all
    /// their type arguments to the corresponding trait implementations.
    pub fn solved_impls(self) -> SolvedTraitImpls {
        self.solved_impls
    }

    /// Checks for overlapping trait implementations.
    ///
    /// This method iterates through all the trait declarations.
    /// For each declaration, it checks that there are no traits with the same name and overlapping type variables.
    /// Overlapping implementations can lead to ambiguity in trait function calls, even when all types
    /// are fully concrete. This check helps prevent such ambiguities and ensures clear resolution
    /// of trait function calls.
    ///
    /// It also checks that the number of type variables in the implementation matches
    /// the number of type variables in the corresponding trait declaration.
    pub fn validate_trait_implementations(
        &self,
        definitions: &HashMap<String, (Symbol, Option<FunctionValueDefinition>)>,
    ) -> Result<(), Vec<Error>> {
        let mut errors = Vec::new();
        for (_name, (_symbol, definition)) in definitions.iter() {
            if let Some(FunctionValueDefinition::TraitDeclaration(trait_decl)) = definition {
                let name = trait_decl.name.clone();
                if let Some(trait_impls) = self.trait_impls.get(&name) {
                    match self.validate_impl_definitions(trait_impls, trait_decl) {
                        Ok(_) => {
                            if let Err(e) = self.ensure_unique_impls(trait_impls) {
                                errors.push(e);
                            }
                        }
                        Err(e) => {
                            errors.push(e);
                        }
                    }
                } else {
                    errors.push(trait_decl.source_ref.with_error(format!(
                        "Could not find an implementation for the trait {name}"
                    )));
                }
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    /// Validates the trait implementations in the given `implementations` list against the trait
    /// declarations passed in the `trait_decl` parameter.
    fn validate_impl_definitions(
        &self,
        implementations: &[(&TraitImplementation<Expression>, usize)],
        trait_decl: &TraitDeclaration,
    ) -> Result<(), Error> {
        for trait_impl in implementations {
            let decl_vars_set: HashSet<_> = trait_decl.type_vars.clone().into_iter().collect();
            let used_vars_set: HashSet<_> = trait_decl
                .functions
                .iter()
                .flat_map(|nt| nt.ty.contained_type_vars())
                .collect();
            for var in decl_vars_set.iter() {
                if !used_vars_set.contains(var) {
                    return Err(trait_decl.source_ref.with_error(format!(
                        "Type variable {var} is declared but never used in trait declaration {}",
                        trait_decl.name
                    )));
                }
            }
            let Type::Tuple(TupleType { items: ref types }) = trait_impl.0.type_scheme.ty else {
                panic!("Type from trait scheme is not a tuple.")
            };
            let trait_name = trait_impl.0.name.clone();

            if types.len() != trait_decl.type_vars.len() {
                return Err(trait_impl.0.source_ref.with_error(format!(
                    "Trait {} has {} type parameters, but implementation has {}",
                    trait_name,
                    trait_decl.type_vars.len(),
                    types.len(),
                )));
            }

            // let type_vars_in_tuple: Vec<_> = types
            //     .iter_mut()
            //     .flat_map(|t| t.contained_type_vars())
            //     .collect();

            // let type_vars_in_scheme: Vec<_> = trait_impl.0.type_scheme.vars.vars().collect();

            // for var in type_vars_in_scheme {
            //     if !type_vars_in_tuple.contains(&var) {
            //         return Err(trait_impl.0.source_ref.with_error(format!(
            //             "Impl {trait_name} introduces a type variable {var} that's never used",
            //         )));
            //     }
            // }
        }
        Ok(())
    }

    /// Ensures that there are no overlapping trait implementations in the given `implementations` list.
    ///
    /// This function iterates through all the trait implementations comparing them with each other and ensuring that
    /// there are no traits with overlapping type variables.
    fn ensure_unique_impls(
        &self,
        implementations: &[(&TraitImplementation<Expression>, usize)],
    ) -> Result<(), Error> {
        for i in 0..implementations.len() {
            let mut _impl1 = implementations[i].0.clone();
            let type_vars: HashSet<_> = implementations[i].0.type_scheme.vars.vars().collect();
            _impl1.type_scheme.ty.map_to_type_vars(&type_vars);

            for j in (i + 1)..implementations.len() {
                let mut _impl2 = implementations[j].0.clone();
                let type_vars: HashSet<_> = _impl2.type_scheme.vars.vars().collect();
                _impl2.type_scheme.ty.map_to_type_vars(&type_vars);

                if let Err(err) = self.unify_traits_types(&_impl1.type_scheme, &_impl2.type_scheme)
                {
                    return Err(_impl1
                        .source_ref
                        .with_error(format!("Impls for {}: {err}", implementations[i].0.name)));
                }
            }
        }
        Ok(())
    }

    fn unify_traits_types(&self, ty1: &TypeScheme, ty2: &TypeScheme) -> Result<(), String> {
        let mut unifier = Unifier::new();
        let instantiated_ty1 = unifier.instantiate_scheme(ty1.clone());
        let instantiated_ty2 = unifier.instantiate_scheme(ty2.clone());

        match unifier.unify_types(instantiated_ty1.0.clone(), instantiated_ty2.0.clone()) {
            Ok(_) => Err(format!("Types {} and {} overlap", ty1.ty, ty2.ty)),
            Err(_) => Ok(()),
        }
    }
}

fn find_trait_implementation(
    function: &str,
    type_args: &[Type],
    implementations: &[(&TraitImplementation<Expression>, usize)],
) -> Option<(Arc<Expression>, usize)> {
    let tuple_args = Type::Tuple(TupleType {
        items: type_args.to_vec(),
    });
    assert!(tuple_args.is_concrete_type());

    implementations.iter().find_map(|(impl_, index)| {
        Unifier::default()
            .unify_types(tuple_args.clone(), impl_.type_scheme.ty.clone())
            .is_ok()
            .then(|| {
                (
                    impl_.function_by_name(function).unwrap().body.clone(),
                    *index,
                )
            })
    })
}
