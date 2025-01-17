// use powdr_ast::{
//     analyzed::{Expression, PolynomialReference}, //, SolvedTraitImpls},
//     parsed::{
//         types::{TupleType, Type},
//         TraitImplementation,
//     },
// };
// use std::{
//     collections::{HashMap, HashSet},
//     sync::Arc,
// };

// use crate::type_unifier::Unifier;

// /// TraitsResolver helps to find the implementation for a given trait function
// /// and concrete type arguments.
// pub struct TraitsResolver<'a> {
//     /// All trait names, even if they have no implementation.
//     traits: HashSet<&'a str>,
//     /// List of implementations for all traits and their index in the list of trait impls.
//     trait_impls: HashMap<String, Vec<(&'a TraitImplementation<Expression>, usize)>>,
//     // /// Index data structure that we are building up here.
//     // solved_impls: SolvedTraitImpls,
// }

// impl<'a> TraitsResolver<'a> {
//     pub fn new(
//         traits: HashSet<&'a str>,
//         trait_impls: &'a [TraitImplementation<Expression>],
//     ) -> Self {
//         let mut impls_by_trait: HashMap<String, Vec<_>> = HashMap::new();
//         for (i, impl_) in trait_impls.iter().enumerate() {
//             impls_by_trait
//                 .entry(impl_.name.to_string())
//                 .or_default()
//                 .push((impl_, i));
//         }
//         Self {
//             traits,
//             trait_impls: impls_by_trait,
//             //solved_impls: Default::default(),
//         }
//     }

//     /// Resolves a trait function reference for a given polynomial reference.
//     /// If successful, it stores the resolved implementation to be returned via `solved_impls()`.
//     pub fn resolve_trait_function_reference(
//         &mut self,
//         reference: &PolynomialReference,
//     ) -> Result<(), String> {
//         let Some(type_args) = reference.type_args.as_ref() else {
//             // Not generic, cannot be a trait function reference.
//             return Ok(());
//         };
//         // Shortcut if have already done this.
//         if self
//             .solved_impls
//             .try_resolve_trait_function(&reference.name, type_args)
//             .is_some()
//         {
//             return Ok(());
//         }

//         // Now we need to find out if this is a trait function at all or just a generic function.
//         let Some((trait_decl_name, trait_fn_name)) = reference.name.rsplit_once("::") else {
//             return Ok(());
//         };
//         if !self.traits.contains(trait_decl_name) {
//             // Not a trait function.
//             return Ok(());
//         }
//         let Some(trait_impls) = self.trait_impls.get(trait_decl_name) else {
//             return Err(format!(
//                 "Could not find an implementation for the trait function {reference} (trait is not implemented at all)"
//             ));
//         };

//         match find_trait_implementation(trait_fn_name, type_args, trait_impls) {
//             Some((expr, index)) => {
//                 self.solved_impls
//                     .insert(reference.name.clone(), type_args.clone(), index, expr);
//                 Ok(())
//             }
//             None => Err(format!(
//                 "Could not find a matching implementation for the trait function {reference}"
//             )),
//         }
//     }

//     /// Returns a data structure that helps in mapping all referenced trait functions and all
//     /// their type arguments to the corresponding trait implementations.
//     // pub fn solved_impls(self) -> SolvedTraitImpls {
//     //     self.solved_impls
//     // }
// }

// fn find_trait_implementation(
//     function: &str,
//     type_args: &[Type],
//     implementations: &[(&TraitImplementation<Expression>, usize)],
// ) -> Option<(Arc<Expression>, usize)> {
//     let tuple_args = Type::Tuple(TupleType {
//         items: type_args.to_vec(),
//     });
//     assert!(tuple_args.is_concrete_type());

//     implementations.iter().find_map(|(impl_, index)| {
//         Unifier::default()
//             .unify_types(tuple_args.clone(), impl_.type_scheme.ty.clone())
//             .is_ok()
//             .then(|| {
//                 (
//                     impl_.function_by_name(function).unwrap().body.clone(),
//                     *index,
//                 )
//             })
//     })
// }
