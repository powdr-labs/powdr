use powdr_ast::{
    analyzed::{
        Expression, FunctionValueDefinition, PolynomialReference, Reference, Symbol,
        TypedExpression,
    },
    parsed::{
        types::{TupleType, Type},
        visitor::AllChildren,
        TraitImplementation,
    },
};
use std::{
    collections::{HashMap, HashSet, VecDeque},
    sync::Arc,
};

use crate::type_unifier::Unifier;

type SolvedImpl = ((String, Vec<Type>), Arc<Expression>);

/// TraitsResolver implements a trait resolver for polynomial references.
/// For each reference to a trait function with type arguments, it finds the corresponding
/// trait implementation and stores this association in a map that is returned.
pub struct TraitsResolver<'a> {
    trait_impls: &'a HashMap<String, Vec<TraitImplementation<Expression>>>,
    solved_impls: HashMap<String, HashMap<Vec<Type>, Arc<Expression>>>,
    trait_typevars_mapping: HashMap<String, HashSet<Vec<Type>>>,
}

impl<'a> TraitsResolver<'a> {
    pub fn new(
        trait_impls: &'a HashMap<String, Vec<TraitImplementation<Expression>>>,
        definitions: &'a HashMap<String, (Symbol, Option<FunctionValueDefinition>)>,
    ) -> Self {
        let filtered = definitions.values().filter_map(|(s, def)| {
            if let Some(FunctionValueDefinition::Expression(TypedExpression { e, .. })) = def {
                Some((s, e))
            } else {
                None
            }
        });
        // Maybe this is a lot here but its better to do it again for every reference
        let trait_typevars_mapping = Self::build_reference_path(filtered);

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
                                let tuple_args = Type::Tuple(TupleType { items: t.clone() });

                                let mut unifier: Unifier = Default::default();

                                let res = unifier
                                    .unify_types(tuple_args.clone(), impl_.type_scheme.ty.clone());
                                if res.is_ok() {
                                    if let Some(expr) = impl_.function_by_name(trait_fn_name) {
                                        solved_impls.push((
                                            (reference.name.clone(), t.clone()),
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

    /// Creates a dictionary that associates each child reference with a generic type along with the references that called it
    /// From this dictionary, it resolves the parent/child relationships to obtain the value of the generic type
    fn build_reference_path<'b>(
        definitions: impl Iterator<Item = (&'b Symbol, &'b Expression)>,
    ) -> HashMap<String, HashSet<Vec<Type>>> {
        let mut result = HashMap::new();

        for (s, e) in definitions {
            let mut references = Vec::new();
            e.all_children().for_each(|expr| {
                if let Expression::Reference(
                    _,
                    Reference::Poly(PolynomialReference {
                        name,
                        type_args: Some(type_args),
                        ..
                    }),
                ) = expr
                {
                    if !type_args.is_empty() {
                        references.push((name, type_args));
                    }
                }
            });
            if !references.is_empty() {
                result.insert(s.absolute_name.clone(), references);
            }
        }

        Self::combine_type_vars_paths(result)
    }

    fn get_types_for_child(
        input: &HashMap<String, Vec<(&String, &Vec<Type>)>>,
        size: usize,
        target: &str,
        result: &mut HashMap<String, HashSet<Vec<Type>>>,
    ) -> bool {
        let mut temp = HashSet::new();
        let mut updated = false;
        for child in input.values() {
            for (child, types) in child {
                if result.contains_key(&child.to_string())
                    || (*child == target && types.iter().all(|t| !matches!(t, Type::TypeVar(_))))
                {
                    let selected_types: Vec<Type> = types.iter().take(size).cloned().collect();
                    temp.insert(selected_types);
                }
            }
        }

        if !temp.is_empty() {
            result.insert(target.to_string(), temp);
            updated = true;
        }

        updated
    }

    /// Solve the initial asociation and builds a dictionary that maps the
    /// names of traits containing generic types to their represented types.
    fn combine_type_vars_paths(
        input: HashMap<String, Vec<(&String, &Vec<Type>)>>,
    ) -> HashMap<String, HashSet<Vec<Type>>> {
        let mut result: HashMap<String, HashSet<Vec<Type>>> = HashMap::new();
        let mut to_process: VecDeque<&String> = input.keys().collect();

        let mut pointer = to_process.pop_front();
        while pointer.is_some() {
            let parent = pointer.unwrap();

            if let Some(children) = input.get(parent) {
                for (child, types) in children {
                    if types.iter().any(|t| matches!(t, Type::TypeVar(_))) {
                        let updated =
                            Self::get_types_for_child(&input, types.len(), child, &mut result);
                        if !updated {
                            to_process.push_back(parent);
                        }
                    } else {
                        if result.contains_key(*child) {
                            result.get_mut(*child).unwrap().insert(types.to_vec());
                        } else {
                            result.insert(child.to_string(), HashSet::from([types.to_vec()]));
                        }
                    }
                }
            }

            pointer = to_process.pop_front();
        }

        result
    }
}
