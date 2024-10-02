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
    collections::{HashMap, HashSet},
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
        let (expressions, trait_functions): (Vec<_>, HashSet<_>) = definitions.values().fold(
            (Vec::new(), HashSet::new()),
            |(mut exprs, mut traits), (s, def)| {
                if let Some(FunctionValueDefinition::Expression(TypedExpression { e, .. })) = def {
                    exprs.push((&s.absolute_name, e));
                } else if let Some(FunctionValueDefinition::TraitFunction(_, _)) = def {
                    traits.insert(&s.absolute_name);
                }
                (exprs, traits)
            },
        );

        let trait_typevars_mapping = if trait_impls.is_empty() {
            Default::default()
        } else {
            Self::build_reference_path(&expressions, &trait_functions)
        };

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

            // TODO: This code is a bit ugly.
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
    fn build_reference_path(
        definitions: &Vec<(&String, &Expression)>,
        defined_traits: &HashSet<&String>,
    ) -> HashMap<String, HashSet<Vec<Type>>> {
        let mut result = HashMap::new();

        // Compute all pairs of references (name, type) for each child and store them associated with the parent's name
        // ["Child": [
        //      ("Parent1", [Some(type_args)]),
        //      ("Parent2", [Some(type_args)])
        // ],...]
        let mut all_type_vars = true;
        for (symbol, expr) in definitions {
            let mut references = Vec::new();
            expr.all_children().for_each(|e| {
                if let Expression::Reference(
                    _,
                    Reference::Poly(PolynomialReference {
                        name,
                        type_args: Some(type_args),
                        ..
                    }),
                ) = e
                {
                    if (!type_args.is_empty()) && (name != *symbol) {
                        if all_type_vars && type_args.iter().any(|t| !matches!(t, Type::TypeVar(_)))
                        {
                            all_type_vars = false;
                        }
                        references.push((name, type_args));
                    }
                }
            });
            for (name, type_args) in references {
                result
                    .entry(name)
                    .or_insert_with(Vec::new)
                    .push((*symbol, type_args));
            }
        }

        if all_type_vars {
            return Default::default();
        }

        Self::combine_type_vars_paths(&result, defined_traits)
    }

    /// Solve the initial asociation and builds a dictionary that maps the
    /// names of traits containing generic types to their represented types.
    fn combine_type_vars_paths(
        input: &HashMap<&String, Vec<(&String, &Vec<Type>)>>,
        defined_traits: &HashSet<&String>,
    ) -> HashMap<String, HashSet<Vec<Type>>> {
        let mut result: HashMap<String, HashSet<Vec<Type>>> = HashMap::new();

        for t in defined_traits {
            match input.get(t) {
                Some(children) => {
                    for (father, type_arg) in children {
                        if type_arg.iter().any(|ty| matches!(ty, Type::TypeVar(_))) {
                            let new_type = Self::solve_type_vars(input, father, type_arg);
                            result.insert(t.to_string(), new_type);
                        }
                    }
                }
                None => continue,
            }
        }

        result
    }

    fn solve_type_vars(
        input: &HashMap<&String, Vec<(&String, &Vec<Type>)>>,
        parent: &String,
        type_args: &Vec<Type>,
    ) -> HashSet<Vec<Type>> {
        let mut result = HashSet::new();
        match input.get(parent) {
            Some(children) => {
                for (child, c_type_arg) in children {
                    let mut unifier: Unifier = Default::default();

                    let tuple_args = Type::Tuple(TupleType {
                        items: type_args.to_vec(),
                    });

                    let c_tuple_arg = Type::Tuple(TupleType {
                        items: c_type_arg.to_vec(),
                    });

                    let res = unifier.unify_types(tuple_args, c_tuple_arg);
                    if res.is_ok() {
                        let new_type_arg = c_type_arg
                            .iter()
                            .zip(type_args.iter())
                            .map(|(a, b)| match (a, b) {
                                (Type::TypeVar(_), Type::TypeVar(_)) => a.clone(),
                                (Type::TypeVar(_), _) => b.clone(),
                                (_, Type::TypeVar(_)) => a.clone(),
                                _ => a.clone(),
                            })
                            .collect();

                        let mut res = Self::solve_type_vars(input, child, &new_type_arg);
                        if new_type_arg
                            .iter()
                            .all(|ty| !matches!(ty, Type::TypeVar(_)))
                        {
                            res.insert(new_type_arg);
                        }
                        result.extend(res);
                    }
                }
            }
            None => {
                result.insert(type_args.to_vec());
            }
        }

        result
    }
}
