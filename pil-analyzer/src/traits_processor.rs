use std::collections::{HashMap, HashSet};

use powdr_ast::{
    analyzed::{Expression, FunctionValueDefinition, Symbol},
    parsed::{
        types::{TupleType, Type, TypeScheme},
        TraitDeclaration, TraitImplementation,
    },
};

use crate::type_unifier::Unifier;

/// Checks for overlapping trait implementations in the current `PILAnalyzer` instance.
///
/// This method iterates through all the trait implementations.
/// For each implementation, it checks that there are no traits with overlapping type vars and the same name between them.
/// Overlapping occurs when there are two implementations of the same trait whose trait types can be unified.
///
/// It also checks that the number of type variables in the implementation matches
/// the number of type variables in the corresponding trait declaration.
pub fn check_traits_overlap(
    implementations: &mut HashMap<String, Vec<TraitImplementation<Expression>>>,
    definitions: &HashMap<String, (Symbol, Option<FunctionValueDefinition>)>,
) {
    for trait_impls in implementations.values_mut() {
        // All the impls in trait_impls are of the same trait declaration.
        let trait_name = trait_impls[0].name.clone();
        let trait_decl = definitions
            .get(&trait_name)
            .unwrap_or_else(|| panic!("Trait {trait_name} not found"))
            .1
            .as_ref()
            .unwrap_or_else(|| panic!("Trait definition for {trait_name} not found"));

        let trait_decl = if let FunctionValueDefinition::TraitDeclaration(trait_decl) = trait_decl {
            trait_decl
        } else {
            panic!("Invalid trait declaration");
        };

        validate_impl_definitions(trait_impls, trait_decl);
        ensure_unique_impls(trait_impls);
    }
}

/// Validates the trait implementation definitions in the given `implementations` map against the trait
/// declarations in the `definitions` map.
fn validate_impl_definitions(
    implementations: &[TraitImplementation<Expression>],
    trait_decl: &TraitDeclaration,
) {
    for trait_impl in implementations {
        let Type::Tuple(TupleType { items: mut types }) = trait_impl.type_scheme.ty.clone() else {
            panic!("Type from trait scheme is not a tuple.")
        };
        let trait_name = trait_impl.name.clone();

        if types.len() != trait_decl.type_vars.len() {
            panic!(
                "{}",
                trait_impl.source_ref.with_error(format!(
                    "Trait {} has {} type parameters, but implementation has {}",
                    trait_name,
                    trait_decl.type_vars.len(),
                    types.len(),
                ))
            );
        }

        let type_vars: HashSet<_> = trait_impl.type_scheme.vars.vars().collect();

        let type_vars_in_tuple: Vec<_> = types
            .iter_mut()
            .flat_map(|t| {
                t.map_to_type_vars(&type_vars);
                t.contained_type_vars()
            })
            .collect();

        let type_vars_in_scheme: Vec<_> = trait_impl.type_scheme.vars.vars().collect();

        for var in type_vars_in_scheme {
            if !type_vars_in_tuple.contains(&var) {
                panic!(
                    "{}",
                    trait_impl.source_ref.with_error(format!(
                        "Impl {trait_name} introduces a type variable {var} that is not used",
                    ))
                );
            }
        }
    }
}

/// Ensures that there are no overlapping trait implementations in the given `implementations` map.
///
/// This function iterates through all the trait implementations comparing them with each other and checks that
/// there are no traits with overlapping type variables.
fn ensure_unique_impls(implementations: &mut [TraitImplementation<Expression>]) {
    for i in 0..implementations.len() {
        let type_vars: HashSet<_> = implementations[i].type_scheme.vars.vars().collect();
        implementations[i]
            .type_scheme
            .ty
            .map_to_type_vars(&type_vars);

        for j in (i + 1)..implementations.len() {
            let type_vars: HashSet<_> = implementations[j].type_scheme.vars.vars().collect();
            implementations[j]
                .type_scheme
                .ty
                .map_to_type_vars(&type_vars);

            unify_traits_types(
                implementations[i].type_scheme.clone(),
                implementations[j].type_scheme.clone(),
            )
            .map_err(|err| {
                implementations[i]
                    .source_ref
                    .with_error(format!("Impls for {}: {err}", implementations[i].name))
            })
            .unwrap()
        }
    }
}

fn unify_traits_types(ty1: TypeScheme, ty2: TypeScheme) -> Result<(), String> {
    let mut type_var_manager = TypeVarManager::new();
    let instantiated_ty1 = type_var_manager.instantiate_scheme(ty1);
    let instantiated_ty2 = type_var_manager.instantiate_scheme(ty2);

    match Unifier::default().unify_types(instantiated_ty1.clone(), instantiated_ty2.clone()) {
        Ok(_) => Err(format!(
            "Types {instantiated_ty1} and {instantiated_ty2} overlap"
        )),
        Err(_) => Ok(()),
    }
}

pub struct TypeVarManager {
    last_type_var: usize,
}

impl TypeVarManager {
    pub fn new() -> Self {
        Self { last_type_var: 0 }
    }

    pub fn instantiate_scheme(&mut self, scheme: TypeScheme) -> Type {
        let mut substitutions = HashMap::new();
        for var in scheme.vars.vars() {
            substitutions.insert(var.clone(), self.fresh_type_var());
        }
        scheme.ty.substitute_type_vars_to(&substitutions)
    }

    pub fn fresh_type_var(&mut self) -> Type {
        self.last_type_var += 1;
        Type::TypeVar(format!("T{}", self.last_type_var))
    }
}
