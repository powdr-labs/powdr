use std::collections::{HashMap, HashSet};

use powdr_ast::parsed::{
    types::{Type, TypeScheme},
    visitor::Children,
};

use crate::type_builtins::elementary_type_bounds;

// TODO Optimization ideas:
// the substitutions are applied a lot.
// 1) One idea would be to extract a separate substitution map
// that contains only the newly added substitutions during a unification run. Then we only have to use
// that smaller map during the recursive substitutions.
// 2) We could have a wrapper around type that stores which type variables are contained in it.
// This way we can exit early if we know that all contained type vars are substituted.
// This might be especially useful inside add_substitution.

#[derive(Default, Clone)]
pub struct Unifier {
    /// Inferred type constraints (traits) on type variables.
    type_var_bounds: HashMap<String, HashSet<String>>,
    /// Substitutions for type variables
    substitutions: HashMap<String, Type>,
    /// Last used type variable index.
    last_type_var: usize,
}

impl Unifier {
    pub fn new() -> Self {
        Self {
            type_var_bounds: HashMap::new(),
            substitutions: HashMap::new(),
            last_type_var: 0,
        }
    }

    pub fn substitutions(&self) -> &HashMap<String, Type> {
        &self.substitutions
    }

    pub fn type_var_bounds(&self, type_var: &String) -> HashSet<String> {
        self.type_var_bounds
            .get(type_var)
            .cloned()
            .unwrap_or_default()
    }

    pub fn ensure_bound(&mut self, ty: &Type, bound: String) -> Result<(), String> {
        let ty = (if let Type::TypeVar(n) = ty {
            self.substitutions.get(n)
        } else {
            None
        })
        .unwrap_or(ty);

        match ty {
            Type::TypeVar(n) => {
                self.add_type_var_bound(n.clone(), bound);
            }
            Type::Array(_) | Type::Tuple(_) if bound == "ToString" => {
                // TODO Change this to a proper trait impl later.
                for c in ty.clone().children().collect::<Vec<_>>() {
                    self.ensure_bound(c, "ToString".to_string())?;
                }
            }
            Type::NamedType(n, _) => {
                // Change this as soon as we support user-implemented traits.
                return Err(format!("Type {n} does not satisfy trait {bound}."));
            }
            _ => {
                let bounds = elementary_type_bounds(ty);
                if !bounds.contains(&bound.as_str()) {
                    return Err(format!("Type {ty} does not satisfy trait {bound}."));
                }
            }
        }
        Ok(())
    }

    pub fn unify_types(&mut self, mut inner: Type, mut expected: Type) -> Result<(), String> {
        inner.substitute_type_vars(&self.substitutions);
        expected.substitute_type_vars(&self.substitutions);

        if inner == expected {
            return Ok(());
        }
        match (inner, expected) {
            (Type::Bottom, _) | (_, Type::Bottom) => Ok(()),
            (Type::TypeVar(name), ty) | (ty, Type::TypeVar(name)) => {
                // TODO this is the reason why we have to re-substitute for each
                // recursive call.
                // Maybe it would be better to store those and only apply them?
                self.add_substitution(name, ty)
            }
            (Type::Function(f1), Type::Function(f2)) => {
                if f1.params.len() != f2.params.len() {
                    return Err(format!(
                        "Function types have different number of parameters: {f1} and {f2}"
                    ));
                }
                for (p1, p2) in f1.params.into_iter().zip(f2.params.into_iter()) {
                    self.unify_types(p1, p2)?;
                }
                self.unify_types(*f1.value, *f2.value)
            }
            (Type::Array(a1), Type::Array(a2)) => {
                if a1.length != a2.length {
                    return Err(format!("Array types have different lengths: {a1} and {a2}"));
                }
                self.unify_types(*a1.base, *a2.base)
            }
            (Type::Tuple(t1), Type::Tuple(t2)) => {
                if t1.items.len() != t2.items.len() {
                    return Err(format!(
                        "Tuple types have different number of items: {t1} and {t2}"
                    ));
                }
                t1.items
                    .into_iter()
                    .zip(t2.items)
                    .try_for_each(|(i1, i2)| self.unify_types(i1, i2))
            }
            (Type::NamedType(n1, Some(args1)), Type::NamedType(n2, Some(args2))) if n1 == n2 => {
                // The "None"-part is already handled with the equality comparison.
                assert!(!args1.is_empty() && !args2.is_empty());
                args1
                    .into_iter()
                    .zip(args2)
                    .try_for_each(|(a1, a2)| self.unify_types(a1, a2))
            }
            (ty1, ty2) => Err(format!("Cannot unify types {ty1} and {ty2}")),
        }
    }

    fn add_type_var_bound(&mut self, type_var: String, bound: String) {
        self.type_var_bounds
            .entry(type_var)
            .or_default()
            .insert(bound);
    }

    fn add_substitution(&mut self, type_var: String, ty: Type) -> Result<(), String> {
        if ty.contains_type_var(&type_var) {
            return Err(format!(
                "Cannot unify types {ty} and {type_var}: They depend on each other"
            ));
        }
        let subs = [(type_var.clone(), ty.clone())].into();

        for bound in self.type_var_bounds(&type_var) {
            self.ensure_bound(&ty, bound)?;
        }

        self.substitutions
            .values_mut()
            .for_each(|t| t.substitute_type_vars(&subs));
        self.substitutions.insert(type_var, ty);
        Ok(())
    }

    pub fn instantiate_scheme(&mut self, scheme: TypeScheme) -> (Type, Vec<Type>) {
        let mut ty = scheme.ty;
        let vars = scheme
            .vars
            .bounds()
            .map(|(_, bounds)| {
                let new_var = self.new_type_var();
                for b in bounds {
                    self.ensure_bound(&new_var, b.clone()).unwrap();
                }
                new_var
            })
            .collect::<Vec<_>>();
        let substitutions = scheme.vars.vars().cloned().zip(vars.clone()).collect();
        ty.substitute_type_vars(&substitutions);
        (ty, vars)
    }

    pub fn new_type_var_name(&mut self) -> String {
        self.last_type_var += 1;
        format!("T{}", self.last_type_var)
    }

    fn new_type_var(&mut self) -> Type {
        Type::TypeVar(self.new_type_var_name())
    }
}
