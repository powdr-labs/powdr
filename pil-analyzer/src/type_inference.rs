use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

use itertools::Itertools;
use powdr_ast::{
    analyzed::{
        types::{ArrayType, FunctionType, Type, TypeScheme, TypedExpression},
        Expression, FunctionValueDefinition, Identity, IdentityKind, PolynomialReference,
        PolynomialType, Reference, Symbol, SymbolKind,
    },
    parsed::{
        ArrayLiteral, BinaryOperator, FunctionCall, IndexAccess, LambdaExpression, MatchArm,
        MatchPattern, TypeBounds, UnaryOperator,
    },
};
use powdr_number::{FieldElement, GoldilocksField};
use powdr_parser::{parse_type_name, parse_type_var_bounds};

use crate::call_graph::sort_called_first;

// TODO in the end, this needs to modify the expressions,
// because it has to ineject implict conversions
// and it has to change generic trait function calls to concrete ones
// and it might also need to add the FromLiteral calls.

pub fn infer_types<T: FieldElement>(
    definitions: &HashMap<String, (Symbol, Option<FunctionValueDefinition<T>>)>,
    identities: &Vec<Identity<Expression<T>>>,
) -> Result<BTreeMap<String, TypeScheme>, String> {
    let definitions = definitions
        .iter()
        .map(|(n, (symbol, value))| {
            let (ty, value) = type_and_expr_from_definition(symbol, value);
            assert!(ty.is_some() || value.is_some());
            (n.clone(), (ty, value))
        })
        .collect();
    TypeChecker::default().infer_types(&definitions, identities)
}

/// Extracts the declared type and the expression from a definition.
fn type_and_expr_from_definition<'a, T>(
    symbol: &'a Symbol,
    value: &'a Option<FunctionValueDefinition<T>>,
) -> (Option<TypeScheme>, Option<&'a Expression<T>>) {
    // TODO extract his function to a different type so that type_inference.rs only needs to deal with Expressions
    if let Some(value) = value {
        match value {
            FunctionValueDefinition::Array(_) => (Some(Type::col().into()), None), // TODO we could check inisde the array
            FunctionValueDefinition::Query(_) => (Some(Type::col().into()), None), // TODO we could check inisde the query string
            FunctionValueDefinition::Expression(TypedExpression { e, type_scheme }) => {
                (type_scheme.clone(), Some(e))
            }
        }
    } else {
        assert!(
            symbol.kind == SymbolKind::Poly(PolynomialType::Committed)
                || symbol.kind == SymbolKind::Poly(PolynomialType::Constant)
        );
        if let Some(_) = symbol.length {
            // TODO fixed length arrays?
            (
                Some(
                    Type::Array(ArrayType {
                        base: Box::new(Type::col()),
                        length: None,
                    })
                    .into(),
                ),
                None,
            )
        } else {
            (Some(Type::col().into()), None)
        }
    }
}

#[derive(Default)]
struct TypeChecker {
    /// Declared types for symbols. Type scheme for polymorphic symbols
    /// and unquantified type variables for symbols without type.
    types: HashMap<String, TypeScheme>,
    state: TypeCheckerState,
    last_type_var: usize,
}

#[derive(Default, Clone)]
struct TypeCheckerState {
    /// Types for local variables, might contain type variables.
    local_var_types: Vec<Type>,
    /// Inferred type constraints (traits) on type variables.
    type_var_bounds: HashMap<String, HashSet<String>>,
    /// Substitutions for type variables
    substitutions: HashMap<String, Type>,
}

impl TypeCheckerState {
    pub fn substitutions(&self) -> &HashMap<String, Type> {
        &self.substitutions
    }

    pub fn type_var_bounds(&self, type_var: &String) -> HashSet<String> {
        self.type_var_bounds
            .get(type_var)
            .cloned()
            .unwrap_or_default()
    }

    pub fn add_type_var_bound(&mut self, type_var: String, bound: String) {
        self.type_var_bounds
            .entry(type_var)
            .or_insert_with(HashSet::new)
            .insert(bound);
    }

    pub fn local_var_type(&self, id: u64) -> Type {
        self.local_var_types[id as usize].clone()
    }

    pub fn push_new_local_vars(&mut self, types: Vec<Type>) {
        self.local_var_types = [types, self.local_var_types.clone()].concat();
    }

    pub fn pop_local_var_types(&mut self, count: usize) -> Vec<Type> {
        self.local_var_types.drain(0..count).collect()
    }

    pub fn add_substitution(&mut self, type_var: String, ty: Type) {
        let subs = [(type_var.clone(), ty.clone())].into();
        self.substitutions
            .values_mut()
            .for_each(|t| t.substitute_type_vars(&subs));
        //println!("Adding substitution: {name} := {ty}");
        self.substitutions.insert(type_var, ty);
    }

    fn unify_types(&mut self, mut ty1: Type, mut ty2: Type) -> Result<(), String> {
        //println!("Unify start: {ty1}  <->  {ty2}");
        if let (Type::TypeVar(n1), Type::TypeVar(n2)) = (&ty1, &ty2) {
            if n1 == n2 {
                return Ok(());
            }
        }
        // TODO this should not be needed for recursive calls, should it?
        ty1.substitute_type_vars(&self.substitutions);
        ty2.substitute_type_vars(&self.substitutions);

        // TODO erm, do we really need the is_elementory check here?
        if ty1.is_elementary() && ty2.is_elementary() && ty1 == ty2 {
            return Ok(());
        }
        match (ty1, ty2) {
            (Type::Bottom, _) | (_, Type::Bottom) => Ok(()),
            (Type::TypeVar(n1), Type::TypeVar(n2)) if n1 == n2 => Ok(()),
            (Type::TypeVar(name), ty) | (ty, Type::TypeVar(name)) => {
                if ty.contains_type_var(&name) {
                    Err(format!(
                        "Cannot unify types {ty} and {name}: They depend on each other"
                    ))
                } else {
                    for bound in self.type_var_bounds(&name) {
                        self.ensure_bound(&ty, bound)?;
                    }
                    self.add_substitution(name, ty);
                    Ok(())
                }
            }
            (Type::Function(f1), Type::Function(f2)) => {
                if f1.params.len() != f2.params.len() {
                    Err(format!(
                        "Function types have different number of parameters: {f1} and {f2}"
                    ))?;
                }
                for (p1, p2) in f1.params.iter().zip(f2.params.iter()) {
                    self.unify_types(p1.clone(), p2.clone())?;
                }
                self.unify_types(*f1.value, *f2.value)
            }
            (Type::Array(a1), Type::Array(a2)) => {
                if a1.length != a2.length {
                    Err(format!("Array types have different lengths: {a1} and {a2}"))?;
                }
                self.unify_types(*a1.base, *a2.base)
            }
            (Type::Tuple(t1), Type::Tuple(t2)) => {
                if t1.items.len() != t2.items.len() {
                    Err(format!(
                        "Tuple types have different number of items: {t1} and {t2}"
                    ))?;
                }
                t1.items
                    .into_iter()
                    .zip(t2.items)
                    .try_for_each(|(i1, i2)| self.unify_types(i1.clone(), i2.clone()))
            }

            (ty1, ty2) => Err(format!("Cannot unify types {ty1} and {ty2}")),
        }
    }

    fn ensure_bound(&mut self, ty: &Type, bound: String) -> Result<(), String> {
        //println!("Ensuring type bound {ty}: {bound}");
        if let Type::TypeVar(n) = ty {
            self.add_type_var_bound(n.clone(), bound);
            Ok(())
        } else {
            let bounds = elementary_type_bounds(&ty);
            if bounds.contains(&bound.as_str()) {
                Ok(())
            } else {
                Err(format!(
                    "Type {ty} is required to satisfy trait {bound}, but does not."
                ))
            }
        }
    }
}

impl TypeChecker {
    pub fn infer_types<T: FieldElement>(
        mut self,
        definitions: &HashMap<String, (Option<TypeScheme>, Option<&Expression<T>>)>,
        identities: &Vec<Identity<Expression<T>>>,
    ) -> Result<BTreeMap<String, TypeScheme>, String> {
        let builtins = builtin_schemes();
        // TODO we should actually cross-check them with the definitions and not just override!
        // TODO we should make self.types immutable after construction.
        self.types = builtins.clone();
        // Add types from declarations. Type schemes are added without instantiating.
        for (name, (declared_type_scheme, _)) in definitions {
            if builtins.contains_key(name) {
                continue;
            }
            // This stores an (uninstantiated) type scheme for symbols with a declared
            // polymorphic type and it creates a new (unquantified) type variable for
            // symbols without declared type. This forces a single concrete type for the latter.
            let ty = declared_type_scheme
                .clone()
                .unwrap_or_else(|| self.new_type_var().into());
            self.types.insert(name.clone(), ty);
        }

        self.infer_types_inner(definitions, identities)
        // .map_err(|err| {
        //     format!(
        //         "{err}\n\nInferred types:\n{}",
        //         self.inferred_types()
        //             .iter()
        //             .map(|(n, t)| format!("let{} {n}: {}", t.type_vars_to_string(), t.ty))
        //             .format("\n")
        //     )
        // })
    }

    fn infer_types_inner<T: FieldElement>(
        &mut self,
        definitions: &HashMap<String, (Option<TypeScheme>, Option<&Expression<T>>)>,
        identities: &Vec<Identity<Expression<T>>>,
    ) -> Result<BTreeMap<String, TypeScheme>, String> {
        // TODO we should probably create a call graph and check according to that (inner to outer).
        // but how would that work for local variables? Create a call graph for the locals as well, once we check the function?

        // Now check the declarations for consistency or derive a concrete type
        // (or check against consistency of the type scheme in the declaration).
        let names = sort_called_first(
            definitions
                .iter()
                .map(|(n, (_, e))| (n.as_str(), e.clone())),
        );

        // TODO in order to fix type inference on recursive functions, we need to:
        // - collect all groups of functions that call each other recursively
        // - analyze each such group in an environment, where their type schemes
        //   are instantiated once at the start and not anymore for the symbol lookup.

        for name in names {
            if builtin_schemes().contains_key(name) {
                continue;
            }
            let (_, Some(value)) = definitions[&name.to_string()] else {
                continue;
            };

            let ty = self.instantiate_scheme(self.types[name].clone());
            //let snapshot = self.state.clone();
            self.unify_expression(ty.clone(), value)?;
            // (match self.unify_expression_allow_implicit_conversion(ty.clone(), value) {
            //     Err(err) if self.substitute_to(ty.clone()) == Type::col() => {
            //         self.state = snapshot;
            //         // Ok, this did not work, but we are expecting a column.
            //         // Let's see if we can add an implicit conversion int -> fe
            //         // at the end, i.e. type check it as int- > int.
            //         self.unify_expression(
            //             Type::Function(FunctionType {
            //                 params: vec![Type::Int],
            //                 value: Box::new(Type::Int),
            //             }),
            //             value,
            //         )
            //         .map_err(|_| err)
            //     }
            //     x => x,
            // })
            // .map_err(|e| format!("Error type checking the symbol {name} = {value}:\n{e}",))?;
            if !self.types[name].vars.is_empty() {
                // It is a type scheme, so we need to check if it conforms to its declared type now.
                // TODO the instantiation adds type var bounds. But this routine cannot check if some
                // of the bounds are actually not needed.
                // So maybe we should instantiate the scheme differently? Or maybe just start with a fresh type if it is a scheme?
                let inferred = self.to_type_scheme(ty);
                let declared = self.types[name].clone().simplify_type_vars();
                if inferred != declared {
                    // TODO we could also collect those.
                    Err(format!(
                        "Inferred type scheme for symbol {name} does not match the declared type.\nInferred: let{} {name}: {}\nDeclared: let{} {name}: {}",
                        inferred.type_vars_to_string(),
                        inferred.ty,
                        self.types[name].type_vars_to_string(),
                        self.types[name].ty
                    ))?;
                }
            }
        }

        self.check_identities(identities)?;

        // for name in definitions.keys() {
        //     // TODO in the future, we just add them as type schemes.
        //     if builtin_schemes().contains_key(name) {
        //         continue;
        //     }
        //     let ty = self.substitute_to(self.derived_types[name].clone());
        //     if !ty.contained_type_vars().is_empty() {
        //         let scheme = self.to_type_scheme(ty.clone()).simplify_type_vars();
        //         Err(format!(
        //         "Could not determine a concrete type for symbol {name}.\nInferred type scheme: {} {}",
        //         scheme.type_vars_to_string(),
        //         scheme.ty
        //     ))?;
        //     }
        //     println!("{name} -> {ty}");
        // }
        for name in definitions.keys() {
            if self.types[name].vars.is_empty() {
                // It is not a type scheme, see if we were able to derive a concrete type.
                let inferred = self.substitute_to(self.types[name].ty.clone());
                if !inferred.is_concrete_type() {
                    let inferred_scheme = self.to_type_scheme(inferred);
                    Err(format!(
                        "Could not derive a concrete type for symbol {name}.\nInferred type scheme: {} {}\n",
                        inferred_scheme.type_vars_to_string(),
                        inferred_scheme.ty
                    ))?;
                }
            }
        }

        Ok(self.inferred_types())
    }

    fn check_identities<T: FieldElement>(
        &mut self,
        identities: &[Identity<Expression<T>>],
    ) -> Result<(), String> {
        for id in identities {
            if id.kind == IdentityKind::Polynomial {
                self.unify_expression(Type::Constr, id.expression_for_poly_id())?;
                // let snapshot = self.state.clone();
                // match self.unify_expression(Type::Constr, id.expression_for_poly_id()) {
                //     Ok(()) => Ok(()),
                //     Err(original_err) => {
                //         // Unification with constr failed, let's try constr[].
                //         self.state = snapshot;
                //         self.unify_expression(
                //             Type::Array(ArrayType {
                //                 base: Box::new(Type::Constr),
                //                 length: None,
                //             }),
                //             id.expression_for_poly_id(),
                //         )
                //         .map_err(|_| original_err)
                //     }
                // }
                // .map_err(|e| {
                //     format!("Expresison is expected to evaluate to a constraint: {id}:\n{e}")
                // })?;
            } else {
                for part in [&id.left, &id.right] {
                    if let Some(selector) = &part.selector {
                        self.unify_expression_allow_implicit_conversion(Type::Expr, selector)
                            .map_err(|e| {
                                format!("Selector is expected to evaluate to an algebraic expresison: {selector}:\n{e}")
                            })?;
                    }
                    for e in &part.expressions {
                        self.unify_expression_allow_implicit_conversion(Type::Expr, e)
                            .map_err(|err| {
                                format!("Expression in lookup is expected to evaluate to an algebraic expresison: {e}:\n{err}")
                            })?;
                    }
                }
            }
        }
        Ok(())
    }

    fn inferred_types(&mut self) -> BTreeMap<String, TypeScheme> {
        let types = self.types.clone();
        types
            .iter()
            .map(|(name, ty)| {
                // TODO for concrete types we want to run substitution.
                // for type schemes we don't. Maybe implement substitution also on type schemes?
                let ty = self.instantiate_scheme(ty.clone());
                (
                    name.clone(),
                    //TODO this is probably wrong
                    self.to_type_scheme(ty),
                )
            })
            .collect()
    }

    fn unify_new_expression<T: FieldElement>(&mut self, e: &Expression<T>) -> Result<Type, String> {
        let ty = self.new_type_var();
        self.unify_expression(ty.clone(), e)?;
        //println!("Type of {e}: {}", self.substitute_to(ty.clone()));
        Ok(ty)
    }

    fn unify_expression_allow_implicit_conversion<T: FieldElement>(
        &mut self,
        ty: Type,
        e: &Expression<T>,
    ) -> Result<(), String> {
        let expr_type = self.unify_new_expression(e)?;
        self.state.unify_types(ty.clone(), expr_type.clone())
        //let snapshot = self.state.clone();
        // match self.state.unify_types(ty.clone(), expr_type.clone()) {
        //     Err(e) => {
        //         if self.substitute_to(ty.clone()) == Type::Expr
        //             && self.substitute_to(expr_type.clone()) == Type::col()
        //         {
        //             // TODO is it OK to check for col?
        //             //println!("Unification faild, but we are expecting an 'expr' type and have a 'col'. Trying to add conversion to expr.");
        //             // Ok try to convert the col to an expr
        //             self.state = snapshot;
        //             self.state.unify_types(Type::col(), expr_type)?;
        //             let converted = self.new_type_var();
        //             self.state.unify_types(Type::Expr, converted.clone())?;
        //             self.state.unify_types(ty, converted)
        //         } else {
        //             Err(e)
        //         }
        //     }
        //     Ok(_) => Ok(()),
        // }
    }

    fn unify_expression<T: FieldElement>(
        &mut self,
        ty: Type,
        e: &Expression<T>,
    ) -> Result<(), String> {
        // println!(
        //     "Unifying {e}, expecting it to be {}",
        //     self.substitute_to(ty.clone())
        // );
        match e {
            Expression::Reference(Reference::LocalVar(id, _name)) => {
                self.state.unify_types(ty, self.state.local_var_type(*id))
            }
            Expression::Reference(Reference::Poly(PolynomialReference { name, poly_id: _ })) => {
                let type_of_symbol = self.instantiate_scheme(self.types[name].clone());
                self.state.unify_types(ty, type_of_symbol)
            }
            Expression::PublicReference(_) => todo!(),
            Expression::Number(_) => self.state.ensure_bound(&ty, "FromLiteral".to_string()),
            Expression::String(_) => self.state.unify_types(ty, Type::String),
            Expression::Tuple(_) => todo!(),
            Expression::LambdaExpression(LambdaExpression { params, body }) => {
                let param_types = (0..params.len())
                    .map(|_| self.new_type_var())
                    .collect::<Vec<_>>();
                self.state.push_new_local_vars(param_types);
                let body_type = self.unify_new_expression(body)?;
                let param_types = self.state.pop_local_var_types(params.len());
                self.state.unify_types(
                    ty,
                    Type::Function(FunctionType {
                        params: param_types,
                        value: Box::new(body_type),
                    }),
                )
            }
            Expression::ArrayLiteral(ArrayLiteral { items }) => {
                let item_type = self.new_type_var();
                self.state.unify_types(
                    ty,
                    Type::Array(ArrayType {
                        base: Box::new(item_type.clone()),
                        length: None,
                    }),
                )?;
                items
                    .iter()
                    .try_for_each(|e| self.unify_expression(item_type.clone(), e))
            }
            Expression::BinaryOperation(left, op, right) => {
                let fun_type = self.instantiate_scheme(binary_operator_scheme(*op));
                let value = self
                    .unify_function_call(fun_type, [left, right].into_iter().map(AsRef::as_ref))?;
                self.state.unify_types(ty, value)?;
                Ok(())
            }
            Expression::UnaryOperation(op, inner) => {
                let fun_type = self.instantiate_scheme(unary_operator_scheme(*op));
                let value =
                    self.unify_function_call(fun_type, [inner].into_iter().map(AsRef::as_ref))?;
                self.state.unify_types(ty, value)?;
                Ok(())
            }
            Expression::IndexAccess(IndexAccess { array, index }) => {
                let array_type = self.unify_new_expression(array)?;
                self.state.unify_types(
                    array_type,
                    Type::Array(ArrayType {
                        base: Box::new(ty.clone()),
                        length: None,
                    }),
                )?;

                let index_type = self.unify_new_expression(index)?;
                self.state.unify_types(index_type, Type::Int)
            }
            Expression::FunctionCall(FunctionCall {
                function,
                arguments,
            }) => {
                let ft = self.unify_new_expression(function)?;
                let value = self.unify_function_call(ft, arguments.iter())?;
                self.state.unify_types(ty, value)?;
                Ok(())
            }
            Expression::FreeInput(_) => todo!(),
            Expression::MatchExpression(scrutinee, arms) => {
                let scrutinee_type = self.unify_new_expression(&scrutinee)?;
                let arm_types = arms
                    .iter()
                    .map(|MatchArm { pattern, value }| {
                        if let MatchPattern::Pattern(pattern) = pattern {
                            let pat_type = self.unify_new_expression(pattern)?;
                            self.state.unify_types(scrutinee_type.clone(), pat_type)?;
                        }
                        self.unify_new_expression(value)
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                for arm_type in arm_types {
                    self.state.unify_types(ty.clone(), arm_type)?;
                }
                Ok(())
            }
            Expression::IfExpression(if_expr) => {
                let cond_type = self.unify_new_expression(&if_expr.condition)?;
                self.state.unify_types(cond_type, Type::Bool)?;
                let true_type = self.unify_new_expression(&if_expr.body)?;
                let false_type = self.unify_new_expression(&if_expr.else_body)?;
                self.state.unify_types(true_type.clone(), false_type)?;
                self.state.unify_types(ty, true_type)?;
                Ok(())
            }
        }
    }

    fn unify_function_call<'b, T: FieldElement>(
        &mut self,
        function_type: Type,
        arguments: impl Iterator<Item = &'b Expression<T>>,
    ) -> Result<Type, String> {
        let args = arguments
            .map(|a| self.unify_new_expression(a))
            .collect::<Result<Vec<_>, _>>()?;
        let value = self.new_type_var();
        // println!(
        //     "Unifying function call \"{function_type}\" with \"{} -> {value}\"",
        //     args.iter().format(", ")
        // );

        // TODO If we change unify_type to not change state (but instead return an additional substitution
        // and bounds), then we do not have to do the snapshot.

        // TODO This is more or less the algorithm by  Luo, which depends on the order
        // of evaluation. A better version is "Extending Hindley-Milner Type Inference with
        // "Coercive Structural Subtyping" by "Dmitriy Traytel, Stefan Berghofer, and Tobias Nipkow",
        // which does HM and collects subtying constraints during the process and then solves
        // these constraints only at the end.
        let snapshot = self.state.clone();
        match self.state.unify_types(
            function_type.clone(),
            Type::Function(FunctionType {
                params: args.clone(),
                value: Box::new(value.clone()),
            }),
        ) {
            Err(e) => Err(e),
            // {
            //     // TODO add much more conditions.
            //     let conversions = args
            //         .iter()
            //         .enumerate()
            //         .filter_map(|(i, x)| {
            //             self.can_convert_implicitly(&self.substitute_to(x.clone()))
            //                 .map(|t| (i, t))
            //         })
            //         .collect::<Vec<_>>();
            //     if conversions.is_empty() {
            //         return Err(e);
            //     }

            //     // println!("Unification faild, but we have a 'col' argument. Trying to add conversion to expr.");
            //     // Ok try to convert the col to an expr
            //     self.state = snapshot;
            //     let mut new_args = args;
            //     for (pos, converted) in conversions {
            //         new_args[pos] = converted;
            //     }
            //     self.state.unify_types(
            //         function_type,
            //         Type::Function(FunctionType {
            //             params: new_args,
            //             value: Box::new(value.clone()),
            //         }),
            //     )?;
            //     Ok(value)
            // }
            Ok(_) => Ok(value),
        }
    }

    fn can_convert_implicitly(&self, from: &Type) -> Option<Type> {
        if from == &Type::col() {
            Some(Type::Expr)
        } else if from
            == &Type::Array(ArrayType {
                base: Box::new(Type::col()),
                length: None,
            })
        {
            Some(Type::Array(ArrayType {
                base: Box::new(Type::Expr),
                length: None,
            }))
        } else {
            None
        }
    }

    fn substitute_to(&self, mut ty: Type) -> Type {
        ty.substitute_type_vars(&self.state.substitutions());
        ty
    }

    /// Instantiates a type scheme by creating new type variables for the quantified
    /// type variables in the scheme and adds the required trait bounds for the
    /// new type variables.
    fn instantiate_scheme(&mut self, scheme: TypeScheme) -> Type {
        let mut ty = scheme.ty;
        //println!("Instantiating scheme {ty}");
        let substitution = scheme
            .vars
            .bounds()
            .map(|(var, bounds)| {
                let new_var = self.new_type_var();
                for b in bounds {
                    self.state.ensure_bound(&new_var, b.clone()).unwrap();
                }
                (var.clone(), new_var)
            })
            .collect();
        ty.substitute_type_vars(&substitution);
        ty
    }

    fn new_type_var(&mut self) -> Type {
        self.last_type_var += 1;
        Type::TypeVar(format!("T{}", self.last_type_var))
    }

    fn to_type_scheme(&self, ty: Type) -> TypeScheme {
        // TODO this generalizes all type vars - is that correct?
        let ty = self.substitute_to(ty);
        let vars = TypeBounds::new(ty.contained_type_vars().into_iter().map(|v| {
            (
                v.clone(),
                self.state
                    .type_var_bounds(v)
                    .into_iter()
                    .collect::<BTreeSet<_>>(),
            )
        }));
        TypeScheme { vars, ty }.simplify_type_vars()
    }
}

fn builtin_schemes() -> HashMap<String, TypeScheme> {
    [
        ("std::array::len", ("T", "T[] -> int")),
        ("std::check::panic", ("", "string -> !")), // TODO should be T: ToString
        ("std::convert::fe", ("T: FromLiteral", "T -> fe")),
        ("std::convert::int", ("T: FromLiteral", "T -> int")),
        ("std::debug::print", ("", "string -> constr[]")),
        ("std::field::modulus", ("", "-> int")),
    ]
    .into_iter()
    .map(|(name, (vars, ty))| {
        (
            name.to_string(),
            TypeScheme {
                vars: parse_type_var_bounds(vars).unwrap(),
                ty: parse_type_name::<GoldilocksField>(ty).unwrap().into(),
            },
        )
    })
    .collect()
}

fn binary_operator_scheme(op: BinaryOperator) -> TypeScheme {
    let (vars, ty) = match op {
        BinaryOperator::Add => ("T: Add", "T, T -> T"),
        BinaryOperator::Sub => ("T: Sub", "T, T -> T"),
        BinaryOperator::Mul => ("T: Mul", "T, T -> T"),
        BinaryOperator::Div => ("", "int, int -> int"),
        BinaryOperator::Mod => ("", "int, int -> int"),
        BinaryOperator::Pow => ("T: Pow", "T, int -> T"),
        BinaryOperator::ShiftLeft => ("", "int, int -> int"),
        BinaryOperator::ShiftRight => ("", "int, int -> int"),
        BinaryOperator::BinaryAnd => ("", "int, int -> int"),
        BinaryOperator::BinaryOr => ("", "int, int -> int"),
        BinaryOperator::BinaryXor => ("", "int, int -> int"),
        BinaryOperator::Less => ("T: Ord", "T, T -> bool"),
        BinaryOperator::LessEqual => ("T: Ord", "T, T -> bool"),
        BinaryOperator::Equal => ("T: Eq", "T, T -> bool"),
        BinaryOperator::Identity => ("", "Expr, Expr -> Constr"),
        BinaryOperator::NotEqual => ("T: Eq", "T, T -> bool"),
        BinaryOperator::GreaterEqual => ("T: Ord", "T, T -> bool"),
        BinaryOperator::Greater => ("T: Ord", "T, T -> bool"),
        BinaryOperator::LogicalOr => ("", "bool, bool -> bool"),
        BinaryOperator::LogicalAnd => ("", "bool, bool -> bool"),
    };
    TypeScheme {
        vars: parse_type_var_bounds(vars).unwrap(),
        ty: parse_type_name::<GoldilocksField>(ty).unwrap().into(),
    }
}

fn unary_operator_scheme(op: UnaryOperator) -> TypeScheme {
    let (vars, ty) = match op {
        UnaryOperator::Minus => ("T: Neg", "T -> T"),
        UnaryOperator::LogicalNot => ("", "bool -> bool"),
        UnaryOperator::Next => ("", "expr -> expr"),
    };
    TypeScheme {
        vars: parse_type_var_bounds(vars).unwrap(),
        ty: parse_type_name::<GoldilocksField>(ty).unwrap().into(),
    }
}

fn elementary_type_bounds(ty: &Type) -> Vec<&'static str> {
    match ty {
        Type::Bottom => vec![], // TODO or all of them?
        Type::Bool => vec![],
        Type::Int => vec![
            "FromLiteral",
            "Add",
            "Sub",
            "Neg",
            "Mul",
            "Div",
            "Mod",
            "Pow",
            "Ord",
            "Eq",
        ],
        Type::Fe => vec![
            "FromLiteral",
            "Add",
            "Sub",
            "Neg",
            "Mul",
            "Pow",
            "Neg",
            "Eq",
        ],
        Type::String => vec!["Add"],
        Type::Expr => vec![
            "FromLiteral",
            "Add",
            "Sub",
            "Neg",
            "Mul",
            "Pow",
            "Neg",
            "Eq",
        ],
        Type::Constr => vec![],
        Type::Array(_) => vec!["Add"],
        Type::Tuple(_) => vec![],
        Type::Function(_) => vec![],
        Type::TypeVar(_) => unreachable!(),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use powdr_number::GoldilocksField;

    use pretty_assertions::assert_eq;

    use crate::pil_analyzer::process_before_type_checking;

    fn parse_and_type_check(input: &str) -> Result<BTreeMap<String, TypeScheme>, String> {
        let (definitions, identities) = process_before_type_checking::<GoldilocksField>(input);
        infer_types(&definitions, &identities)
    }

    fn check(
        types: &Result<BTreeMap<String, TypeScheme>, String>,
        expected: &[(&str, &str, &str)],
    ) {
        let types = types
            .as_ref()
            .map_err(|e| {
                eprintln!("{e}");
                e
            })
            .unwrap();
        for (name, bounds, ty) in expected {
            let scheme = &types[&name.to_string()];
            assert_eq!(
                (*bounds, *ty),
                (
                    scheme.vars.to_string().as_str(),
                    scheme.ty.to_string().as_str()
                ),
                "Failure for symbol {name}"
            );
        }
    }

    #[test]
    fn single_literal() {
        let input = "let<T: FromLiteral> x: T[] = [1,2];";
        let result = parse_and_type_check(input);
        check(&result, &[("x", "T: FromLiteral", "T[]")]);
    }

    #[test]
    fn assignment() {
        // This should derive a concrete type for x due to how it is used by y.
        let input = "let x = [|i| i]; let y: int[] = [x[0](2)];";
        let result = parse_and_type_check(input);
        check(&result, &[("x", "", "(int -> int)[]"), ("y", "", "int[]")]);
    }

    #[test]
    fn higher_order() {
        let input = "
            let<T: Add + FromLiteral> x: T -> ((T -> T) -> T) = |i| |f| i + f(i);
            let<T: Add + FromLiteral> y: T = x(2)(|k| k + 8);
        ";
        let result = parse_and_type_check(input);
        check(
            &result,
            &[
                // TODO don't we need parentheses in the type here? How does it parse?
                ("x", "T: Add + FromLiteral", "T -> (T -> T) -> T"),
                ("y", "T: Add + FromLiteral", "T"),
            ],
        );
    }

    #[test]
    #[should_panic(expected = "Cannot unify types")]
    fn invalid_recursive() {
        let input = "let x = |i| |f| x(i);";
        parse_and_type_check(input).unwrap();
    }

    #[test]
    fn fold() {
        let input = "let<T1, T2> fold: int, (int -> T1), T2, (T2, T1 -> T2) -> T2 = |length, f, initial, folder|
            if length <= 0 {
                initial
            } else {
                folder(fold((length - 1), f, initial, folder), f((length - 1)))
            };";
        let result = parse_and_type_check(input);
        check(
            &result,
            &[(
                "fold",
                "T1, T2",
                "int, (int -> T1), T2, (T2, T1 -> T2) -> T2",
            )],
        );
    }

    #[test]
    #[should_panic(expected = "Inferred type scheme: <T: Add> T, T -> T")]
    fn sum() {
        let input = "let sum = |a, b| a + b;";
        parse_and_type_check(input).unwrap();
    }

    #[test]
    fn sum_via_fold() {
        let input = "
        let<T1, T2> fold: int, (int -> T1), T2, (T2, T1 -> T2) -> T2 = |length, f, initial, folder|
            if length <= 0 {
                initial
            } else {
                folder(fold((length - 1), f, initial, folder), f((length - 1)))
            };
        let<T: Add + FromLiteral> sum: int, (int -> T) -> T = |n, f| fold(n, f, 0, |a, b| a + b);
        ";
        parse_and_type_check(input).unwrap();
    }

    #[test]
    fn pow() {
        let input =
            "let<T: FromLiteral + Pow> pow: T, int -> T = |a, b| a ** b; let<T: FromLiteral + Pow> x: T = pow(2, 3);";
        let result = parse_and_type_check(input);
        check(
            &result,
            &[
                ("pow", "T: FromLiteral + Pow", "T, int -> T"),
                ("x", "T: FromLiteral + Pow", "T"),
            ],
        );
    }

    #[test]
    fn if_statement() {
        let input = "
            let g = || g();
            let x = |a, b| if g() { a } else { b + 2 };
            let c: int = 2;
            let y = [|i| x(c, i)];
        ";
        let result = parse_and_type_check(input);
        check(
            &result,
            // TODO " -> bool" is also not formatted correctly.
            &[
                ("g", "", " -> bool"),
                ("x", "", "int, int -> int"),
                ("c", "", "int"),
                ("y", "", "(int -> int)[]"),
            ],
        );
    }

    #[test]
    fn constraints() {
        let input = "
            let a;
            let BYTE = |i| std::convert::fe(i & 0xff);
            { a + 1 } in {BYTE};
            namespace std::convert(8);
            let fe = 8;
        ";
        let result = parse_and_type_check(input);
        check(&result, &[("a", "", "col"), ("BYTE", "", "col")]);
    }

    #[test]
    fn bottom() {
        let input = "
        namespace std::check(8);
            let panic: string -> ! = panic();
            let div: int, int -> int = |x, y| if y == 0 { panic(\"Division by zero\") } else { x / y };";
        let result = parse_and_type_check(input);
        check(&result, &[("std::check::div", "", "int, int -> int")]);
    }
}
