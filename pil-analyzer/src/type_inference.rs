use std::collections::{HashMap, HashSet};

use powdr_ast::{
    analyzed::{
        types::{FunctionType, Type, TypeScheme},
        Expression, FunctionValueDefinition, Identity, IdentityKind, PolynomialReference,
        Reference, Symbol,
    },
    parsed::{BinaryOperator, FunctionCall, LambdaExpression, UnaryOperator},
};
use powdr_number::{FieldElement, GoldilocksField};
use powdr_parser::{parse_type_name, parse_type_var_bounds};

pub fn infer_types<T: FieldElement>(
    definitions: &HashMap<String, (Symbol, Option<FunctionValueDefinition<T>>)>,
    identities: &Vec<Identity<Expression<T>>>,
) -> Result<HashMap<String, TypeScheme>, String> {
    TypeChecker::new(definitions).infer_types(identities)
}

struct TypeChecker<'a, T> {
    definitions: &'a HashMap<String, (Symbol, Option<FunctionValueDefinition<T>>)>,
    /// Types for symbols, might contain type variables.
    /// TODO could these be type schemes?
    types: HashMap<String, Type>,
    /// Types for local variables, might contain type variables.
    local_var_types: Vec<Type>,
    /// Inferred type constraints (traits) on type variables.
    type_var_bounds: HashMap<String, HashSet<String>>,
    /// Substitutions for type variables
    substitutions: HashMap<String, Type>,
    next_type_var: usize,
}

impl<'a, T: FieldElement> TypeChecker<'a, T> {
    pub fn new(
        definitions: &'a HashMap<String, (Symbol, Option<FunctionValueDefinition<T>>)>,
    ) -> Self {
        let mut tc = Self {
            definitions,
            types: HashMap::new(),
            local_var_types: vec![],
            type_var_bounds: HashMap::new(),
            substitutions: HashMap::new(),
            next_type_var: 1,
        };
        for n in definitions.keys() {
            // TODO do not just create new types.
            // if we create new types, this forces concrete types.
            // Instead, look at the definition and take the type schemes from there.
            // But we should not instantiate the type schemes, should we?
            let tv = tc.new_type_var();
            tc.types.insert(n.clone(), tv);
        }
        // TODO this whole system works by just restricting the types more and more.
        // when we look up a symbol, we cannot use the type annotation, instead,
        // we just infer the type and in the end compare the annotations...
        tc
    }

    fn infer_types(
        mut self,
        identities: &Vec<Identity<Expression<T>>>,
    ) -> Result<HashMap<String, TypeScheme>, String> {
        for (name, value) in self.definitions {
            let ty = self.types[name].clone();
            self.unify(&ty, value)?;
        }
        for id in identities {
            if id.kind == IdentityKind::Polynomial {
                self.unify_expression(Type::Constr, id.expression_for_poly_id())?;
            } else {
                for part in [&id.left, &id.right] {
                    if let Some(selector) = &part.selector {
                        self.unify_expression(Type::Expr, selector)?;
                    }
                    for e in &part.expressions {
                        self.unify_expression(Type::Expr, e)?;
                    }
                }
            }
        }
        Ok(self
            .types
            .into_iter()
            .map(|(name, mut ty)| {
                ty.substitute_type_vars(&self.substitutions);
                // TODO is this properly generalized?
                let vars = ty
                    .contained_type_vars()
                    .into_iter()
                    .map(|v| {
                        (
                            v.clone(),
                            self.type_var_bounds.get(v).cloned().unwrap_or_default(),
                        )
                    })
                    .collect();
                (name, TypeScheme { vars, ty }.simplify_type_vars())
            })
            .collect())
    }

    fn unify(
        &mut self,
        ty: &Type,
        value: &(Symbol, Option<FunctionValueDefinition<T>>),
    ) -> Result<(), String> {
        match (value.0.kind, value.0.length, &value.1) {
            (_, _, Some(FunctionValueDefinition::Expression(v))) => {
                self.unify_expression(ty.clone(), &v.e)
            } // TODO Type name
            (_, _, None) => self.unify_types(ty.clone(), Type::col()),
            _ => todo!(),
        }
    }

    fn unify_new_expression(&mut self, e: &Expression<T>) -> Result<Type, String> {
        let ty = self.new_type_var();
        self.unify_expression(ty.clone(), e)?;
        Ok(ty)
    }

    fn unify_expression(&mut self, ty: Type, e: &Expression<T>) -> Result<(), String> {
        //println!("Unifying {e}: {ty}");
        match e {
            Expression::Reference(Reference::LocalVar(id, _name)) => {
                self.unify_types(ty, self.local_var_types[*id as usize].clone())
            }
            Expression::Reference(Reference::Poly(PolynomialReference { name, poly_id: _ })) => {
                self.unify_types(ty, self.types[name].clone())
            }
            Expression::PublicReference(_) => todo!(),
            Expression::Number(_) => self.ensure_bound(&ty, "FromLiteral".to_string()),
            Expression::String(_) => self.unify_types(ty, Type::String),
            Expression::Tuple(_) => todo!(),
            Expression::LambdaExpression(LambdaExpression { params, body }) => {
                let param_types = (0..params.len())
                    .map(|_| self.new_type_var())
                    .collect::<Vec<_>>();
                self.local_var_types = [param_types, self.local_var_types.clone()].concat();
                let body_type = self.unify_new_expression(body)?;
                let param_types = self.local_var_types.drain(0..params.len()).collect();
                self.unify_types(
                    ty,
                    Type::Function(FunctionType {
                        params: param_types,
                        value: Box::new(body_type),
                    }),
                )
            }
            Expression::ArrayLiteral(_) => todo!(),
            Expression::BinaryOperation(left, op, right) => {
                let fun_type = self.instantiate_scheme(binary_operator_scheme(*op));
                let value = self
                    .unify_function_call(fun_type, [left, right].into_iter().map(AsRef::as_ref))?;
                self.unify_types(ty, value)?;
                Ok(())
            }
            Expression::UnaryOperation(op, inner) => {
                let fun_type = self.instantiate_scheme(unary_operator_scheme(*op));
                let value =
                    self.unify_function_call(fun_type, [inner].into_iter().map(AsRef::as_ref))?;
                self.unify_types(ty, value)?;
                Ok(())
            }
            Expression::IndexAccess(_) => todo!(),
            Expression::FunctionCall(FunctionCall {
                function,
                arguments,
            }) => {
                let ft = self.unify_new_expression(function)?;
                let value = self.unify_function_call(ft, arguments.iter())?;
                self.unify_types(ty, value)?;
                Ok(())
            }
            Expression::FreeInput(_) => todo!(),
            Expression::MatchExpression(_, _) => todo!(),
            Expression::IfExpression(if_expr) => {
                let cond_type = self.unify_new_expression(&if_expr.condition)?;
                self.unify_types(cond_type, Type::Bool)?;
                let true_type = self.unify_new_expression(&if_expr.body)?;
                let false_type = self.unify_new_expression(&if_expr.else_body)?;
                self.unify_types(true_type.clone(), false_type)?;
                self.unify_types(ty, true_type)?;
                Ok(())
            }
        }
    }

    fn unify_function_call<'b>(
        &mut self,
        function_type: Type,
        arguments: impl Iterator<Item = &'b Expression<T>>,
    ) -> Result<Type, String> {
        let args = arguments
            .map(|a| self.unify_new_expression(a))
            .collect::<Result<Vec<_>, _>>()?;
        let value = self.new_type_var();
        self.unify_types(
            function_type,
            Type::Function(FunctionType {
                params: args,
                value: Box::new(value.clone()),
            }),
        )?;
        Ok(value)
    }

    /// Applies the current substitutions to the type.
    fn substitute(&self, ty: &mut Type) {
        ty.substitute_type_vars(&self.substitutions);
    }

    /// Instantiates a type scheme by creating new type variables for the quantified
    /// type variables in the scheme and adds the required trait bounds for the
    /// new type variables.
    fn instantiate_scheme(&mut self, scheme: TypeScheme) -> Type {
        let mut ty = scheme.ty;
        //println!("Instantiating scheme {ty}");
        for (var, bounds) in scheme.vars {
            let new_var = self.new_type_var();
            ty.substitute_type_vars(&[(var.clone(), new_var.clone())].into());
            for b in bounds {
                self.ensure_bound(&new_var, b).unwrap();
            }
        }
        //println!("   -> instantiated to {ty}");
        ty
    }

    fn unify_types(&mut self, mut ty1: Type, mut ty2: Type) -> Result<(), String> {
        //println!("Unify start: {ty1}  <->  {ty2}");
        if let (Type::TypeVar(n1), Type::TypeVar(n2)) = (&ty1, &ty2) {
            if n1 == n2 {
                return Ok(());
            }
        }
        // TODO this should not be needed for recursive calls, should it?
        self.substitute(&mut ty1);
        self.substitute(&mut ty2);
        //println!("Unify {ty1}   <=>   {ty2}");
        match (ty1, ty2) {
            (Type::Bool, Type::Bool)
            | (Type::Int, Type::Int)
            | (Type::Fe, Type::Fe)
            | (Type::String, Type::String)
            | (Type::Expr, Type::Expr)
            | (Type::Constr, Type::Constr) => Ok(()),
            (Type::TypeVar(n1), Type::TypeVar(n2)) if n1 == n2 => Ok(()),
            (Type::TypeVar(name), ty) | (ty, Type::TypeVar(name)) => {
                if ty.contains_type_var(&name) {
                    Err(format!("Cannot unify types {ty} and {name}"))
                } else {
                    for bound in self.type_var_bounds.get(&name).cloned().unwrap_or_default() {
                        self.ensure_bound(&ty, bound)?;
                    }
                    let subs = [(name.clone(), ty.clone())].into();
                    self.substitutions
                        .values_mut()
                        .for_each(|t| t.substitute_type_vars(&subs));
                    //println!("Adding substitution: {name} := {ty}");
                    self.substitutions.insert(name, ty);
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
            self.type_var_bounds
                .entry(n.clone())
                .or_insert_with(HashSet::new)
                .insert(bound);
            Ok(())
        } else {
            let bounds = match ty {
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
                Type::Expr => vec!["Add", "Sub", "Neg", "Mul", "Pow", "Neg", "Eq"],
                Type::Constr => vec![],
                Type::Array(_) => vec!["Add"],
                Type::Tuple(_) => vec![],
                Type::Function(_) => vec![],
                Type::TypeVar(_) => unreachable!(),
            };
            if bounds.contains(&bound.as_str()) {
                Ok(())
            } else {
                Err(format!(
                    "Type {ty} is required to satisfy trait {bound}, but does not."
                ))
            }
        }
    }

    fn new_type_var(&mut self) -> Type {
        let name = format!("T{}", self.next_type_var);
        self.next_type_var += 1;
        Type::TypeVar(name)
    }
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

#[cfg(test)]
mod test {
    use super::*;

    use powdr_number::GoldilocksField;

    use pretty_assertions::assert_eq;

    use crate::pil_analyzer::process_before_type_checking;

    fn parse_and_type_check(input: &str) -> Result<HashMap<String, TypeScheme>, String> {
        let (definitions, identities) = process_before_type_checking::<GoldilocksField>(input);
        infer_types(&definitions, &identities)
    }

    fn check(types: &Result<HashMap<String, TypeScheme>, String>, expected: &[(&str, &str, &str)]) {
        let types = types.as_ref().unwrap();
        for (name, bounds, ty) in expected {
            let scheme = &types[&name.to_string()];
            assert_eq!(
                (*bounds, *ty),
                (
                    scheme.bounds_to_string().as_str(),
                    scheme.ty.to_string().as_str()
                ),
                "Failure for symbol {name}"
            );
        }
    }

    #[test]
    fn assignment() {
        let input = "let x = |i| i; let y = x(2);";
        let result = parse_and_type_check(input);
        check(
            &result,
            &[
                ("x", "T: FromLiteral", "T -> T"),
                ("y", "T: FromLiteral", "T"),
            ],
        );
    }

    #[test]
    fn higher_order() {
        let input = "let x = |i| |f| i + f(i); let y = x(2)(|k| k + 8);";
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
        let input = "let fold = |length, f, initial, folder|
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
                "T1: FromLiteral + Ord + Sub, T2, T3",
                "T1, (T1 -> T2), T3, (T3, T2 -> T3) -> T3",
            )],
        );
    }

    #[test]
    fn sum() {
        let input = "let sum = |a, b| a + b;";
        let result = parse_and_type_check(input);
        check(&result, &[("sum", "T: Add", "T, T -> T")]);
    }

    #[test]
    fn sum_via_fold() {
        let input = "let fold = |length, f, initial, folder|
            if length <= 0 {
                initial
            } else {
                folder(fold((length - 1), f, initial, folder), f((length - 1)))
            };
        let sum = |n, f| fold(n, f, 0, |a, b| a + b);
        ";
        let result = parse_and_type_check(input);
        check(
            &result,
            &[(
                "sum",
                "T1: FromLiteral + Ord + Sub, T2: Add + FromLiteral",
                "T1, (T1 -> T2) -> T2",
            )],
        );
    }

    #[test]
    fn pow() {
        let input = "let pow = |a, b| a ** b; let x = pow(2, 3);";
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
        let input = "let g = || g(); let x = |a, b| if g() { a } else { b + 2 };";
        let result = parse_and_type_check(input);
        check(
            &result,
            // TODO " -> bool" is also not formatted correctly.
            // TODO this test shows that we do not have let-polymorphism,
            // since the type of g is determined by how it is used in x.
            &[
                ("g", "", " -> bool"),
                ("x", "T: Add + FromLiteral", "T, T -> T"),
            ],
        );
    }

    #[test]
    fn constraints() {
        let input = "let a; let BYTE = |i| i & 0xff; {a + 1} in {BYTE};";
        let result = parse_and_type_check(input);
        check(
            &result,
            // TODO " -> bool" is also not formatted correctly.
            // TODO this test shows that we do not have let-polymorphism,
            // since the type of g is determined by how it is used in x.
            &[
                ("g", "", " -> bool"),
                ("x", "T: Add + FromLiteral", "T, T -> T"),
            ],
        );
    }
}
