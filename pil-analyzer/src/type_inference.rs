use std::collections::HashMap;

use powdr_ast::{
    analyzed::{
        types::{ArrayType, FunctionType, TupleType, Type, TypeScheme},
        Expression, FunctionValueDefinition, PolynomialReference, Reference, Symbol,
    },
    parsed::{FunctionCall, LambdaExpression},
};
use powdr_number::{FieldElement, GoldilocksField};
use powdr_parser::parse_type_name;

pub fn infer_types<T: FieldElement>(
    definitions: &HashMap<String, (Symbol, Option<FunctionValueDefinition<T>>)>,
) -> Result<HashMap<String, Type>, String> {
    TypeChecker::new(definitions).infer_types()
}

struct TypeChecker<'a, T> {
    definitions: &'a HashMap<String, (Symbol, Option<FunctionValueDefinition<T>>)>,
    /// Types for symbols, might contain type variables.
    /// TODO could these be type schemes?
    types: HashMap<String, Type>,
    /// Types for local variables, might contain type variables.
    local_var_types: Vec<Type>,
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
        tc
    }

    fn infer_types(mut self) -> Result<HashMap<String, Type>, String> {
        for (name, value) in self.definitions {
            let ty = self.types[name].clone();
            self.unify(&ty, value)?;
            // TODO apply substitutions
        }
        // TODO this shows that we probably need a sub-struct just for the substitutions.
        let mut types = std::mem::take(&mut self.types);
        for t in types.values_mut() {
            self.substitute(t);
        }
        Ok(types)
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
        match e {
            Expression::Reference(Reference::LocalVar(id, _name)) => {
                self.unify_types(ty, self.local_var_types[*id as usize].clone())
            }
            Expression::Reference(Reference::Poly(PolynomialReference { name, poly_id: _ })) => {
                self.unify_types(ty, self.types[name].clone())
            }
            Expression::PublicReference(_) => todo!(),
            Expression::Number(_) => {
                // TODO unify ty with a new type var T: FromLiteral
                self.unify_types(ty, Type::Int)
            }
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
            Expression::BinaryOperation(left, _op, right) => {
                // Function application instantiates a type scheme.
                // Or does symbol lookup instantiatet it?
                // TODO: We actually have: let<T: Sum> +: T, T -> T;
                // So for the newly instantiated type, we also add the restriction
                // that it has to implement Sum.
                let scheme = TypeScheme {
                    vars: vec!["T".to_string()],
                    ty: parse_type_name::<GoldilocksField>("T, T -> T")
                        .unwrap()
                        .into(),
                };
                let fun_type = self.instantiate_scheme(scheme);
                let value = self
                    .unify_function_call(fun_type, [left, right].into_iter().map(AsRef::as_ref))?;
                self.unify_types(ty, value)?;
                Ok(())
            }
            Expression::UnaryOperation(_, _) => todo!(),
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
            Expression::IfExpression(_) => todo!(),
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
        match ty {
            Type::TypeVar(name) => {
                if let Some(t) = self.substitutions.get(name) {
                    *ty = t.clone();
                }
            }
            Type::Array(ArrayType { base, length: _ }) => {
                self.substitute(base);
            }
            Type::Tuple(TupleType { items }) => {
                items.iter_mut().for_each(|t| self.substitute(t));
            }
            Type::Function(FunctionType { params, value }) => {
                params.iter_mut().for_each(|t| self.substitute(t));
                self.substitute(value);
            }
            _ => {
                assert!(ty.is_elementary());
            }
        }
    }

    /// Applies the given substitutions to the type.
    fn substitute_single(ty: &mut Type, name: &str, sub: &Type) {
        match ty {
            Type::TypeVar(n) => {
                if n.as_str() == name {
                    *ty = sub.clone();
                }
            }
            Type::Array(ArrayType { base, length: _ }) => {
                Self::substitute_single(base, name, sub);
            }
            Type::Tuple(TupleType { items }) => {
                items
                    .iter_mut()
                    .for_each(|t| Self::substitute_single(t, name, sub));
            }
            Type::Function(FunctionType { params, value }) => {
                params
                    .iter_mut()
                    .for_each(|t| Self::substitute_single(t, name, sub));
                Self::substitute_single(value, name, sub);
            }
            _ => {
                assert!(ty.is_elementary());
            }
        }
    }

    fn instantiate_scheme(&mut self, scheme: TypeScheme) -> Type {
        scheme.vars.into_iter().fold(scheme.ty, |mut ty, v| {
            Self::substitute_single(&mut ty, &v, &self.new_type_var());
            ty
        })
    }

    fn unify_types(&mut self, mut ty1: Type, mut ty2: Type) -> Result<(), String> {
        if let (Type::TypeVar(n1), Type::TypeVar(n2)) = (&ty1, &ty2) {
            if n1 == n2 {
                return Ok(());
            }
        }
        // TODO this should not be needed for recursive calls, should it?
        self.substitute(&mut ty1);
        self.substitute(&mut ty2);
        println!("Unify {ty1}   <=>   {ty2}");
        match (ty1, ty2) {
            (Type::TypeVar(n1), Type::TypeVar(n2)) if n1 == n2 => Ok(()),
            (Type::TypeVar(name), ty) | (ty, Type::TypeVar(name)) => {
                if ty.contains_type_var(&name) {
                    Err(format!("Cannot unify types {ty} and {name}"))
                } else {
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
            _ => todo!(),
        }
    }

    /// Adds a substitution of a type variable to a type.
    /// Note that this requires the type to be fully substituted.
    fn add_substitution(&mut self, name: String, ty: Type) {
        self.substitutions
            .values_mut()
            .for_each(|t| Self::substitute_single(t, &name, &ty));
        println!("Adding substitution: {name} := {ty}");
        self.substitutions.insert(name, ty);
    }

    fn new_type_var(&mut self) -> Type {
        let name = format!("T{}", self.next_type_var);
        self.next_type_var += 1;
        Type::TypeVar(name)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use powdr_number::GoldilocksField;

    use crate::analyze_string;

    fn parse_and_type_check(input: &str) -> Result<HashMap<String, Type>, String> {
        let analyzed = analyze_string::<GoldilocksField>(input);
        infer_types(&analyzed.definitions)
    }

    fn check(types: &Result<HashMap<String, Type>, String>, expected: &[(&str, &str)]) {
        let types = types.as_ref().unwrap();
        for (name, ty) in expected {
            assert_eq!(types[&name.to_string()].to_string(), *ty);
        }
    }

    #[test]
    fn assignment() {
        let input = "let x: int -> int = |i| i; let y = x(2);";
        let result = parse_and_type_check(input);
        check(&result, &[("x", "int -> int"), ("y", "int")]);
    }

    #[test]
    fn higher_order() {
        let input = "let x = |i| |f| i + f(i); let y = x(2)(|k| k + 8);";
        let result = parse_and_type_check(input);
        check(&result, &[("x", "int -> int"), ("y", "int")]);
    }

    #[test]
    #[should_panic(expected = "Cannot unify types")]
    fn invalid_recursive() {
        let input = "let x = |i| |f| x(i);";
        parse_and_type_check(input).unwrap();
    }

    // Test the following, once we can parse it:
    // Question: Do we instantiate type schemes on lookup or function applitaino?
    //     Does s need to be generic?
    // let<T> sum: T, T -> T = |a, b| a + b;
    // let<T> s: -> (T, T -> T) = || sum;
    // let x = s()(2, 3);
    // let y = s()(std::convert::fe(4), std::convert::fe(5));
}
