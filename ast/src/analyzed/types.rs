use std::{collections::HashMap, fmt::Display};

use itertools::Itertools;
use powdr_number::FieldElement;

use crate::parsed::{
    ArrayTypeName, Expression, FunctionTypeName, TupleTypeName, TypeBounds, TypeName,
};

use super::Reference;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct TypedExpression<T, Ref = Reference> {
    pub e: Expression<T, Ref>,
    pub type_scheme: Option<TypeScheme>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum Type {
    /// Boolean
    Bool,
    /// Integer (arbitrary precision)
    Int,
    /// Field element (unspecified field)
    Fe,
    /// String
    String,
    /// Algebraic expression
    Expr,
    /// Polynomial identity or lookup (not yet supported)
    Constr,
    Array(ArrayType),
    Tuple(TupleType),
    Function(FunctionType),
    TypeVar(String),
}

impl Type {
    /// Returns the column type `int -> fe`.
    pub fn col() -> Self {
        Type::Function(FunctionType::col())
    }

    pub fn is_elementary(&self) -> bool {
        match self {
            Type::Bool | Type::Int | Type::Fe | Type::String | Type::Expr | Type::Constr => true,
            Type::Array(_) | Type::Tuple(_) | Type::Function(_) | Type::TypeVar(_) => false,
        }
    }

    /// Returns true if the type name needs parentheses around it during formatting
    /// when used inside a complex expression.
    pub fn needs_parentheses(&self) -> bool {
        match self {
            _ if self.is_elementary() => false,
            Type::Array(_) | Type::Tuple(_) => false,
            Type::Function(fun) => fun.needs_parentheses(),
            Type::TypeVar(_) => false,
            _ => unreachable!(),
        }
    }

    pub fn is_concrete_type(&self) -> bool {
        self.contained_type_vars_with_repetitions().next().is_none()
    }

    pub fn contains_type_var(&self, name: &str) -> bool {
        self.contained_type_vars_with_repetitions()
            .any(|n| n == name)
    }

    /// Returns the list of contained type vars in order of first occurrence.
    pub fn contained_type_vars(&self) -> Vec<&String> {
        self.contained_type_vars_with_repetitions()
            .unique()
            .collect()
    }

    /// Substitutes all occurrences of the given type variables with the given types.
    /// Type variables to be substituted must not occur in the replacement types.
    pub fn substitute_type_vars(&mut self, substitutions: &HashMap<String, Type>) {
        match self {
            Type::TypeVar(n) => {
                if let Some(t) = substitutions.get(n) {
                    *self = t.clone();
                }
            }
            Type::Array(ArrayType { base, length: _ }) => {
                base.substitute_type_vars(substitutions);
            }
            Type::Tuple(TupleType { items }) => {
                items
                    .iter_mut()
                    .for_each(|t| t.substitute_type_vars(substitutions));
            }
            Type::Function(FunctionType { params, value }) => {
                params
                    .iter_mut()
                    .for_each(|t| t.substitute_type_vars(substitutions));
                value.substitute_type_vars(substitutions);
            }
            _ => {
                assert!(self.is_elementary());
            }
        }
    }

    fn contained_type_vars_with_repetitions(&self) -> Box<dyn Iterator<Item = &String> + '_> {
        match self {
            Type::TypeVar(n) => Box::new(std::iter::once(n)),
            Type::Array(ar) => ar.base.contained_type_vars_with_repetitions(),
            Type::Tuple(tu) => Box::new(
                tu.items
                    .iter()
                    .flat_map(|t| t.contained_type_vars_with_repetitions()),
            ),
            Type::Function(fun) => Box::new(
                fun.params
                    .iter()
                    .flat_map(|t| t.contained_type_vars_with_repetitions())
                    .chain(fun.value.contained_type_vars_with_repetitions()),
            ),
            _ => {
                assert!(self.is_elementary());
                Box::new(std::iter::empty())
            }
        }
    }
}

impl<T: FieldElement, Ref: Display> From<TypeName<Expression<T, Ref>>> for Type {
    fn from(value: TypeName<Expression<T, Ref>>) -> Self {
        match value {
            TypeName::Bool => Type::Bool,
            TypeName::Int => Type::Int,
            TypeName::Fe => Type::Fe,
            TypeName::String => Type::String,
            TypeName::Expr => Type::Expr,
            TypeName::Constr => Type::Constr,
            TypeName::Col => Type::col(),
            TypeName::Array(ar) => Type::Array(ar.into()),
            TypeName::Tuple(tu) => Type::Tuple(tu.into()),
            TypeName::Function(fun) => Type::Function(fun.into()),
            TypeName::TypeVar(v) => Type::TypeVar(v),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct ArrayType {
    pub base: Box<Type>,
    pub length: Option<u64>,
}

impl<T: FieldElement, Ref: Display> From<ArrayTypeName<Expression<T, Ref>>> for ArrayType {
    fn from(name: ArrayTypeName<Expression<T, Ref>>) -> Self {
        let length = name.length.as_ref().map(|l| {
            if let Expression::Number(n) = l {
                n.to_degree()
            } else {
                panic!(
                    "Array length expression not resolved in type name prior to conversion: {name}"
                );
            }
        });
        ArrayType {
            base: Box::new(Type::from(*name.base)),
            length,
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct TupleType {
    pub items: Vec<Type>,
}

impl<T: FieldElement, Ref: Display> From<TupleTypeName<Expression<T, Ref>>> for TupleType {
    fn from(value: TupleTypeName<Expression<T, Ref>>) -> Self {
        TupleType {
            items: value.items.into_iter().map(Into::into).collect(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct FunctionType {
    pub params: Vec<Type>,
    pub value: Box<Type>,
}

impl FunctionType {
    /// Returns the column type `int -> fe`.
    pub fn col() -> Self {
        FunctionType {
            params: vec![Type::Int],
            value: Box::new(Type::Fe),
        }
    }

    pub fn needs_parentheses(&self) -> bool {
        *self != Self::col()
    }
}

impl<T: FieldElement, Ref: Display> From<FunctionTypeName<Expression<T, Ref>>> for FunctionType {
    fn from(name: FunctionTypeName<Expression<T, Ref>>) -> Self {
        FunctionType {
            params: name.params.into_iter().map(Into::into).collect(),
            value: Box::new(Type::from(*name.value)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeScheme {
    /// Type variables and their trait bounds
    pub vars: TypeBounds,
    pub ty: Type,
}

impl TypeScheme {
    pub fn simplify_type_vars(self) -> TypeScheme {
        let name_substitutions: HashMap<_, _> = match self.vars.len() {
            0 => return self,
            1 => {
                let var = self.vars.vars().next().unwrap();
                [(var.clone(), "T".to_string())].into()
            }
            _ => self
                .ty
                .contained_type_vars()
                .iter()
                .enumerate()
                .map(|(i, v)| ((*v).clone(), format!("T{}", i + 1)))
                .collect(),
        };
        assert!(name_substitutions.len() == self.vars.len());
        let mut ty = self.ty;
        ty.substitute_type_vars(
            &name_substitutions
                .iter()
                .map(|(n, s)| (n.clone(), Type::TypeVar(s.clone())))
                .collect(),
        );
        TypeScheme {
            vars: TypeBounds::new(
                self.vars
                    .bounds()
                    .map(|(v, b)| (name_substitutions[v].clone(), b.clone())),
            ),
            ty,
        }
    }

    pub fn type_vars_to_string(&self) -> String {
        if self.vars.is_empty() {
            return String::new();
        } else {
            format!("<{}>", self.vars)
        }
    }
}

impl From<Type> for TypeScheme {
    fn from(value: Type) -> Self {
        // TODO check that value does not contain any type variables?
        TypeScheme {
            vars: Default::default(),
            ty: value,
        }
    }
}
