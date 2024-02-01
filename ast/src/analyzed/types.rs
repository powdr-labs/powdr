use std::fmt::Display;

use powdr_number::FieldElement;

use crate::parsed::{ArrayTypeName, Expression, FunctionTypeName, TupleTypeName, TypeName};

use super::Reference;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct TypedExpression<T, Ref = Reference> {
    pub e: Expression<T, Ref>,
    pub ty: Option<Type>,
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
}

impl Type {
    /// Returns the column type `int -> fe`.
    pub fn col() -> Self {
        Type::Function(FunctionType::col())
    }

    /// Returns true if the type name needs parentheses around it during formatting
    /// when used inside a complex expression.
    pub fn needs_parentheses(&self) -> bool {
        match self {
            Type::Bool
            | Type::Int
            | Type::Fe
            | Type::String
            | Type::Expr
            | Type::Constr
            | Type::Array(_)
            | Type::Tuple(_) => false,
            Type::Function(fun) => fun.needs_parantheses(),
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
            TypeName::Col => Type::Function(FunctionType {
                params: vec![Type::Int],
                value: Box::new(Type::Fe),
            }),
            TypeName::Array(ar) => Type::Array(ar.into()),
            TypeName::Tuple(tu) => Type::Tuple(tu.into()),
            TypeName::Function(fun) => Type::Function(fun.into()),
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

    pub fn needs_parantheses(&self) -> bool {
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
