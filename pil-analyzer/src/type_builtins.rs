use std::collections::HashMap;

use powdr_ast::{
    parsed::types::{ArrayType, Type, TypeScheme},
    parsed::{BinaryOperator, UnaryOperator},
};
use powdr_parser::parse_type_scheme;

use lazy_static::lazy_static;

/// Returns the type used for a reference to a declaration.
pub fn type_for_reference(declared: &Type) -> Type {
    match declared {
        // References to columns are exprs
        Type::Col => Type::Expr,
        // Similar for arrays of columns
        Type::Array(ArrayType { base, length: _ }) if base.as_ref() == &Type::Col => {
            Type::Array(ArrayType {
                base: Type::Expr.into(),
                length: None,
            })
        }
        // Arrays of intermediate columns lose their length.
        Type::Array(ArrayType {
            base,
            length: Some(_),
        }) if base.as_ref() == &Type::Expr => Type::Array(ArrayType {
            base: base.clone(),
            length: None,
        }),
        t => t.clone(),
    }
}

lazy_static! {
    static ref BUILTIN_SCHEMES: HashMap<String, TypeScheme> = [
        ("std::array::len", ("T", "T[] -> int")),
        ("std::check::panic", ("", "string -> !")),
        ("std::convert::expr", ("T: FromLiteral", "T -> expr")),
        ("std::convert::fe", ("T: FromLiteral", "T -> fe")),
        ("std::convert::int", ("T: FromLiteral", "T -> int")),
        ("std::convert::expr", ("T: FromLiteral", "T -> expr")),
        (
            "std::debug::print",
            ("T: ToString", "T -> std::prelude::Constr[]")
        ),
        ("std::field::modulus", ("", "-> int")),
        (
            "std::prover::capture_stage",
            ("", "(-> int) -> std::prelude::Constr[]")
        ),
        ("std::prover::challenge", ("", "int, int -> expr")),
        ("std::prover::degree", ("", "-> int")),
        ("std::prover::eval", ("", "expr -> fe")),
    ]
    .into_iter()
    .map(|(name, (vars, ty))| { (name.to_string(), parse_type_scheme(vars, ty)) })
    .collect();
    static ref BINARY_OPERATOR_SCHEMES: HashMap<BinaryOperator, TypeScheme> = [
        (BinaryOperator::Add, ("T: Add", "T, T -> T")),
        (BinaryOperator::Sub, ("T: Sub", "T, T -> T")),
        (BinaryOperator::Mul, ("T: Mul", "T, T -> T")),
        (BinaryOperator::Div, ("", "int, int -> int")),
        (BinaryOperator::Mod, ("", "int, int -> int")),
        (BinaryOperator::Pow, ("T: Pow", "T, int -> T")),
        (BinaryOperator::ShiftLeft, ("", "int, int -> int")),
        (BinaryOperator::ShiftRight, ("", "int, int -> int")),
        (BinaryOperator::BinaryAnd, ("", "int, int -> int")),
        (BinaryOperator::BinaryOr, ("", "int, int -> int")),
        (BinaryOperator::BinaryXor, ("", "int, int -> int")),
        (BinaryOperator::Less, ("T: Ord", "T, T -> bool")),
        (BinaryOperator::LessEqual, ("T: Ord", "T, T -> bool")),
        (BinaryOperator::Equal, ("T: Eq", "T, T -> bool")),
        (
            BinaryOperator::Identity,
            ("", "expr, expr -> std::prelude::Constr")
        ),
        (BinaryOperator::NotEqual, ("T: Eq", "T, T -> bool")),
        (BinaryOperator::GreaterEqual, ("T: Ord", "T, T -> bool")),
        (BinaryOperator::Greater, ("T: Ord", "T, T -> bool")),
        (BinaryOperator::LogicalOr, ("", "bool, bool -> bool")),
        (BinaryOperator::LogicalAnd, ("", "bool, bool -> bool")),
    ]
    .into_iter()
    .map(|(op, (vars, ty))| { (op, parse_type_scheme(vars, ty)) })
    .collect();
    static ref UNARY_OPERATOR_SCHEMES: HashMap<UnaryOperator, TypeScheme> = [
        (UnaryOperator::Minus, ("T: Neg", "T -> T")),
        (UnaryOperator::LogicalNot, ("", "bool -> bool")),
        (UnaryOperator::Next, ("", "expr -> expr")),
    ]
    .into_iter()
    .map(|(op, (vars, ty))| (op, parse_type_scheme(vars, ty)))
    .collect();
}

pub fn builtin_schemes() -> &'static HashMap<String, TypeScheme> {
    &BUILTIN_SCHEMES
}

pub fn binary_operator_scheme(op: BinaryOperator) -> TypeScheme {
    BINARY_OPERATOR_SCHEMES[&op].clone()
}

pub fn unary_operator_scheme(op: UnaryOperator) -> TypeScheme {
    UNARY_OPERATOR_SCHEMES[&op].clone()
}

pub fn elementary_type_bounds(ty: &Type) -> &'static [&'static str] {
    match ty {
        Type::Bottom => &[],
        Type::Bool => &["ToString"],
        Type::Int => &[
            "ToString",
            "FromLiteral",
            "Add",
            "Sub",
            "Neg",
            "Mul",
            "Mod",
            "Pow",
            "Ord",
            "Eq",
        ],
        Type::Fe => &[
            "ToString",
            "FromLiteral",
            "Add",
            "Sub",
            "Neg",
            "Mul",
            "Pow",
            "Neg",
            "Eq",
        ],
        Type::String => &["ToString", "Add", "Eq"],
        Type::Expr => &[
            "ToString",
            "FromLiteral",
            "Add",
            "Sub",
            "Neg",
            "Mul",
            "Pow",
            "Neg",
            "Eq",
        ],
        Type::Col => &[],
        Type::Array(_) => &["Add"],
        Type::Tuple(_) => &[],
        Type::Function(_) => &[],
        Type::TypeVar(_) | Type::NamedType(_, _) => unreachable!(),
    }
}
