use powdr_number::GoldilocksField;

use crate::{
    quadratic_symbolic_expression::{QuadraticSymbolicExpression, RuntimeConstant},
    symbolic_expression::SymbolicExpression,
};

pub type Var = &'static str;
pub type Qse = QuadraticSymbolicExpression<SymbolicExpression<GoldilocksField, Var>, Var>;

pub fn var(name: Var) -> Qse {
    Qse::from_unknown_variable(name)
}

pub fn constant(value: u64) -> Qse {
    SymbolicExpression::from_u64(value).into()
}
