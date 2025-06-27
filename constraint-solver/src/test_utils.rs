use powdr_number::GoldilocksField;

use crate::quadratic_symbolic_expression::QuadraticSymbolicExpression;

pub type Var = &'static str;
pub type Qse = QuadraticSymbolicExpression<GoldilocksField, Var>;

pub fn var(name: Var) -> Qse {
    Qse::from_unknown_variable(name)
}

pub fn constant(value: u64) -> Qse {
    Qse::from_number(GoldilocksField::from(value))
}
