use powdr_number::GoldilocksField;

use crate::{grouped_expression::GroupedExpression, symbolic_expression::SymbolicExpression};

pub type Var = &'static str;
pub type Qse = GroupedExpression<SymbolicExpression<GoldilocksField, Var>, Var>;

pub fn var(name: Var) -> Qse {
    Qse::from_unknown_variable(name)
}

pub fn constant(value: u64) -> Qse {
    Qse::from_number(GoldilocksField::from(value))
}
