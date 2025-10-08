use powdr_number::GoldilocksField;

use crate::{
    constraint_system::{AlgebraicConstraint, BusInteraction, ConstraintSystem},
    grouped_expression::GroupedExpression,
    runtime_constant::RuntimeConstant,
};

pub type Var = &'static str;
pub type Qse = GroupedExpression<GoldilocksField, Var>;

pub fn var(name: Var) -> Qse {
    Qse::from_unknown_variable(name)
}

pub fn constant(value: u64) -> Qse {
    Qse::from_number(GoldilocksField::from(value))
}

impl<T: RuntimeConstant, V> ConstraintSystem<T, V> {
    pub fn with_constraints(
        mut self,
        constraints: Vec<impl Into<AlgebraicConstraint<GroupedExpression<T, V>>>>,
    ) -> Self {
        self.algebraic_constraints
            .extend(constraints.into_iter().map(Into::into));
        self
    }

    pub fn with_bus_interactions(
        mut self,
        bus_interactions: Vec<impl Into<BusInteraction<GroupedExpression<T, V>>>>,
    ) -> Self {
        self.bus_interactions
            .extend(bus_interactions.into_iter().map(Into::into));
        self
    }
}
