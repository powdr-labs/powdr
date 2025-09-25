use std::fmt::Display;

use crate::{
    grouped_expression::GroupedExpression, indexed_constraint_system::IndexedConstraintSystem,
    runtime_constant::RuntimeConstant,
};

#[derive(Debug)]
pub struct CorrectnessTrace<T, V> {
    assignments: Vec<Assignment<T, V>>,
}

#[derive(Debug)]
pub struct Assignment<T, V> {
    var: V,
    value: GroupedExpression<T, V>,
    reason: Reason<T, V>,
}

#[derive(Debug)]
pub enum Reason<T, V> {
    SolvedConstraint(GroupedExpression<T, V>),
}

impl<T: Display + RuntimeConstant, V: Display + Clone + Ord> Display for Reason<T, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Reason::SolvedConstraint(c) => write!(f, "{c}"),
        }
    }
}

impl<T, V> Default for CorrectnessTrace<T, V> {
    fn default() -> Self {
        Self {
            assignments: Vec::new(),
        }
    }
}

impl<T: Display + RuntimeConstant, V: Display + Clone + Ord> Display for CorrectnessTrace<T, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for assignment in &self.assignments {
            writeln!(
                f,
                "{} = {} [{}]",
                assignment.var, assignment.value, assignment.reason
            )?;
        }
        Ok(())
    }
}

impl<T, V> CorrectnessTrace<T, V> {
    pub fn add_assignment_by_solving(
        &mut self,
        v: V,
        value: GroupedExpression<T, V>,
        constraint: GroupedExpression<T, V>,
    ) {
        self.assignments.push(Assignment {
            var: v,
            value,
            reason: Reason::SolvedConstraint(constraint),
        });
    }
}
