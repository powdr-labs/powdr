//! In this module, we instantiate `powdr_expression::AlgebraicExpression` using a
//! custom `AlgebraicReference` type.
use core::ops::{Add, Mul, Neg, Sub};
use powdr_expression::{AlgebraicBinaryOperator, AlgebraicUnaryOperator};
use serde::{Deserialize, Serialize};
use std::{collections::BTreeMap, hash::Hash, marker::PhantomData, sync::Arc};

use crate::SymbolicBusInteraction;

pub type AlgebraicExpression<T> = powdr_expression::AlgebraicExpression<T, AlgebraicReference>;

#[derive(Debug, Clone, Eq, Serialize, Deserialize)]
pub struct AlgebraicReference {
    /// Name of the polynomial - just for informational purposes.
    /// Comparisons are based on the ID.
    pub name: Arc<String>,
    /// Identifier for a reference.
    pub id: u64,
}

impl std::fmt::Display for AlgebraicReference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl PartialOrd for AlgebraicReference {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for AlgebraicReference {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.id.cmp(&other.id)
    }
}

impl PartialEq for AlgebraicReference {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Hash for AlgebraicReference {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

/// Tries to convert a `powdr_expression::AlgebraicExpression<T, R>` into a
/// `powdr_expression::AlgebraicExpression<T, AlgebraicReference>`.
pub fn try_convert<T, R: TryInto<AlgebraicReference>>(
    expr: powdr_expression::AlgebraicExpression<T, R>,
) -> Result<AlgebraicExpression<T>, R::Error> {
    match expr {
        powdr_expression::AlgebraicExpression::Reference(reference) => Ok(
            powdr_expression::AlgebraicExpression::Reference(reference.try_into()?),
        ),
        powdr_expression::AlgebraicExpression::Number(n) => {
            Ok(powdr_expression::AlgebraicExpression::Number(n))
        }
        powdr_expression::AlgebraicExpression::BinaryOperation(binary) => {
            Ok(powdr_expression::AlgebraicExpression::BinaryOperation(
                powdr_expression::AlgebraicBinaryOperation {
                    left: Box::new(try_convert(*binary.left)?),
                    op: binary.op,
                    right: Box::new(try_convert(*binary.right)?),
                },
            ))
        }
        powdr_expression::AlgebraicExpression::UnaryOperation(unary) => {
            Ok(powdr_expression::AlgebraicExpression::UnaryOperation(
                powdr_expression::AlgebraicUnaryOperation {
                    op: unary.op,
                    expr: Box::new(try_convert(*unary.expr)?),
                },
            ))
        }
    }
}

pub struct RowEvaluator<
    'a,
    F: Add<Output = F> + Sub<Output = F> + Mul<Output = F> + Neg<Output = F> + Copy,
> {
    pub row: &'a [F],
    pub witness_id_to_index: Option<&'a BTreeMap<u64, usize>>,
}

impl<'a, F: Add<Output = F> + Sub<Output = F> + Mul<Output = F> + Neg<Output = F> + Copy>
    RowEvaluator<'a, F>
{
    pub fn new(row: &'a [F], witness_id_to_index: Option<&'a BTreeMap<u64, usize>>) -> Self {
        Self {
            row,
            witness_id_to_index,
        }
    }

    pub fn eval_expr(&self, algebraic_expr: &AlgebraicExpression<F>) -> F {
        match algebraic_expr {
            AlgebraicExpression::Number(n) => *n,
            AlgebraicExpression::BinaryOperation(binary) => match binary.op {
                AlgebraicBinaryOperator::Add => {
                    self.eval_expr(&binary.left) + self.eval_expr(&binary.right)
                }
                AlgebraicBinaryOperator::Sub => {
                    self.eval_expr(&binary.left) - self.eval_expr(&binary.right)
                }
                AlgebraicBinaryOperator::Mul => {
                    self.eval_expr(&binary.left) * self.eval_expr(&binary.right)
                }
            },
            AlgebraicExpression::UnaryOperation(unary) => match unary.op {
                AlgebraicUnaryOperator::Minus => -self.eval_expr(&unary.expr),
            },
            AlgebraicExpression::Reference(var) => self.eval_var(var),
        }
    }

    fn eval_var(&self, algebraic_var: &AlgebraicReference) -> F {
        let index = if let Some(witness_id_to_index) = self.witness_id_to_index {
            witness_id_to_index[&(algebraic_var.id)]
        } else {
            algebraic_var.id as usize
        };
        self.row[index]
    }

    pub fn eval_bus_interaction<'b>(
        &'a self,
        bus_interaction: &'b SymbolicBusInteraction<F>,
    ) -> ConcreteBusInteraction<F, impl Iterator<Item = F> + 'b>
    where
        'a: 'b,
    {
        let mult = self.eval_expr(&bus_interaction.mult);
        let args = bus_interaction.args.iter().map(|arg| self.eval_expr(arg));
        ConcreteBusInteraction {
            id: bus_interaction.id,
            mult,
            args,
        }
    }
}

pub struct ConcreteBusInteraction<F, I> {
    pub id: u64,
    pub mult: F,
    pub args: I,
}

pub trait AlgebraicEvaluator<F, E>
where
    F: Add<Output = F> + Sub<Output = F> + Mul<Output = F> + Neg<Output = F> + Copy,
    E: Add<E, Output = E> + Sub<E, Output = E> + Mul<E, Output = E> + Neg<Output = E>,
{
    fn eval_const(&self, c: F) -> E;
    fn eval_var(&self, algebraic_var: &AlgebraicReference) -> E;

    fn eval_expr(&self, algebraic_expr: &AlgebraicExpression<F>) -> E {
        match algebraic_expr {
            AlgebraicExpression::Number(n) => self.eval_const(*n),
            AlgebraicExpression::BinaryOperation(binary) => match binary.op {
                AlgebraicBinaryOperator::Add => {
                    self.eval_expr(&binary.left) + self.eval_expr(&binary.right)
                }
                AlgebraicBinaryOperator::Sub => {
                    self.eval_expr(&binary.left) - self.eval_expr(&binary.right)
                }
                AlgebraicBinaryOperator::Mul => {
                    self.eval_expr(&binary.left) * self.eval_expr(&binary.right)
                }
            },
            AlgebraicExpression::UnaryOperation(unary) => match unary.op {
                AlgebraicUnaryOperator::Minus => -self.eval_expr(&unary.expr),
            },
            AlgebraicExpression::Reference(var) => self.eval_var(var),
        }
    }
}

pub struct WitnessEvaluator<'a, V, F, E> {
    pub witness: &'a BTreeMap<u64, V>,
    _phantom: PhantomData<F>,
    _phantom_e: PhantomData<E>,
}

impl<'a, V, F, E> WitnessEvaluator<'a, V, F, E> {
    pub fn new(witness: &'a BTreeMap<u64, V>) -> Self {
        Self {
            witness,
            _phantom: PhantomData,
            _phantom_e: PhantomData,
        }
    }
}

impl<V, F, E> AlgebraicEvaluator<F, E> for WitnessEvaluator<'_, V, F, E>
where
    V: Into<E> + Copy,
    F: Add<Output = F> + Sub<Output = F> + Mul<Output = F> + Neg<Output = F> + Copy + Into<E>,
    E: Add<E, Output = E> + Sub<E, Output = E> + Mul<E, Output = E> + Neg<Output = E>,
{
    fn eval_const(&self, c: F) -> E {
        c.into()
    }

    fn eval_var(&self, algebraic_var: &AlgebraicReference) -> E {
        (*self.witness.get(&algebraic_var.id).unwrap()).into()
    }
}
