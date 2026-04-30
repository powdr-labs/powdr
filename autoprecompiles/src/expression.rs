//! In this module, we instantiate `powdr_expression::AlgebraicExpression` using a
//! custom `AlgebraicReference` type.
use core::ops::{Add, Mul, Neg, Sub};
use powdr_number::ExpressionConvertible;
use serde::{Deserialize, Serialize};
use std::{collections::BTreeMap, hash::Hash, marker::PhantomData, sync::Arc};

use crate::symbolic_machine::{SymbolicBusInteraction, SymbolicConstraint};

pub type AlgebraicExpression<T> = powdr_expression::AlgebraicExpression<T, AlgebraicReference>;

#[derive(Debug, Clone, Eq)]
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

impl Serialize for AlgebraicReference {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&format!("{}@{}", self.name, self.id))
    }
}

impl<'de> Deserialize<'de> for AlgebraicReference {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        let Some(separator_pos) = s.rfind('@') else {
            return Err(serde::de::Error::custom(format!(
                "Invalid format for AlgebraicReference: {s}",
            )));
        };
        let name = Arc::new(s[..separator_pos].to_string());
        let id: u64 = s[separator_pos + 1..].parse().map_err(|_| {
            serde::de::Error::custom(format!(
                "Invalid ID in AlgebraicReference: {}",
                &s[separator_pos + 1..]
            ))
        })?;
        Ok(AlgebraicReference { name, id })
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

/// Evaluate an `AlgebraicExpression` to a generic type, which for example can be an expression or a concrete value.
pub trait AlgebraicEvaluator<F, E>
where
    F: Add<Output = F> + Sub<Output = F> + Mul<Output = F> + Neg<Output = F> + Copy,
    E: Add<E, Output = E> + Sub<E, Output = E> + Mul<E, Output = E> + Neg<Output = E>,
{
    fn eval_const(&self, c: F) -> E;
    fn eval_var(&self, algebraic_var: &AlgebraicReference) -> E;

    fn eval_expr(&self, algebraic_expr: &AlgebraicExpression<F>) -> E {
        algebraic_expr.to_expression(&|n| self.eval_const(*n), &|var| self.eval_var(var))
    }
    fn eval_bus_interaction<'a, 'b>(
        &'a self,
        bus_interaction: &'b SymbolicBusInteraction<F>,
    ) -> ConcreteBusInteraction<E, impl Iterator<Item = E> + 'b>
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

    fn eval_constraint(&self, constraint: &SymbolicConstraint<F>) -> ConcreteConstraint<E> {
        ConcreteConstraint {
            expr: self.eval_expr(&constraint.expr),
        }
    }
}

/// Evaluates an `AlgebraicExpression` to a concrete value by subsituting the polynomial references by known values.
pub struct RowEvaluator<'a, F>
where
    F: Add<Output = F> + Sub<Output = F> + Mul<Output = F> + Neg<Output = F> + Copy,
{
    pub row: &'a [F],
}

impl<'a, F> RowEvaluator<'a, F>
where
    F: Add<Output = F> + Sub<Output = F> + Mul<Output = F> + Neg<Output = F> + Copy,
{
    pub fn new(row: &'a [F]) -> Self {
        Self { row }
    }
}

impl<F> AlgebraicEvaluator<F, F> for RowEvaluator<'_, F>
where
    F: Add<Output = F> + Sub<Output = F> + Mul<Output = F> + Neg<Output = F> + Copy,
{
    fn eval_const(&self, c: F) -> F {
        c
    }

    fn eval_var(&self, algebraic_var: &AlgebraicReference) -> F {
        self.row[algebraic_var.id as usize]
    }
}

/// Evaluates an `AlgebraicExpression` to a concrete value by substituting the polynomial
/// references by known values where known value is looked up via a dense Vec index mapping.
pub struct MappingRowEvaluator<'a, F>
where
    F: Add<Output = F> + Sub<Output = F> + Mul<Output = F> + Neg<Output = F> + Copy,
{
    pub row: &'a [F],
    /// Dense Vec indexed by polynomial ID -> column index in row.
    pub witness_id_to_index: &'a [usize],
}

impl<'a, F> MappingRowEvaluator<'a, F>
where
    F: Add<Output = F> + Sub<Output = F> + Mul<Output = F> + Neg<Output = F> + Copy,
{
    pub fn new(row: &'a [F], witness_id_to_index: &'a [usize]) -> Self {
        Self {
            row,
            witness_id_to_index,
        }
    }
}

impl<F> AlgebraicEvaluator<F, F> for MappingRowEvaluator<'_, F>
where
    F: Add<Output = F> + Sub<Output = F> + Mul<Output = F> + Neg<Output = F> + Copy,
{
    fn eval_const(&self, c: F) -> F {
        c
    }

    fn eval_var(&self, algebraic_var: &AlgebraicReference) -> F {
        let index = self.witness_id_to_index[algebraic_var.id as usize];
        self.row[index]
    }
}

pub struct ConcreteBusInteraction<E, I> {
    pub id: u64,
    pub mult: E,
    pub args: I,
}

pub struct ConcreteConstraint<E> {
    pub expr: E,
}

/// Evaluates by subsituting the polynomial references by known values, potentially changing the expression type in the process.
pub struct WitnessEvaluator<'a, V, F, E> {
    pub witness: &'a BTreeMap<u64, V>,
    _phantom: PhantomData<(F, E)>,
}

impl<'a, V, F, E> WitnessEvaluator<'a, V, F, E> {
    pub fn new(witness: &'a BTreeMap<u64, V>) -> Self {
        Self {
            witness,
            _phantom: PhantomData,
        }
    }
}

impl<V, F, E> AlgebraicEvaluator<F, E> for WitnessEvaluator<'_, V, F, E>
where
    V: Into<E> + Copy,
    F: Add<Output = F> + Sub<Output = F> + Mul<Output = F> + Neg<Output = F> + Into<E> + Copy,
    E: Add<E, Output = E> + Sub<E, Output = E> + Mul<E, Output = E> + Neg<Output = E>,
{
    fn eval_const(&self, c: F) -> E {
        c.into()
    }

    fn eval_var(&self, algebraic_var: &AlgebraicReference) -> E {
        (*self.witness.get(&algebraic_var.id).unwrap()).into()
    }
}

use powdr_expression::{AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicUnaryOperator};

/// Pre-compiled expression for fast per-row evaluation without recursive AST walking.
///
/// At build time, each `AlgebraicExpression` is analyzed once and compiled into the most
/// efficient variant. The specialized variants avoid the recursive enum-matching, closure
/// dispatch, and trait-object overhead of `AlgebraicExpression::to_expression()`.
pub enum CompiledExpr<F> {
    /// Expression is a compile-time constant. Zero cost per row.
    Constant(F),
    /// Expression is a single column read: `row[col_idx]`. One array access per row.
    DirectLoad(usize),
    /// Expression is `constant + sum(coeff * row[col_idx]) + sum(coeff * row[a] * row[b])`.
    /// Covers both linear (degree-1) and quadratic (degree-2) expressions.
    /// Linear expressions have an empty `products` vec.
    Polynomial {
        constant: F,
        /// Linear terms: (col_idx, coefficient).
        linear: Vec<(usize, F)>,
        /// Quadratic terms: (col_idx_a, col_idx_b, coefficient).
        products: Vec<(usize, usize, F)>,
    },
}

/// Intermediate decomposition: constant + linear terms + quadratic products.
/// Used during compilation; converted to `CompiledExpr` afterwards.
struct Decomposition<F> {
    constant: F,
    linear: Vec<(usize, F)>,
    products: Vec<(usize, usize, F)>,
}

impl<F> CompiledExpr<F>
where
    F: Add<Output = F> + Sub<Output = F> + Mul<Output = F> + Neg<Output = F> + Copy + PartialEq,
{
    /// Compile an expression at build time into a fast-eval form.
    /// `zero` and `one` are the field's additive and multiplicative identities.
    pub fn compile(expr: &AlgebraicExpression<F>, id_to_idx: &[usize], zero: F, one: F) -> Self {
        let d = Self::decompose(expr, id_to_idx, zero, one);
        if d.linear.is_empty() && d.products.is_empty() {
            CompiledExpr::Constant(d.constant)
        } else if d.linear.len() == 1
            && d.products.is_empty()
            && d.constant == zero
            && d.linear[0].1 == one
        {
            CompiledExpr::DirectLoad(d.linear[0].0)
        } else {
            CompiledExpr::Polynomial {
                constant: d.constant,
                linear: d.linear,
                products: d.products,
            }
        }
    }

    /// Evaluate the compiled expression against a row slice.
    #[inline(always)]
    pub fn eval(&self, row: &[F]) -> F {
        match self {
            CompiledExpr::Constant(c) => *c,
            CompiledExpr::DirectLoad(idx) => row[*idx],
            CompiledExpr::Polynomial {
                constant,
                linear,
                products,
            } => {
                let lin = linear
                    .iter()
                    .fold(*constant, |acc, &(idx, coeff)| acc + coeff * row[idx]);
                products
                    .iter()
                    .fold(lin, |acc, &(a, b, coeff)| acc + coeff * row[a] * row[b])
            }
        }
    }

    /// Decompose an expression into constant + linear + quadratic components.
    /// Panics if the expression is degree > 2.
    fn decompose(
        expr: &AlgebraicExpression<F>,
        id_to_idx: &[usize],
        zero: F,
        one: F,
    ) -> Decomposition<F> {
        use powdr_expression::AlgebraicExpression as AE;
        match expr {
            AE::Number(c) => Decomposition {
                constant: *c,
                linear: vec![],
                products: vec![],
            },
            AE::Reference(r) => Decomposition {
                constant: zero,
                linear: vec![(id_to_idx[r.id as usize], one)],
                products: vec![],
            },
            AE::BinaryOperation(AlgebraicBinaryOperation { left, op, right }) => {
                let l = Self::decompose(left, id_to_idx, zero, one);
                let r = Self::decompose(right, id_to_idx, zero, one);
                match op {
                    AlgebraicBinaryOperator::Add => Decomposition {
                        constant: l.constant + r.constant,
                        linear: l.linear.into_iter().chain(r.linear).collect(),
                        products: l.products.into_iter().chain(r.products).collect(),
                    },
                    AlgebraicBinaryOperator::Sub => Decomposition {
                        constant: l.constant - r.constant,
                        linear: l
                            .linear
                            .into_iter()
                            .chain(r.linear.into_iter().map(|(i, c)| (i, -c)))
                            .collect(),
                        products: l
                            .products
                            .into_iter()
                            .chain(r.products.into_iter().map(|(a, b, c)| (a, b, -c)))
                            .collect(),
                    },
                    AlgebraicBinaryOperator::Mul => Self::mul_decompositions(l, r, zero),
                }
            }
            AE::UnaryOperation(powdr_expression::AlgebraicUnaryOperation { op, expr }) => {
                match op {
                    AlgebraicUnaryOperator::Minus => {
                        let d = Self::decompose(expr, id_to_idx, zero, one);
                        Decomposition {
                            constant: -d.constant,
                            linear: d.linear.into_iter().map(|(i, c)| (i, -c)).collect(),
                            products: d.products.into_iter().map(|(a, b, c)| (a, b, -c)).collect(),
                        }
                    }
                }
            }
        }
    }

    /// Multiply two decompositions. Supports constant * anything and
    /// linear * linear (producing quadratic products).
    /// Panics if the result would be degree > 2.
    fn mul_decompositions(l: Decomposition<F>, r: Decomposition<F>, zero: F) -> Decomposition<F> {
        // If either side is purely constant, scale the other.
        if l.linear.is_empty() && l.products.is_empty() {
            return Decomposition {
                constant: l.constant * r.constant,
                linear: r
                    .linear
                    .into_iter()
                    .map(|(i, c)| (i, c * l.constant))
                    .collect(),
                products: r
                    .products
                    .into_iter()
                    .map(|(a, b, c)| (a, b, c * l.constant))
                    .collect(),
            };
        }
        if r.linear.is_empty() && r.products.is_empty() {
            return Decomposition {
                constant: l.constant * r.constant,
                linear: l
                    .linear
                    .into_iter()
                    .map(|(i, c)| (i, c * r.constant))
                    .collect(),
                products: l
                    .products
                    .into_iter()
                    .map(|(a, b, c)| (a, b, c * r.constant))
                    .collect(),
            };
        }
        // Both sides have variable terms. Products of quadratic with anything → degree > 2.
        assert!(
            l.products.is_empty() && r.products.is_empty(),
            "Bus interaction expression is degree > 2. This is unexpected."
        );
        // (c_l + sum(a_i * x_i)) * (c_r + sum(b_j * y_j))
        // = c_l*c_r + c_l*sum(b_j*y_j) + c_r*sum(a_i*x_i) + sum(a_i*b_j * x_i*y_j)
        let mut linear = Vec::new();
        let mut products = Vec::new();

        // c_l * linear_r
        if l.constant != zero {
            linear.extend(r.linear.iter().map(|&(i, c)| (i, c * l.constant)));
        }
        // c_r * linear_l
        if r.constant != zero {
            linear.extend(l.linear.iter().map(|&(i, c)| (i, c * r.constant)));
        }
        // linear_l * linear_r → quadratic products
        for &(i, ci) in &l.linear {
            for &(j, cj) in &r.linear {
                products.push((i, j, ci * cj));
            }
        }

        Decomposition {
            constant: l.constant * r.constant,
            linear,
            products,
        }
    }
}

/// Pre-compiled bus interaction for fast per-row evaluation.
pub struct CompiledBusInteraction<F> {
    pub id: u64,
    pub mult: CompiledExpr<F>,
    pub args: Vec<CompiledExpr<F>>,
}

impl<F> CompiledBusInteraction<F>
where
    F: Add<Output = F> + Sub<Output = F> + Mul<Output = F> + Neg<Output = F> + Copy + PartialEq,
{
    /// Compile all bus interactions from symbolic form.
    /// `zero` and `one` are the field's additive and multiplicative identities.
    pub fn compile_all(
        interactions: &[SymbolicBusInteraction<F>],
        id_to_idx: &[usize],
        zero: F,
        one: F,
    ) -> Vec<Self> {
        interactions
            .iter()
            .map(|bi| CompiledBusInteraction {
                id: bi.id,
                mult: CompiledExpr::compile(&bi.mult, id_to_idx, zero, one),
                args: bi
                    .args
                    .iter()
                    .map(|a| CompiledExpr::compile(a, id_to_idx, zero, one))
                    .collect(),
            })
            .collect()
    }
}
