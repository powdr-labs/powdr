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

// ---- Compiled expression evaluation (O2 optimization) ----

use powdr_expression::{AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicUnaryOperator};

/// Pre-compiled expression for fast per-row evaluation without recursive AST walking.
pub enum CompiledExpr<F> {
    /// Compile-time constant.
    Constant(F),
    /// Single column load: row[col_idx].
    DirectLoad(usize),
    /// constant + sum(coeff * row[col_idx]).
    LinearCombination { constant: F, terms: Vec<(usize, F)> },
    /// Pre-resolved general expression (references already mapped to column indices).
    /// Used for degree-2+ expressions. The AlgebraicReference.id stores the column index directly.
    General(AlgebraicExpression<F>),
}

impl<F> CompiledExpr<F>
where
    F: Add<Output = F> + Sub<Output = F> + Mul<Output = F> + Neg<Output = F> + Copy + PartialEq,
{
    /// Compile an expression at build time into a fast-eval form.
    /// `zero` and `one` are the field's additive and multiplicative identities.
    pub fn compile(expr: &AlgebraicExpression<F>, id_to_idx: &[usize], zero: F, one: F) -> Self {
        match Self::try_linearize(expr, id_to_idx, zero, one) {
            Some((c, terms)) if terms.is_empty() => CompiledExpr::Constant(c),
            Some((c, terms)) if terms.len() == 1 && c == zero && terms[0].1 == one => {
                CompiledExpr::DirectLoad(terms[0].0)
            }
            Some((constant, terms)) => CompiledExpr::LinearCombination { constant, terms },
            None => {
                // Degree >= 2: pre-resolve references so eval_var can index row directly.
                CompiledExpr::General(Self::resolve_refs(expr, id_to_idx))
            }
        }
    }

    /// Evaluate the compiled expression against a row slice.
    #[inline(always)]
    pub fn eval(&self, row: &[F]) -> F {
        match self {
            CompiledExpr::Constant(c) => *c,
            CompiledExpr::DirectLoad(idx) => row[*idx],
            CompiledExpr::LinearCombination { constant, terms } => {
                let mut acc = *constant;
                for &(idx, coeff) in terms {
                    acc = acc + coeff * row[idx];
                }
                acc
            }
            CompiledExpr::General(expr) => {
                // References already pre-resolved: id stores column index directly.
                expr.to_expression(&|n| *n, &|var| row[var.id as usize])
            }
        }
    }

    /// Pre-resolve all references in an expression: replace each reference's id
    /// with its column index so eval can do `row[id]` directly.
    fn resolve_refs(expr: &AlgebraicExpression<F>, id_to_idx: &[usize]) -> AlgebraicExpression<F> {
        use powdr_expression::AlgebraicExpression as AE;
        match expr {
            AE::Number(c) => AE::Number(*c),
            AE::Reference(r) => {
                let mut resolved = r.clone();
                resolved.id = id_to_idx[r.id as usize] as u64;
                AE::Reference(resolved)
            }
            AE::BinaryOperation(AlgebraicBinaryOperation { left, op, right }) => {
                AE::new_binary(
                    Self::resolve_refs(left, id_to_idx),
                    *op,
                    Self::resolve_refs(right, id_to_idx),
                )
            }
            AE::UnaryOperation(powdr_expression::AlgebraicUnaryOperation { op, expr }) => {
                AE::new_unary(*op, Self::resolve_refs(expr, id_to_idx))
            }
        }
    }

    /// Try to decompose expr into (constant, Vec<(col_idx, coeff)>).
    /// Returns None if expression contains col*col (degree >= 2).
    fn try_linearize(
        expr: &AlgebraicExpression<F>,
        id_to_idx: &[usize],
        zero: F,
        one: F,
    ) -> Option<(F, Vec<(usize, F)>)> {
        use powdr_expression::AlgebraicExpression as AE;
        match expr {
            AE::Number(c) => Some((*c, vec![])),
            AE::Reference(r) => {
                let idx = id_to_idx[r.id as usize];
                Some((zero, vec![(idx, one)]))
            }
            AE::BinaryOperation(AlgebraicBinaryOperation { left, op, right }) => match op {
                AlgebraicBinaryOperator::Add => {
                    let (c_l, mut t_l) = Self::try_linearize(left, id_to_idx, zero, one)?;
                    let (c_r, t_r) = Self::try_linearize(right, id_to_idx, zero, one)?;
                    t_l.extend(t_r);
                    Some((c_l + c_r, t_l))
                }
                AlgebraicBinaryOperator::Sub => {
                    let (c_l, mut t_l) = Self::try_linearize(left, id_to_idx, zero, one)?;
                    let (c_r, t_r) = Self::try_linearize(right, id_to_idx, zero, one)?;
                    for (idx, coeff) in t_r {
                        t_l.push((idx, -coeff));
                    }
                    Some((c_l - c_r, t_l))
                }
                AlgebraicBinaryOperator::Mul => {
                    let (c_l, t_l) = Self::try_linearize(left, id_to_idx, zero, one)?;
                    let (c_r, t_r) = Self::try_linearize(right, id_to_idx, zero, one)?;
                    if t_r.is_empty() {
                        let terms = t_l.into_iter().map(|(i, c)| (i, c * c_r)).collect();
                        Some((c_l * c_r, terms))
                    } else if t_l.is_empty() {
                        let terms = t_r.into_iter().map(|(i, c)| (i, c * c_l)).collect();
                        Some((c_l * c_r, terms))
                    } else {
                        None
                    }
                }
            },
            AE::UnaryOperation(powdr_expression::AlgebraicUnaryOperation { op, expr }) => {
                match op {
                    AlgebraicUnaryOperator::Minus => {
                        let (c, terms) = Self::try_linearize(expr, id_to_idx, zero, one)?;
                        let neg_terms = terms.into_iter().map(|(i, c)| (i, -c)).collect();
                        Some((-c, neg_terms))
                    }
                }
            }
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
