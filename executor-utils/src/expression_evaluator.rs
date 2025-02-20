use core::ops::{Add, Mul, Sub};
use itertools::Itertools;
use std::collections::BTreeMap;
use std::ops::Neg;

use powdr_ast::analyzed::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression as Expression,
    AlgebraicReference, AlgebraicReferenceThin, AlgebraicUnaryOperation, AlgebraicUnaryOperator,
    Analyzed, Challenge, PolyID, PolynomialType,
};
use powdr_number::FieldElement;
use powdr_number::LargeInt;

/// Accessor for terminal symbols.
pub trait TerminalAccess<T> {
    fn get(&self, _poly_ref: &AlgebraicReference) -> T {
        unimplemented!();
    }
    fn get_public(&self, _public: &str) -> T {
        unimplemented!();
    }
    fn get_challenge(&self, _challenge: &Challenge) -> T {
        unimplemented!();
    }
}

/// A simple container for trace values.
pub struct OwnedTerminalValues<F> {
    pub trace: BTreeMap<PolyID, Vec<F>>,
    pub public_values: BTreeMap<String, F>,
    pub challenge_values: BTreeMap<u64, F>,
}

/// A view into the trace values for a single row.
pub struct RowValues<'a, F> {
    values: &'a OwnedTerminalValues<F>,
    row: usize,
}

impl<F: std::fmt::Debug> OwnedTerminalValues<F> {
    pub fn new(
        pil: &Analyzed<F>,
        witness_columns: Vec<(String, Vec<F>)>,
        fixed_columns: Vec<(String, Vec<F>)>,
    ) -> Self {
        let mut columns_by_name = witness_columns
            .into_iter()
            .chain(fixed_columns)
            .collect::<BTreeMap<_, _>>();
        let trace = pil
            .committed_polys_in_source_order()
            .chain(pil.constant_polys_in_source_order())
            .flat_map(|(symbol, _)| symbol.array_elements())
            .filter_map(|(name, poly_id)| {
                columns_by_name
                    .remove(&name)
                    .map(|column| (poly_id, column))
            })
            .collect();
        Self {
            trace,
            public_values: Default::default(),
            challenge_values: Default::default(),
        }
    }

    pub fn with_publics(mut self, publics: Vec<(String, F)>) -> Self {
        self.public_values = publics.into_iter().collect();
        self
    }

    pub fn with_challenges(mut self, challenges: BTreeMap<u64, F>) -> Self {
        self.challenge_values = challenges;
        self
    }

    /// The height of the trace. Panics if columns have different lengths.
    pub fn height(&self) -> usize {
        self.trace
            .values()
            .map(|v| v.len())
            .unique()
            .exactly_one()
            .unwrap()
    }

    /// The length of a given column.
    pub fn column_length(&self, poly_id: &PolyID) -> usize {
        self.trace.get(poly_id).unwrap().len()
    }

    pub fn row(&self, row: usize) -> RowValues<F> {
        RowValues { values: self, row }
    }

    pub fn into_trace(self) -> BTreeMap<PolyID, Vec<F>> {
        self.trace
    }
}

impl<F: FieldElement, T: From<F>> TerminalAccess<T> for RowValues<'_, F> {
    fn get(&self, column: &AlgebraicReference) -> T {
        match column.poly_id.ptype {
            PolynomialType::Committed | PolynomialType::Constant => {
                let column_values = self.values.trace.get(&column.poly_id).unwrap();
                let row = (self.row + column.next as usize) % column_values.len();
                column_values[row].into()
            }
            PolynomialType::Intermediate => unreachable!(
                "Intermediate polynomials should have been handled by ExpressionEvaluator"
            ),
        }
    }

    fn get_public(&self, public: &str) -> T {
        self.values.public_values[public].into()
    }

    fn get_challenge(&self, challenge: &Challenge) -> T {
        self.values.challenge_values[&challenge.id].into()
    }
}

/// Evaluates an algebraic expression to a value.
pub struct ExpressionEvaluator<'a, T, Expr, TA> {
    terminal_access: TA,
    intermediate_definitions: &'a BTreeMap<AlgebraicReferenceThin, Expression<T>>,
    /// Maps intermediate reference to their evaluation. Updated throughout the lifetime of the
    /// ExpressionEvaluator.
    intermediates_cache: BTreeMap<AlgebraicReferenceThin, Expr>,
    to_expr: fn(&T) -> Expr,
}

impl<'a, T, TA> ExpressionEvaluator<'a, T, T, TA>
where
    TA: TerminalAccess<T>,
    T: FieldElement,
{
    /// Create a new expression evaluator (for the case where Expr = T).
    pub fn new(
        terminal_access: TA,
        intermediate_definitions: &'a BTreeMap<AlgebraicReferenceThin, Expression<T>>,
    ) -> Self {
        Self::new_with_custom_expr(terminal_access, intermediate_definitions, |x| *x)
    }
}

impl<'a, T, Expr, TA> ExpressionEvaluator<'a, T, Expr, TA>
where
    TA: TerminalAccess<Expr>,
    Expr: Clone + Add<Output = Expr> + Sub<Output = Expr> + Mul<Output = Expr> + Neg<Output = Expr>,
    T: FieldElement,
{
    /// Create a new expression evaluator with custom expression converters.
    pub fn new_with_custom_expr(
        terminal_access: TA,
        intermediate_definitions: &'a BTreeMap<AlgebraicReferenceThin, Expression<T>>,
        to_expr: fn(&T) -> Expr,
    ) -> Self {
        Self {
            terminal_access,
            intermediate_definitions,
            intermediates_cache: Default::default(),
            to_expr,
        }
    }

    pub fn evaluate(&mut self, expr: &'a Expression<T>) -> Expr {
        match expr {
            Expression::Reference(reference) => match reference.poly_id.ptype {
                PolynomialType::Committed => self.terminal_access.get(reference),
                PolynomialType::Constant => self.terminal_access.get(reference),
                PolynomialType::Intermediate => {
                    let reference = reference.to_thin();
                    let value = self.intermediates_cache.get(&reference).cloned();
                    match value {
                        Some(v) => v,
                        None => {
                            let definition = self.intermediate_definitions.get(&reference).unwrap();
                            let result = self.evaluate(definition);
                            self.intermediates_cache.insert(reference, result.clone());
                            result
                        }
                    }
                }
            },
            Expression::PublicReference(public) => {
                println!("public: {}", public);
                self.terminal_access.get_public(public)
            }
            Expression::Number(n) => (self.to_expr)(n),
            Expression::BinaryOperation(AlgebraicBinaryOperation { left, op, right }) => match op {
                AlgebraicBinaryOperator::Add => self.evaluate(left) + self.evaluate(right),
                AlgebraicBinaryOperator::Sub => self.evaluate(left) - self.evaluate(right),
                AlgebraicBinaryOperator::Mul => self.evaluate(left) * self.evaluate(right),
                AlgebraicBinaryOperator::Pow => match &**right {
                    Expression::Number(n) => {
                        let left = self.evaluate(left);
                        (0u32..n.to_integer().try_into_u32().unwrap())
                            .fold((self.to_expr)(&T::one()), |acc, _| acc * left.clone())
                    }
                    _ => unimplemented!("pow with non-constant exponent"),
                },
            },
            Expression::UnaryOperation(AlgebraicUnaryOperation { op, expr }) => match op {
                AlgebraicUnaryOperator::Minus => -self.evaluate(expr),
            },
            Expression::Challenge(challenge) => self.terminal_access.get_challenge(challenge),
        }
    }
}
