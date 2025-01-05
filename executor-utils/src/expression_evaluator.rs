use core::ops::{Add, Mul, Sub};
use std::collections::BTreeMap;

use powdr_ast::analyzed::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression as Expression,
    AlgebraicReference, AlgebraicReferenceThin, AlgebraicUnaryOperation, AlgebraicUnaryOperator,
    Analyzed, Challenge, PolyID, PolynomialType,
};
use powdr_number::FieldElement;
use powdr_number::LargeInt;

/// Accessor for trace values.
pub trait TraceValues<T> {
    fn get(&self, poly_ref: &AlgebraicReference) -> T;
}

/// Accessor for global values.
pub trait GlobalValues<T> {
    fn get_public(&self, public: &str) -> T;
    fn get_challenge(&self, challenge: &Challenge) -> T;
}

/// A simple container for trace values.
pub struct OwnedTraceValues<T> {
    pub values: BTreeMap<PolyID, Vec<T>>,
}

/// A view into the trace values for a single row.
pub struct RowTraceValues<'a, T> {
    trace: &'a OwnedTraceValues<T>,
    row: usize,
}

impl<T> OwnedTraceValues<T> {
    pub fn new(
        pil: &Analyzed<T>,
        witness_columns: Vec<(String, Vec<T>)>,
        fixed_columns: Vec<(String, Vec<T>)>,
    ) -> Self {
        let mut columns_by_name = witness_columns
            .into_iter()
            .chain(fixed_columns)
            .collect::<BTreeMap<_, _>>();
        let values = pil
            .committed_polys_in_source_order()
            .chain(pil.constant_polys_in_source_order())
            .flat_map(|(symbol, _)| symbol.array_elements())
            .filter_map(|(name, poly_id)| {
                columns_by_name
                    .remove(&name)
                    .map(|column| (poly_id, column))
            })
            .collect();
        Self { values }
    }

    pub fn height(&self) -> usize {
        self.values.values().next().map(|v| v.len()).unwrap()
    }

    pub fn row(&self, row: usize) -> RowTraceValues<T> {
        RowTraceValues { trace: self, row }
    }
}

impl<F: FieldElement> TraceValues<F> for RowTraceValues<'_, F> {
    fn get(&self, column: &AlgebraicReference) -> F {
        match column.poly_id.ptype {
            PolynomialType::Committed | PolynomialType::Constant => {
                let column_values = self.trace.values.get(&column.poly_id).unwrap();
                let row = (self.row + column.next as usize) % column_values.len();
                column_values[row]
            }
            PolynomialType::Intermediate => unreachable!(
                "Intermediate polynomials should have been handled by ExpressionEvaluator"
            ),
        }
    }
}

#[derive(Default)]
pub struct OwnedGlobalValues<T> {
    pub public_values: BTreeMap<String, T>,
    pub challenge_values: BTreeMap<u64, T>,
}

impl<T: Clone> GlobalValues<T> for &OwnedGlobalValues<T> {
    fn get_public(&self, public: &str) -> T {
        self.public_values[public].clone()
    }

    fn get_challenge(&self, challenge: &Challenge) -> T {
        self.challenge_values[&challenge.id].clone()
    }
}

/// Evaluates an algebraic expression to a value.
pub struct ExpressionEvaluator<'a, T, Expr, TV, GV> {
    trace_values: TV,
    global_values: GV,
    intermediate_definitions: &'a BTreeMap<AlgebraicReferenceThin, Expression<T>>,
    /// Maps intermediate reference to their evaluation. Updated throughout the lifetime of the
    /// ExpressionEvaluator.
    intermediates_cache: BTreeMap<AlgebraicReferenceThin, Expr>,
    to_expr: fn(&T) -> Expr,
}

impl<'a, T, TV, GV> ExpressionEvaluator<'a, T, T, TV, GV>
where
    TV: TraceValues<T>,
    GV: GlobalValues<T>,
    T: FieldElement,
{
    /// Create a new expression evaluator (for the case where Expr = T).
    pub fn new(
        trace_values: TV,
        global_values: GV,
        intermediate_definitions: &'a BTreeMap<AlgebraicReferenceThin, Expression<T>>,
    ) -> Self {
        Self::new_with_custom_expr(trace_values, global_values, intermediate_definitions, |x| {
            *x
        })
    }
}

impl<'a, T, Expr, TV, GV> ExpressionEvaluator<'a, T, Expr, TV, GV>
where
    TV: TraceValues<Expr>,
    GV: GlobalValues<Expr>,
    Expr: Clone + Add<Output = Expr> + Sub<Output = Expr> + Mul<Output = Expr>,
    T: FieldElement,
{
    /// Create a new expression evaluator with custom expression converters.
    pub fn new_with_custom_expr(
        trace_values: TV,
        global_values: GV,
        intermediate_definitions: &'a BTreeMap<AlgebraicReferenceThin, Expression<T>>,
        to_expr: fn(&T) -> Expr,
    ) -> Self {
        Self {
            trace_values,
            global_values,
            intermediate_definitions,
            intermediates_cache: Default::default(),
            to_expr,
        }
    }

    pub fn evaluate(&mut self, expr: &'a Expression<T>) -> Expr {
        match expr {
            Expression::Reference(reference) => match reference.poly_id.ptype {
                PolynomialType::Committed => self.trace_values.get(reference),
                PolynomialType::Constant => self.trace_values.get(reference),
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
            Expression::PublicReference(_public) => unimplemented!(),
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
                AlgebraicUnaryOperator::Minus => self.evaluate(expr),
            },
            Expression::Challenge(challenge) => self.global_values.get_challenge(challenge),
        }
    }
}
