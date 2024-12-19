use std::collections::BTreeMap;

use powdr_ast::analyzed::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression as Expression,
    AlgebraicReference, AlgebraicReferenceThin, AlgebraicUnaryOperation, AlgebraicUnaryOperator,
    Analyzed, PolyID, PolynomialType,
};
use powdr_number::FieldElement;

/// Accessor for trace values.
pub trait TraceValues<T> {
    fn get(&self, poly_id: &AlgebraicReference) -> T;
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

/// Evaluates an algebraic expression to a value.
pub struct ExpressionEvaluator<'a, T, SV> {
    trace_values: SV,
    intermediate_definitions: &'a BTreeMap<AlgebraicReferenceThin, Expression<T>>,
    challenges: &'a BTreeMap<u64, T>,
    /// Maps intermediate reference to their evaluation. Updated throughout the lifetime of the
    /// ExpressionEvaluator.
    intermediates_cache: BTreeMap<AlgebraicReferenceThin, T>,
}

impl<'a, T, TV> ExpressionEvaluator<'a, T, TV>
where
    TV: TraceValues<T>,
    T: FieldElement,
{
    pub fn new(
        variables: TV,
        intermediate_definitions: &'a BTreeMap<AlgebraicReferenceThin, Expression<T>>,
        challenges: &'a BTreeMap<u64, T>,
    ) -> Self {
        Self {
            trace_values: variables,
            intermediate_definitions,
            challenges,
            intermediates_cache: Default::default(),
        }
    }

    pub fn evaluate(&mut self, expr: &'a Expression<T>) -> T {
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
                            self.intermediates_cache.insert(reference, result);
                            result
                        }
                    }
                }
            },
            Expression::PublicReference(_public) => unimplemented!(),
            Expression::Number(n) => *n,
            Expression::BinaryOperation(AlgebraicBinaryOperation { left, op, right }) => match op {
                AlgebraicBinaryOperator::Add => self.evaluate(left) + self.evaluate(right),
                AlgebraicBinaryOperator::Sub => self.evaluate(left) - self.evaluate(right),
                AlgebraicBinaryOperator::Mul => self.evaluate(left) * self.evaluate(right),
                AlgebraicBinaryOperator::Pow => {
                    self.evaluate(left).pow(self.evaluate(right).to_integer())
                }
            },
            Expression::UnaryOperation(AlgebraicUnaryOperation { op, expr }) => match op {
                AlgebraicUnaryOperator::Minus => self.evaluate(expr),
            },
            Expression::Challenge(challenge) => self.challenges[&challenge.id],
        }
    }
}
