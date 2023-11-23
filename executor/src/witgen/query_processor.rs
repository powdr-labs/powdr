use ast::{
    analyzed::{AlgebraicReference, Expression, PolyID, PolynomialType, Reference},
    evaluate_binary_operation, evaluate_unary_operation,
    parsed::{FunctionCall, MatchArm, MatchPattern},
};

use number::{DegreeType, FieldElement};

use super::{rows::RowPair, Constraint, EvalValue, FixedData, IncompleteCause};

/// Computes value updates that result from a query.
pub struct QueryProcessor<'a, 'b, T: FieldElement, QueryCallback: Send + Sync> {
    fixed_data: &'a FixedData<'a, T>,
    query_callback: &'b mut QueryCallback,
}

impl<'a, 'b, T: FieldElement, QueryCallback: super::QueryCallback<T>>
    QueryProcessor<'a, 'b, T, QueryCallback>
{
    pub fn new(fixed_data: &'a FixedData<'a, T>, query_callback: &'b mut QueryCallback) -> Self {
        Self {
            fixed_data,
            query_callback,
        }
    }

    pub fn process_query(
        &mut self,
        rows: &RowPair<T>,
        poly_id: &PolyID,
    ) -> EvalValue<&'a AlgebraicReference, T> {
        let column = &self.fixed_data.witness_cols[poly_id];

        if let Some(query) = column.query.as_ref() {
            if rows.get_value(&column.poly).is_none() {
                return self.process_witness_query(query, &column.poly, rows);
            }
        }
        // Either no query or the value is already known.
        EvalValue::complete(vec![])
    }

    fn process_witness_query(
        &mut self,
        query: &'a Expression<T>,
        poly: &'a AlgebraicReference,
        rows: &RowPair<T>,
    ) -> EvalValue<&'a AlgebraicReference, T> {
        let query_str = match self.interpolate_query(query, rows) {
            Ok(query) => query,
            Err(incomplete) => return EvalValue::incomplete(incomplete),
        };
        if let Some(value) = (self.query_callback)(&query_str) {
            EvalValue::complete(vec![(poly, Constraint::Assignment(value))])
        } else {
            EvalValue::incomplete(IncompleteCause::NoQueryAnswer(
                query_str,
                poly.name.to_string(),
            ))
        }
    }

    fn interpolate_query(
        &self,
        query: &'a Expression<T>,
        rows: &RowPair<T>,
    ) -> Result<String, IncompleteCause<&'a AlgebraicReference>> {
        // TODO combine that with the constant evaluator and the commit evaluator...
        match query {
            Expression::Tuple(items) => Ok(items
                .iter()
                .map(|i| self.interpolate_query(i, rows))
                .collect::<Result<Vec<_>, _>>()?
                .join(", ")),
            Expression::String(s) => Ok(format!(
                "\"{}\"",
                s.replace('\\', "\\\\").replace('"', "\\\"")
            )),
            Expression::MatchExpression(scrutinee, arms) => {
                let v = self.evaluate_expression(scrutinee, rows)?;
                let expr = arms
                    .iter()
                    .find_map(|MatchArm { pattern, value }| match pattern {
                        MatchPattern::CatchAll => Some(Ok(value)),
                        MatchPattern::Pattern(pattern) => {
                            match self.evaluate_expression(pattern, rows) {
                                Ok(p) => (p == v).then_some(Ok(value)),
                                Err(e) => Some(Err(e)),
                            }
                        }
                    })
                    .ok_or(IncompleteCause::NoMatchArmFound)??;
                self.interpolate_query(expr, rows)
            }
            _ => self.evaluate_expression(query, rows).map(|v| v.to_string()),
        }
    }

    fn evaluate_expression(
        &self,
        expr: &'a Expression<T>,
        rows: &RowPair<T>,
    ) -> Result<T, IncompleteCause<&'a AlgebraicReference>> {
        match expr {
            Expression::Number(n) => Ok(*n),
            Expression::BinaryOperation(left, op, right) => Ok(evaluate_binary_operation(
                self.evaluate_expression(left, rows)?,
                *op,
                self.evaluate_expression(right, rows)?,
            )),
            Expression::UnaryOperation(op, inner) => Ok(evaluate_unary_operation(
                *op,
                self.evaluate_expression(inner, rows)?,
            )),
            Expression::Reference(Reference::LocalVar(i, _name)) => {
                assert!(*i == 0);
                Ok(rows.current_row_index.into())
            }
            Expression::FunctionCall(FunctionCall { id, arguments }) => {
                let [arg] = &arguments[..] else {
                    return Err(IncompleteCause::ExpressionEvaluationUnimplemented(
                        expr.to_string(),
                    ));
                };
                let arg = self.evaluate_expression(arg, rows)?;
                self.evaluate_poly(id.to_string().as_str(), arg.to_degree(), rows)
            }
            e => Err(IncompleteCause::ExpressionEvaluationUnimplemented(
                e.to_string(),
            )),
        }
    }

    fn evaluate_poly(
        &self,
        poly: &str,
        row: DegreeType,
        rows: &RowPair<T>,
    ) -> Result<T, IncompleteCause<&'a AlgebraicReference>> {
        let poly_id = self.fixed_data.column_by_name(poly);
        match poly_id.ptype {
            PolynomialType::Committed | PolynomialType::Intermediate => {
                let next = if row == rows.current_row_index {
                    false
                } else if row == rows.current_row_index + 1 {
                    true
                } else {
                    return Err(IncompleteCause::ExpressionEvaluationUnimplemented(
                        "Referenced witness column outside of evaluation window.".to_string(),
                    ));
                };

                let poly_ref = AlgebraicReference {
                    name: poly.to_string(),
                    poly_id,
                    next,
                };
                Ok(rows
                    .get_value(&poly_ref)
                    .ok_or(IncompleteCause::DataNotYetAvailable)?)
            }
            PolynomialType::Constant => {
                let values = self.fixed_data.fixed_cols[&poly_id].values;
                Ok(values[row as usize % values.len()])
            }
        }
    }
}
