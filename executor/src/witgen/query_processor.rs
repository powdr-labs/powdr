use ast::{
    analyzed::{Expression, PolyID, PolynomialReference, Reference},
    parsed::{MatchArm, MatchPattern},
};
use number::FieldElement;

use super::{rows::RowPair, Constraint, EvalValue, FixedData, IncompleteCause, Query};

/// Computes value updates that result from a query.
pub struct QueryProcessor<'a, 'b, T: FieldElement, QueryCallback: Send + Sync> {
    fixed_data: &'a FixedData<'a, T>,
    query_callback: &'b mut QueryCallback,
}

impl<'a, 'b, T: FieldElement, QueryCallback> QueryProcessor<'a, 'b, T, QueryCallback>
where
    QueryCallback: FnMut(&str) -> Option<T> + Send + Sync,
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
    ) -> EvalValue<&'a PolynomialReference, T> {
        let column = &self.fixed_data.witness_cols[poly_id];

        if let Some(query) = column.query.as_ref() {
            if rows.get_value(&query.poly).is_none() {
                return self.process_witness_query(query, rows);
            }
        }
        // Either no query or the value is already known.
        EvalValue::complete(vec![])
    }

    fn process_witness_query(
        &mut self,
        query: &'a Query<'_, T>,
        rows: &RowPair<T>,
    ) -> EvalValue<&'a PolynomialReference, T> {
        let query_str = match interpolate_query(query.expr, rows) {
            Ok(query) => query,
            Err(incomplete) => return EvalValue::incomplete(incomplete),
        };
        if let Some(value) = (self.query_callback)(&query_str) {
            EvalValue::complete(vec![(&query.poly, Constraint::Assignment(value))])
        } else {
            EvalValue::incomplete(IncompleteCause::NoQueryAnswer(
                query_str,
                query.poly.name.to_string(),
            ))
        }
    }
}

fn interpolate_query<'b, T: FieldElement>(
    query: &'b Expression<T>,
    rows: &RowPair<T>,
) -> Result<String, IncompleteCause<&'b PolynomialReference>> {
    // TODO combine that with the constant evaluator and the commit evaluator...
    match query {
        Expression::Tuple(items) => Ok(items
            .iter()
            .map(|i| interpolate_query(i, rows))
            .collect::<Result<Vec<_>, _>>()?
            .join(", ")),
        Expression::Reference(Reference::LocalVar(i, _name)) => {
            assert!(*i == 0);
            Ok(format!("{}", rows.current_row_index))
        }
        Expression::String(s) => Ok(format!(
            "\"{}\"",
            s.replace('\\', "\\\\").replace('"', "\\\"")
        )),
        Expression::MatchExpression(scrutinee, arms) => {
            let v = rows
                .evaluate(scrutinee)?
                .constant_value()
                .ok_or(IncompleteCause::NonConstantQueryMatchScrutinee)?;
            let expr = arms
                .iter()
                .find_map(|MatchArm { pattern, value }| {
                    (match pattern {
                        MatchPattern::CatchAll => true,
                        MatchPattern::Pattern(pattern) => {
                            rows.evaluate(pattern).unwrap().constant_value() == Some(v)
                        }
                    })
                    .then_some(value)
                })
                .ok_or(IncompleteCause::NoMatchArmFound)?;
            interpolate_query(expr, rows)
        }
        _ => rows
            .evaluate(query)?
            .constant_value()
            .map(|c| c.to_string())
            .ok_or(IncompleteCause::NonConstantQueryElement),
    }
}
