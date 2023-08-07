use ast::analyzed::{Expression, PolynomialReference};
use number::FieldElement;

use super::{rows::RowPair, Constraint, EvalValue, FixedData, IncompleteCause, WitnessColumn};

pub struct QueryProcessor<'a, T: FieldElement, QueryCallback: Send + Sync> {
    fixed_data: &'a FixedData<'a, T>,
    query_callback: QueryCallback,
}

impl<'a, T: FieldElement, QueryCallback> QueryProcessor<'a, T, QueryCallback>
where
    QueryCallback: FnMut(&str) -> Option<T> + Send + Sync,
{
    pub fn new(fixed_data: &'a FixedData<'a, T>, query_callback: QueryCallback) -> Self {
        Self {
            fixed_data,
            query_callback,
        }
    }

    pub fn process_queries_on_current_row(
        &mut self,
        rows: &RowPair<T>,
    ) -> EvalValue<&'a PolynomialReference, T> {
        let mut eval_value = EvalValue::complete(vec![]);
        for column in self.fixed_data.witness_cols.values() {
            // TODO: What if the column is not part of the machine?
            // Do something iff:
            // 1. This witness column has a query
            // 2. This witness column has not been set yet
            if column.query.is_some()
                && rows
                    .get_value(&column.query.as_ref().unwrap().poly)
                    .is_none()
            {
                eval_value.combine(self.process_witness_query(column, rows));
            }
        }
        eval_value
    }

    fn process_witness_query(
        &mut self,
        column: &'a WitnessColumn<T>,
        rows: &RowPair<T>,
    ) -> EvalValue<&'a PolynomialReference, T> {
        let query = column.query.as_ref().unwrap();
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
        Expression::LocalVariableReference(i) => {
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
            let (_, expr) = arms
                .iter()
                .find(|(n, _)| n.is_none() || n.as_ref() == Some(&v))
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
