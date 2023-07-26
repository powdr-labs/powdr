use ast::analyzed::{Expression, PolynomialReference};
use number::FieldElement;

use super::{
    affine_expression::AffineResult, expression_evaluator::ExpressionEvaluator, rows::RowPair,
    symbolic_witness_evaluator::SymoblicWitnessEvaluator, FixedData, IncompleteCause,
    WitnessColumn,
};

pub struct QueryProcessor<'a, T: FieldElement, QueryCallback: Send + Sync> {
    pub fixed_data: &'a FixedData<'a, T>,
    query_callback: Option<QueryCallback>,
}

impl<'a, T: FieldElement, QueryCallback> QueryProcessor<'a, T, QueryCallback>
where
    QueryCallback: FnMut(&str) -> Option<T> + Send + Sync,
{
    pub fn new(fixed_data: &'a FixedData<'a, T>, query_callback: Option<QueryCallback>) -> Self {
        Self {
            fixed_data,
            query_callback,
        }
    }

    pub fn process_queries_on_current_row(&mut self, rows: &mut RowPair<T>) -> bool {
        match &self.query_callback {
            None => false,
            Some(_) => {
                let mut progress = false;
                for column in self.fixed_data.witness_cols() {
                    if rows.get_value(&column.poly).is_none() && column.query.is_some() {
                        progress |= self.process_witness_query(column, rows);
                    }
                }

                progress
            }
        }
    }

    fn process_witness_query(
        &mut self,
        column: &'a WitnessColumn<T>,
        rows: &mut RowPair<T>,
    ) -> bool {
        let query = match self.interpolate_query(column.query.unwrap(), rows) {
            Ok(query) => query,
            Err(_) => return false,
        };
        if let Some(value) = self.query_callback.as_mut().and_then(|c| (c)(&query)) {
            log::trace!("    Processing query: {}", query);
            rows.set_value(&column.poly, value);
            true
        } else {
            false
        }
    }

    fn interpolate_query<'b>(
        &self,
        query: &'b Expression<T>,
        rows: &RowPair<T>,
    ) -> Result<String, IncompleteCause<&'b PolynomialReference>> {
        // TODO combine that with the constant evaluator and the commit evaluator...
        match query {
            Expression::Tuple(items) => Ok(items
                .iter()
                .map(|i| self.interpolate_query(i, rows))
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
                let v = self
                    .evaluate(scrutinee, rows)?
                    .constant_value()
                    .ok_or(IncompleteCause::NonConstantQueryMatchScrutinee)?;
                let (_, expr) = arms
                    .iter()
                    .find(|(n, _)| n.is_none() || n.as_ref() == Some(&v))
                    .ok_or(IncompleteCause::NoMatchArmFound)?;
                self.interpolate_query(expr, rows)
            }
            _ => self
                .evaluate(query, rows)?
                .constant_value()
                .map(|c| c.to_string())
                .ok_or(IncompleteCause::NonConstantQueryElement),
        }
    }

    fn evaluate<'b>(
        &self,
        expr: &'b Expression<T>,
        rows: &RowPair<T>,
    ) -> AffineResult<&'b PolynomialReference, T> {
        ExpressionEvaluator::new(SymoblicWitnessEvaluator::new(
            self.fixed_data,
            rows.current_row_index,
            rows,
        ))
        .evaluate(expr)
    }
}
