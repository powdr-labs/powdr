use std::sync::Arc;

use powdr_ast::analyzed::{AlgebraicExpression, Challenge};
use powdr_ast::analyzed::{AlgebraicReference, Expression, PolyID, PolynomialType};
use powdr_ast::parsed::types::Type;
use powdr_number::{BigInt, FieldElement};
use powdr_pil_analyzer::evaluator::{self, Definitions, EvalError, SymbolLookup, Value};

use super::{rows::RowPair, Constraint, EvalResult, EvalValue, FixedData, IncompleteCause};

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

    pub fn process_query(&mut self, rows: &RowPair<T>, poly_id: &PolyID) -> EvalResult<'a, T> {
        let start = std::time::Instant::now();
        let column = &self.fixed_data.witness_cols[poly_id];

        if let Some(query) = column.query.as_ref() {
            if rows.get_value(&column.poly).is_none() {
                let r = self.process_witness_query(query, &column.poly, rows);
                let end = start.elapsed();
                println!("Query processing time: {} us", end.as_micros());
                if let Ok(ref r) = r {
                    println!("Result size: {}", r.constraints.len());
                }
                return r;
            }
        }

        // Either no query or the value is already known.
        Ok(EvalValue::complete(vec![]))
    }

    fn process_witness_query(
        &mut self,
        query: &'a Expression,
        poly: &'a AlgebraicReference,
        rows: &RowPair<T>,
    ) -> EvalResult<'a, T> {
        let (query_str, assignments) = match self.interpolate_query(query, rows) {
            Ok(query) => query,
            Err(e) => {
                return match e {
                    EvalError::DataNotAvailable => {
                        Ok(EvalValue::incomplete(IncompleteCause::DataNotYetAvailable))
                    }
                    // All other errors are non-recoverable
                    e => Err(super::EvalError::ProverQueryError(format!(
                        "Error occurred when evaluating prover query {query} on {}:\n{e:?}",
                        rows.current_row_index
                    ))),
                };
            }
        };
        let mut query_result = if let Some(value) =
            (self.query_callback)(&query_str).map_err(super::EvalError::ProverQueryError)?
        {
            EvalValue::complete(vec![(poly, Constraint::Assignment(value))])
        } else {
            EvalValue::incomplete(IncompleteCause::NoQueryAnswer(
                query_str,
                poly.name.to_string(),
            ))
        };

        if !assignments.is_empty() {
            let assignments = assignments
                .into_iter()
                .map(|(poly, value)| {
                    assert!(!poly.next);
                    (
                        // We need to find the reference in the rows to get a reference to the AlgebraicReference.
                        &self.fixed_data.witness_cols[&poly.poly_id].poly,
                        Constraint::Assignment(value),
                    )
                })
                .collect::<Vec<_>>();
            query_result.combine(EvalValue::complete(assignments));
        }

        Ok(query_result)
    }

    fn interpolate_query(
        &self,
        query: &'a Expression,
        rows: &RowPair<T>,
    ) -> Result<(String, Vec<(AlgebraicReference, T)>), EvalError> {
        let arguments = vec![Arc::new(Value::Integer(BigInt::from(u64::from(
            rows.current_row_index,
        ))))];
        let mut symbols = Symbols {
            fixed_data: self.fixed_data,
            rows,
            assignments: vec![],
        };
        let fun = evaluator::evaluate(query, &mut symbols)?;
        let result = evaluator::evaluate_function_call(fun, arguments, &mut symbols)?;

        Ok((result.to_string(), symbols.assignments))
    }
}

#[derive(Clone)]
struct Symbols<'a, T: FieldElement> {
    fixed_data: &'a FixedData<'a, T>,
    rows: &'a RowPair<'a, 'a, T>,
    assignments: Vec<(AlgebraicReference, T)>,
}

impl<'a, T: FieldElement> SymbolLookup<'a, T> for Symbols<'a, T> {
    fn lookup<'b>(
        &mut self,
        name: &'a str,
        type_args: Option<Vec<Type>>,
    ) -> Result<Arc<Value<'a, T>>, EvalError> {
        match self.fixed_data.analyzed.intermediate_columns.get(name) {
            // Intermediate polynomials (which includes challenges) are not inlined in hints,
            // so we need to look them up here.
            Some((symbol, expressions)) => {
                if let Some(type_args) = &type_args {
                    assert!(type_args.is_empty());
                }
                Ok(if symbol.is_array() {
                    Value::Array(
                        expressions
                            .clone()
                            .into_iter()
                            .map(|e| Value::from(e).into())
                            .collect(),
                    )
                } else {
                    assert!(expressions.len() == 1);
                    Value::from(expressions[0].clone())
                }
                .into())
            }
            None => Definitions::lookup_with_symbols(
                &self.fixed_data.analyzed.definitions,
                name,
                type_args,
                self,
            ),
        }
    }
    fn eval_reference(
        &self,
        poly_ref: &AlgebraicReference,
    ) -> Result<Arc<Value<'a, T>>, EvalError> {
        Ok(Value::FieldElement(match poly_ref.poly_id.ptype {
            PolynomialType::Committed | PolynomialType::Intermediate => self
                .rows
                .get_value(poly_ref)
                .ok_or(EvalError::DataNotAvailable)?,
            PolynomialType::Constant => {
                let values = self.fixed_data.fixed_cols[&poly_ref.poly_id].values;
                let row = self.rows.current_row_index + if poly_ref.next { 1 } else { 0 };
                values[usize::from(row)]
            }
        })
        .into())
    }

    fn eval_challenge(&self, challenge: &Challenge) -> Result<Arc<Value<'a, T>>, EvalError> {
        Ok(Value::FieldElement(self.fixed_data.challenges[&challenge.id]).into())
    }

    fn set_expr(&mut self, expr: &AlgebraicExpression<T>, value: T) -> Result<(), EvalError> {
        let AlgebraicExpression::Reference(reference) = expr else {
            return Err(EvalError::Unsupported(
                "Expected direct column or next reference in call to set()".to_string(),
            ));
        };
        self.assignments.push((reference.clone(), value));
        Ok(())
    }
}
