use std::rc::Rc;

use powdr_ast::analyzed::{
    types::Type, AlgebraicExpression, AlgebraicReference, Expression, PolyID, PolynomialType,
};
use powdr_number::FieldElement;
use powdr_pil_analyzer::evaluator::{self, Definitions, EvalError, NoCustom, SymbolLookup, Value};

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
        let column = &self.fixed_data.witness_cols[poly_id];

        if let Some(query) = column.query.as_ref() {
            if rows.get_value(&column.poly).is_none() {
                return self.process_witness_query(query, &column.poly, rows);
            }
        }
        // Either no query or the value is already known.
        Ok(EvalValue::complete(vec![]))
    }

    fn process_witness_query(
        &mut self,
        query: &'a Expression<T>,
        poly: &'a AlgebraicReference,
        rows: &RowPair<T>,
    ) -> EvalResult<'a, T> {
        let query_str = match self.interpolate_query(query, rows) {
            Ok(query) => query,
            Err(e) => {
                return match e {
                    // TODO this mechanism should be replaced by a proper Option datatype.
                    EvalError::NoMatch() => Ok(EvalValue::complete(vec![])),
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
        Ok(
            if let Some(value) =
                (self.query_callback)(&query_str).map_err(super::EvalError::ProverQueryError)?
            {
                EvalValue::complete(vec![(poly, Constraint::Assignment(value))])
            } else {
                EvalValue::incomplete(IncompleteCause::NoQueryAnswer(
                    query_str,
                    poly.name.to_string(),
                ))
            },
        )
    }

    fn interpolate_query(
        &self,
        query: &'a Expression<T>,
        rows: &RowPair<T>,
    ) -> Result<String, EvalError> {
        let arguments = vec![Rc::new(Value::Integer(num_bigint::BigInt::from(
            u64::from(rows.current_row_index),
        )))];
        let symbols = Symbols {
            fixed_data: self.fixed_data,
            rows,
        };
        let fun = evaluator::evaluate(query, &symbols)?;
        evaluator::evaluate_function_call(fun, arguments, &symbols).map(|v| v.to_string())
    }
}

#[derive(Clone)]
struct Symbols<'a, T: FieldElement> {
    fixed_data: &'a FixedData<'a, T>,
    rows: &'a RowPair<'a, 'a, T>,
}

impl<'a, T: FieldElement> SymbolLookup<'a, T, NoCustom> for Symbols<'a, T> {
    fn lookup<'b>(
        &self,
        name: &'a str,
        generic_args: Option<Vec<Type>>,
    ) -> Result<Value<'a, T, NoCustom>, EvalError> {
        Definitions(&self.fixed_data.analyzed.definitions).lookup(name, generic_args)
    }

    fn eval_expr(&self, expr: AlgebraicExpression<T>) -> Result<Value<'a, T, NoCustom>, EvalError> {
        let AlgebraicExpression::Reference(poly_ref) = expr else {
            return Err(EvalError::TypeError(format!(
                "Can use std::prover::eval only directly on columns - tried to evaluate {expr}"
            )));
        };

        Ok(Value::FieldElement(match poly_ref.poly_id.ptype {
            PolynomialType::Committed | PolynomialType::Intermediate => self
                .rows
                .get_value(&poly_ref)
                .ok_or(EvalError::DataNotAvailable)?,
            PolynomialType::Constant => {
                let values = self.fixed_data.fixed_cols[&poly_ref.poly_id].values;
                let row = self.rows.current_row_index + if poly_ref.next { 1 } else { 0 };
                values[usize::try_from(row).unwrap()]
            }
        }))
    }
}
