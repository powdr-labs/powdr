use std::{fmt::Display, rc::Rc};

use num_traits::ToPrimitive;
use powdr_ast::analyzed::{AlgebraicReference, Expression, PolyID, PolynomialType};
use powdr_number::FieldElement;
use powdr_pil_analyzer::evaluator::{self, Custom, EvalError, SymbolLookup, Value};

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
        let arguments = vec![Rc::new(T::from(rows.current_row_index).into())];
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
    // TODO we should also provide access to non-column symbols.
    fixed_data: &'a FixedData<'a, T>,
    rows: &'a RowPair<'a, 'a, T>,
}

impl<'a, T: FieldElement> SymbolLookup<'a, T, Reference<'a>> for Symbols<'a, T> {
    fn lookup(&self, name: &'a str) -> Result<Value<'a, T, Reference<'a>>, EvalError> {
        match self.fixed_data.try_column_by_name(name) {
            Some(poly_id) => Ok(Value::Custom(Reference { name, poly_id })),
            None => Err(EvalError::SymbolNotFound(format!(
                "Symbol {name} not found."
            ))),
        }
    }
    fn eval_function_application(
        &self,
        function: Reference<'a>,
        arguments: &[Rc<Value<'a, T, Reference<'a>>>],
    ) -> Result<Value<'a, T, Reference<'a>>, EvalError> {
        if arguments.len() != 1 {
            Err(EvalError::TypeError(format!(
                "Expected one argument, but got {}",
                arguments.len()
            )))?
        };
        let Value::Number(row) = arguments[0].as_ref() else {
            return Err(EvalError::TypeError(format!(
                "Expected number but got {}",
                arguments[0]
            )));
        };
        Ok(Value::Number(match function.poly_id.ptype {
            PolynomialType::Committed | PolynomialType::Intermediate => {
                let next = self.rows.is_row_number_next(row.to_degree()).map_err(|_| {
                    EvalError::OutOfBounds(format!("Referenced row outside of window: {row}"))
                })?;
                let poly_ref = AlgebraicReference {
                    name: function.name.to_string(),
                    poly_id: function.poly_id,
                    next,
                };

                self.rows
                    .get_value(&poly_ref)
                    .ok_or(EvalError::DataNotAvailable)?
            }
            PolynomialType::Constant => {
                let values = self.fixed_data.fixed_cols[&function.poly_id].values;
                // TODO can we avoid bigint?
                values[(row.to_arbitrary_integer() % values.len())
                    .to_u64()
                    .unwrap() as usize]
            }
        }))
    }
}

#[derive(Clone, Debug)]
struct Reference<'a> {
    name: &'a str,
    poly_id: PolyID,
}

impl<'a> PartialEq for Reference<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.poly_id == other.poly_id
    }
}

impl<'a> Display for Reference<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl<'a> Custom for Reference<'a> {
    fn type_name(&self) -> String {
        "col".to_string()
    }
}
