use std::borrow::BorrowMut;
use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap};
use std::sync::Arc;

use powdr_ast::analyzed::Challenge;
use powdr_ast::analyzed::{AlgebraicReference, Expression, PolyID, PolynomialType};
use powdr_ast::parsed::types::Type;
use powdr_number::{BigInt, FieldElement};
use powdr_pil_analyzer::evaluator::{self, Definitions, EvalError, SymbolLookup, Value};

use super::machines::profiling::{record_end_identity, record_start_identity};
use super::IDENTITY_SNIPPET_ID;
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

    pub fn process_query(
        &mut self,
        rows: &RowPair<T>,
        poly_id: &PolyID,
    ) -> Option<EvalResult<'a, T>> {
        let column = &self.fixed_data.witness_cols[poly_id];

        if rows.get_value(&column.poly).is_none() {
            // Document that query must be present.
            Some(self.process_witness_query(column.query.unwrap(), &column.poly, rows))
        } else {
            // Either no query or the value is already known.
            None
        }
    }

    fn process_witness_query(
        &mut self,
        query: &'a Expression,
        poly: &'a AlgebraicReference,
        rows: &RowPair<T>,
    ) -> EvalResult<'a, T> {
        //record_start_identity(IDENTITY_SNIPPET_ID);

        // TODO
        // the X_free_value is the main query that is evaluated and it is done
        // twice per row.
        // One second of execution time for 6 seconds.
        // Number of rows: 39200
        // => 12.7 microseconds per evaluation.
        // Comparison: Constraint for X takes 52 milliseconds in total, meaning
        // 1.3 microseconds per evaluation.

        // But this function only has 2% of witgen time.

        // let poly_id = poly.poly_id;
        // COUNT_PER_POLY.with(|c| {
        //     c.borrow_mut()
        //         .entry(poly_id)
        //         .and_modify(|c| {
        //             *c += 1;
        //             let r: u64 = rows.current_row_index.into();

        //             if *c % 1000 == 0 {
        //                 println!(
        //                     "Count for {poly}: {c}, calls per row: {}",
        //                     (*c as f64) / (r as f64)
        //                 );
        //             }
        //         })
        //         .or_default();
        // });
        let query_str = match self.interpolate_query(query, rows) {
            Ok(query) => query,
            Err(e) => {
                //record_end_identity(IDENTITY_SNIPPET_ID);

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

        let r = Ok(
            if let Some(value) =
                (self.query_callback)(&query_str).map_err(super::EvalError::ProverQueryError)?
            {
                EvalValue::complete(vec![(poly, Constraint::Assignment(value))])
            } else {
                EvalValue::incomplete(IncompleteCause::NoQueryAnswer(
                    query_str,
                    // TODO What about this one? Is this called often?
                    poly.name.to_string(),
                ))
            },
        );
        //record_end_identity(IDENTITY_SNIPPET_ID);

        r
    }

    fn interpolate_query(
        &self,
        query: &'a Expression,
        rows: &RowPair<T>,
    ) -> Result<String, EvalError> {
        let arguments = vec![Arc::new(Value::Integer(BigInt::from(u64::from(
            rows.current_row_index,
        ))))];
        let mut symbols = Symbols {
            fixed_data: self.fixed_data,
            rows,
        };
        // From here to the end it's only 88 ms / 1.7%
        let fun = evaluator::evaluate(query, &mut symbols)?;
        let r =
            evaluator::evaluate_function_call(fun, arguments, &mut symbols).map(|v| v.to_string());
        r
    }
}

thread_local! {
    static COUNT_PER_POLY: RefCell<BTreeMap<PolyID, usize>> = const { RefCell::new(BTreeMap::new()) };
}

#[derive(Clone)]
struct Symbols<'a, T: FieldElement> {
    fixed_data: &'a FixedData<'a, T>,
    rows: &'a RowPair<'a, 'a, T>,
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
}
