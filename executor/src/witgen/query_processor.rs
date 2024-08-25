use std::sync::Arc;

use powdr_ast::analyzed::{AlgebraicExpression, Challenge};
use powdr_ast::analyzed::{AlgebraicReference, Expression, PolyID, PolynomialType};
use powdr_ast::parsed::types::Type;

use powdr_number::{BigInt, DegreeType, FieldElement};
use powdr_pil_analyzer::evaluator::{self, Definitions, EvalError, SymbolLookup, Value};

use super::Constraints;
use super::{rows::RowPair, Constraint, EvalResult, EvalValue, FixedData, IncompleteCause};

/// Computes value updates that result from a query.
pub struct QueryProcessor<'a, 'b, T: FieldElement, QueryCallback: Send + Sync> {
    fixed_data: &'a FixedData<'a, T>,
    query_callback: &'b mut QueryCallback,
    size: DegreeType,
}

impl<'a, 'b, T: FieldElement, QueryCallback: super::QueryCallback<T>>
    QueryProcessor<'a, 'b, T, QueryCallback>
{
    pub fn new(
        fixed_data: &'a FixedData<'a, T>,
        query_callback: &'b mut QueryCallback,
        size: DegreeType,
    ) -> Self {
        Self {
            fixed_data,
            query_callback,
            size,
        }
    }

    pub fn process_prover_function<'c>(
        &'c mut self,
        rows: &'c RowPair<'c, 'a, T>,
        fun: &'a Expression,
    ) -> EvalResult<'a, T> {
        let arguments = vec![Arc::new(Value::Integer(BigInt::from(u64::from(
            rows.current_row_index,
        ))))];

        let mut symbols = Symbols {
            fixed_data: self.fixed_data,
            rows,
            size: self.size,
            updates: Constraints::new(),
        };
        // TODO symbols need to take already provided values into account for eval()
        // during the same evaluation run

        let res = match evaluator::evaluate(fun, &mut symbols)
            .and_then(|fun| evaluator::evaluate_function_call(fun, arguments, &mut symbols))
        {
            Ok(res) => res,
            Err(e) => {
                return match e {
                    EvalError::DataNotAvailable => {
                        Ok(EvalValue::incomplete(IncompleteCause::DataNotYetAvailable))
                    }
                    // All other errors are non-recoverable
                    e => Err(super::EvalError::ProverQueryError(format!(
                        "Error occurred when evaluating prover function {fun} on {}:\n{e:?}",
                        rows.current_row_index
                    ))),
                };
            }
        };

        assert!(matches!(res.as_ref(), Value::Tuple(items) if items.is_empty()));

        // TODO use the status to mark the prover function to be run again or not.
        Ok(EvalValue::complete(symbols.updates()))
    }

    /// Process the prover query of a witness column.
    /// Panics if the column does not have a query attached.
    /// @returns None if the value for that column is already known.
    pub fn process_query(
        &mut self,
        rows: &RowPair<T>,
        poly_id: &PolyID,
    ) -> Option<EvalResult<'a, T>> {
        let column = &self.fixed_data.witness_cols[poly_id];

        if !rows.value_is_known(&column.poly) {
            Some(self.process_witness_query(column.query.unwrap(), &column.poly, rows))
        } else {
            None
        }
    }

    fn process_witness_query(
        &mut self,
        query: &'a Expression,
        poly: &'a AlgebraicReference,
        rows: &RowPair<T>,
    ) -> EvalResult<'a, T> {
        let query_str = match self.interpolate_query(query, rows) {
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
        query: &'a Expression,
        rows: &RowPair<T>,
    ) -> Result<String, EvalError> {
        let arguments = vec![Arc::new(Value::Integer(BigInt::from(u64::from(
            rows.current_row_index,
        ))))];
        let mut symbols = Symbols {
            fixed_data: self.fixed_data,
            rows,
            size: self.size,
            updates: Constraints::new(),
        };
        let fun = evaluator::evaluate(query, &mut symbols)?;
        evaluator::evaluate_function_call(fun, arguments, &mut symbols).map(|v| v.to_string())
    }
}

#[derive(Clone)]
struct Symbols<'a, 'b, T: FieldElement> {
    fixed_data: &'a FixedData<'a, T>,
    rows: &'b RowPair<'b, 'a, T>,
    size: DegreeType,
    updates: Constraints<&'a AlgebraicReference, T>,
}

impl<'a, 'b, T: FieldElement> SymbolLookup<'a, T> for Symbols<'a, 'b, T> {
    fn lookup(
        &mut self,
        name: &'a str,
        type_args: &Option<Vec<Type>>,
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
                let values = self.fixed_data.fixed_cols[&poly_ref.poly_id].values(self.size);
                let row = self.rows.current_row_index + if poly_ref.next { 1 } else { 0 };
                values[usize::from(row)]
            }
        })
        .into())
    }

    fn eval_challenge(&self, challenge: &Challenge) -> Result<Arc<Value<'a, T>>, EvalError> {
        let challenge = *self
            .fixed_data
            .challenges
            .get(&challenge.id)
            .unwrap_or_else(|| {
                panic!(
                    "Challenge {} not found! Available challenges: {:?}",
                    challenge.id,
                    self.fixed_data.challenges.keys()
                )
            });
        Ok(Value::FieldElement(challenge).into())
    }

    fn provide_value(
        &mut self,
        col: Arc<Value<'a, T>>,
        row: Arc<Value<'a, T>>,
        value: Arc<Value<'a, T>>,
    ) -> Result<(), EvalError> {
        let Value::Expression(AlgebraicExpression::Reference(AlgebraicReference {
            poly_id,
            next: false,
            ..
        })) = col.as_ref()
        else {
            return Err(EvalError::TypeError(
                "Expected direct column for first argument of std::prover::provide_value"
                    .to_string(),
            ));
        };
        let Value::Integer(row) = row.as_ref() else {
            return Err(EvalError::TypeError(
                "Expected integer for second argument of std::prover::provide_value".to_string(),
            ));
        };
        let row = i64::try_from(row).unwrap();
        let Value::FieldElement(value) = value.as_ref() else {
            return Err(EvalError::TypeError(
                "Expected field element for third argument of std::prover::provide_value"
                    .to_string(),
            ));
        };
        // TODO check that the row is the same and handle underflow.
        // TODO handle multiple updates to the same value ond alread known values.
        // TODO take updates into account in eval() calls.
        let col = &self.fixed_data.witness_cols[poly_id].poly;
        // TODO instead of this, avoid repeated calls to the prover function.
        if self.rows.get_value(col).is_none() {
            // TODO if this stay,s check that the value is at least the same if it is already known.
            self.updates.push((col, Constraint::Assignment(*value)));
        }

        Ok(())
    }
}

impl<'a, 'b, T: FieldElement> Symbols<'a, 'b, T> {
    fn updates(self) -> Constraints<&'a AlgebraicReference, T> {
        self.updates
    }
}
