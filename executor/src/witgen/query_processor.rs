use std::sync::Arc;

use powdr_ast::analyzed::{AlgebraicExpression, Challenge};
use powdr_ast::analyzed::{AlgebraicReference, Expression, PolyID, PolynomialType};
use powdr_ast::parsed::types::Type;

use powdr_number::{BigInt, DegreeType, FieldElement};
use powdr_pil_analyzer::evaluator::{self, Definitions, EvalError, SymbolLookup, Value};

use super::affine_expression::AlgebraicVariable;
use super::Constraints;
use super::{rows::RowPair, Constraint, EvalResult, EvalValue, FixedData, IncompleteCause};

/// Computes value updates that result from a query.
pub struct QueryProcessor<'a, 'b, T: FieldElement, QueryCallback: Send + Sync> {
    fixed_data: &'a FixedData<'a, T>,
    query_callback: &'b QueryCallback,
    size: DegreeType,
}

impl<'a, 'b, T: FieldElement, QueryCallback: super::QueryCallback<T>>
    QueryProcessor<'a, 'b, T, QueryCallback>
{
    pub fn new(
        fixed_data: &'a FixedData<'a, T>,
        query_callback: &'b QueryCallback,
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
            query_callback: self.query_callback,
        };
        let res = evaluator::evaluate(fun, &mut symbols)
            .and_then(|fun| evaluator::evaluate_function_call(fun, arguments, &mut symbols));

        let res = match res {
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

        Ok(EvalValue::complete(symbols.updates()))
    }

    /// Process the prover query of a witness column.
    /// Panics if the column does not have a query attached.
    /// @returns None if the value for that column is already known.
    pub fn process_query(
        &mut self,
        rows: &RowPair<'_, 'a, T>,
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
        rows: &RowPair<'_, 'a, T>,
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
                EvalValue::complete(vec![(
                    AlgebraicVariable::Column(poly),
                    Constraint::Assignment(value),
                )])
            } else {
                EvalValue::incomplete(IncompleteCause::NoQueryAnswer(
                    query_str,
                    poly.name.to_string(),
                ))
            },
        )
    }

    fn interpolate_query(
        &mut self,
        query: &'a Expression,
        rows: &RowPair<'_, 'a, T>,
    ) -> Result<String, EvalError> {
        let arguments = vec![Arc::new(Value::Integer(BigInt::from(u64::from(
            rows.current_row_index,
        ))))];
        let mut symbols = Symbols {
            fixed_data: self.fixed_data,
            rows,
            size: self.size,
            updates: Constraints::new(),
            query_callback: self.query_callback,
        };
        let fun = evaluator::evaluate(query, &mut symbols)?;
        let res =
            evaluator::evaluate_function_call(fun, arguments, &mut symbols).map(|v| v.to_string());
        res
    }
}

struct Symbols<'a, 'b, 'c, T: FieldElement, QueryCallback: Send + Sync> {
    fixed_data: &'a FixedData<'a, T>,
    rows: &'b RowPair<'b, 'a, T>,
    size: DegreeType,
    updates: Constraints<AlgebraicVariable<'a>, T>,
    query_callback: &'c QueryCallback,
}

impl<'a, T: FieldElement, QueryCallback: super::QueryCallback<T>> SymbolLookup<'a, T>
    for Symbols<'a, '_, '_, T, QueryCallback>
{
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
            PolynomialType::Committed | PolynomialType::Intermediate => {
                if let Some((_, update)) = self
                    .updates
                    .iter()
                    .find(|(p, _)| p.try_as_column().map(|p| p == poly_ref).unwrap_or_default())
                {
                    let Constraint::Assignment(value) = update else {
                        unreachable!()
                    };
                    *value
                } else {
                    self.rows
                        .get_value(AlgebraicVariable::Column(poly_ref))
                        .ok_or(EvalError::DataNotAvailable)?
                }
            }
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
            .ok_or_else(|| {
                EvalError::ProverError(format!(
                    "Challenge {} not found! Available challenges: {:?}",
                    challenge.id,
                    self.fixed_data.challenges.keys(),
                ))
            })?;

        Ok(Value::FieldElement(challenge).into())
    }

    fn provide_value(
        &mut self,
        col: Arc<Value<'a, T>>,
        row: Arc<Value<'a, T>>,
        value: Arc<Value<'a, T>>,
    ) -> Result<(), EvalError> {
        // TODO allow "next: true" in the future.
        // TODO allow assigning to publics in the future
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
            unreachable!()
        };
        let Value::FieldElement(value) = value.as_ref() else {
            unreachable!()
        };
        let row = DegreeType::try_from(row).unwrap();
        if row != DegreeType::from(self.rows.current_row_index) {
            // TODO we should be more flexible with this is the future.
            return Err(EvalError::TypeError(
                "Row index does not match current row index".to_string(),
            ));
        }
        let col = &self.fixed_data.witness_cols[poly_id].poly;
        match self.eval_reference(col) {
            Ok(v) => {
                let Value::FieldElement(v) = v.as_ref() else {
                    unreachable!()
                };
                if v != value {
                    return Err(EvalError::ProverError(format!(
                        "Tried to set {col} in row {row} to {value}, but it already has a different value {v}"
                    )));
                }
            }
            Err(EvalError::DataNotAvailable) => {
                self.updates.push((
                    AlgebraicVariable::Column(col),
                    Constraint::Assignment(*value),
                ));
            }
            Err(e) => return Err(e),
        }

        Ok(())
    }

    fn input_from_channel(
        &mut self,
        channel: u32,
        index: usize,
    ) -> Result<Arc<Value<'a, T>>, EvalError> {
        if let Some(v) = (self.query_callback)(&format!("Input({channel},{index})"))
            .map_err(EvalError::ProverError)?
        {
            Ok(Value::FieldElement(v).into())
        } else {
            Err(EvalError::DataNotAvailable)
        }
    }

    fn output_to_channel(&mut self, fd: u32, elem: T) -> Result<(), EvalError> {
        if ((self.query_callback)(&format!("Output({fd},{elem})"))
            .map_err(EvalError::ProverError)?)
        .is_some()
        {
            Ok(())
        } else {
            Err(EvalError::DataNotAvailable)
        }
    }
}

impl<'a, T: FieldElement, QueryCallback: Send + Sync> Symbols<'a, '_, '_, T, QueryCallback> {
    fn updates(self) -> Constraints<AlgebraicVariable<'a>, T> {
        self.updates
    }
}
