use std::sync::Arc;

use powdr_ast::{
    analyzed::{AlgebraicExpression, AlgebraicReference, Challenge, Expression, PolynomialType},
    parsed::types::Type,
};
use powdr_number::{BigInt, DegreeType, FieldElement};
use powdr_pil_analyzer::evaluator::{self, Definitions, EvalError, SymbolLookup, Value};

use super::{
    data_structures::finalizable_data::FinalizableData, rows::RowIndex, Constraint, FixedData,
};

/// Runs prover functions, giving it access to all rows in a block, directly
/// filling cell values.
pub struct ProverFunctionRunner<'a, T: FieldElement> {
    /// The fixed data (containing information about all columns)
    fixed_data: &'a FixedData<'a, T>,
    /// The cell data.
    data: &'a mut FinalizableData<T>,
    /// The global row index of the first row of `self.data`.
    row_offset: RowIndex,
    /// The relative row index to evaluate the functions on.
    row_index: usize,
    /// The total number of rows.
    size: DegreeType,
    /// Arguments to the prover function (the row inde)
    fun_args: Vec<Arc<Value<'a, T>>>,
    /// True if any change was made to any cell.
    progress: bool,
}

impl<'a, T: FieldElement> ProverFunctionRunner<'a, T> {
    pub fn new(
        fixed_data: &'a FixedData<'a, T>,
        data: &'a mut FinalizableData<T>,
        row_offset: RowIndex,
        row_index: usize,
        size: DegreeType,
    ) -> Self {
        Self {
            fixed_data,
            data,
            row_offset,
            row_index,
            size,
            fun_args: vec![Arc::new(Value::Integer(BigInt::from(usize::from(
                row_offset + row_index,
            ))))],
            progress: false,
        }
    }

    /// Executes the prover function and returns true if it changed any cell values.
    pub fn process_prover_function(
        &mut self,
        fun: &'a Expression,
    ) -> Result<bool, super::EvalError<T>> {
        self.progress = false;

        let res = evaluator::evaluate(fun, self)
            .and_then(|fun| evaluator::evaluate_function_call(fun, self.fun_args.clone(), self))
            .map_err(|e| {
                // TODO we could avoid a non-recoverable error in case we get a
                // "DataNotAvailable" error (like in the QueryProcessor).
                super::EvalError::ProverQueryError(format!(
                    "Error occurred when evaluating prover function {fun} on {}:\n{e:?}",
                    self.row_offset + self.row_index
                ))
            })?;

        assert!(matches!(res.as_ref(), Value::Tuple(items) if items.is_empty()));

        Ok(self.progress)
    }
}

// TODO most of the below is copied from query_processor.rs

impl<'a, T: FieldElement> SymbolLookup<'a, T> for ProverFunctionRunner<'a, T> {
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
                &self.fixed_data.analyzed.solved_impls,
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
            PolynomialType::Committed => {
                let index = self.row_index + usize::from(poly_ref.next);
                self.data[index]
                    .value(&poly_ref.poly_id)
                    .ok_or(EvalError::DataNotAvailable)?
            }
            PolynomialType::Intermediate => todo!(),
            PolynomialType::Constant => {
                let values = self.fixed_data.fixed_cols[&poly_ref.poly_id].values(self.size);
                //TODO convert local to glabel indx.
                let row = self.row_index + if poly_ref.next { 1 } else { 0 };
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
        let row = self.row_offset.relative_to(u64::try_from(row).unwrap()) as usize;
        // TODO could return a proper ProverError if the value is already known.
        self.data[row].apply_update(poly_id, &Constraint::Assignment(*value));
        self.progress = true;

        Ok(())
    }
}
