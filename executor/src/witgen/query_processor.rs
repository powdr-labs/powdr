use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use powdr_ast::analyzed::{AlgebraicExpression, Challenge, PolynomialReference, Reference};
use powdr_ast::analyzed::{AlgebraicReference, Expression, PolyID, PolynomialType};
use powdr_ast::parsed::types::Type;
use powdr_ast::parsed::{FunctionCall, LambdaExpression};
use powdr_number::{BigInt, FieldElement};
use powdr_pil_analyzer::evaluator::{self, Definitions, EvalError, SymbolLookup, Value};

use super::data_structures::column_map::Fixed;
use super::{rows::RowPair, Constraint, EvalResult, EvalValue, FixedData, IncompleteCause};

type SymbolCacheKey = (String, Option<Vec<Type>>);

/// Computes value updates that result from a query.
pub struct QueryProcessor<'a, 'b, 'c, T: FieldElement, QueryCallback: Send + Sync> {
    fixed_data: &'a FixedData<'a, T>,
    query_callback: &'b mut QueryCallback,
    // TODO the cache should really be somewhere else, possibly inside Definitions
    cache: &'c Mutex<BTreeMap<(String, Option<Vec<Type>>), Arc<Value<'a, T>>>>,
}

impl<'a, 'b, 'c, T: FieldElement, QueryCallback: super::QueryCallback<T>>
    QueryProcessor<'a, 'b, 'c, T, QueryCallback>
{
    pub fn new(
        fixed_data: &'a FixedData<'a, T>,
        query_callback: &'b mut QueryCallback,
        cache: &'c Mutex<BTreeMap<(String, Option<Vec<Type>>), Arc<Value<'a, T>>>>,
    ) -> Self {
        Self {
            fixed_data,
            query_callback,
            cache,
        }
    }

    pub fn process_query<'d>(
        &mut self,
        rows: &'d RowPair<T>,
        poly_id: &PolyID,
    ) -> EvalResult<'a, T> {
        // let start = std::time::Instant::now();
        let column = &self.fixed_data.witness_cols[poly_id];

        if let Some(query) = column.query.as_ref() {
            if rows.get_value(&column.poly).is_none() {
                let r = self.process_witness_query(query, &column.poly, rows);
                // let end = start.elapsed();
                // println!("Query processing time: {} us", end.as_micros());
                // if let Ok(ref r) = r {
                //     println!("Result size: {}", r.constraints.len());
                // }
                return r;
            }
        }

        // Either no query or the value is already known.
        Ok(EvalValue::complete(vec![]))
    }

    fn process_witness_query<'d>(
        &mut self,
        query: &'a Expression,
        poly: &'a AlgebraicReference,
        rows: &'d RowPair<T>,
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

    fn interpolate_query<'d>(
        &self,
        query: &'a Expression,
        rows: &'d RowPair<T>,
    ) -> Result<(String, Vec<(AlgebraicReference, T)>), EvalError> {
        match query {
            Expression::LambdaExpression(_, LambdaExpression { body, .. }) => match body.as_ref() {
                Expression::FunctionCall(_, FunctionCall { function, .. }) => {
                    match function.as_ref() {
                        Expression::Reference(
                            _,
                            Reference::Poly(PolynomialReference { name, .. }),
                        ) if name == "main.all_prover_hints" => {
                            return shortcut_exec(self.fixed_data, rows);
                        }
                        _ => {}
                    }
                }
                _ => {}
            },
            _ => {}
        }
        let arguments = vec![Arc::new(Value::Integer(BigInt::from(u64::from(
            rows.current_row_index,
        ))))];
        let mut symbols = Symbols {
            fixed_data: self.fixed_data,
            rows,
            assignments: vec![],
            cache: &self.cache,
        };
        let fun = evaluator::evaluate(query, &mut symbols)?;
        let result = evaluator::evaluate_function_call(fun, arguments, &mut symbols)?;

        Ok((result.to_string(), symbols.assignments))
    }
}

struct Symbols<'a, 'c, 'd, T: FieldElement> {
    fixed_data: &'a FixedData<'a, T>,
    rows: &'d RowPair<'d, 'd, T>,
    assignments: Vec<(AlgebraicReference, T)>,
    cache: &'c Mutex<BTreeMap<(String, Option<Vec<Type>>), Arc<Value<'a, T>>>>,
}

impl<'a, 'c, 'd, T: FieldElement> SymbolLookup<'a, T> for Symbols<'a, 'c, 'd, T> {
    fn lookup<'b>(
        &mut self,
        name: &'a str,
        type_args: Option<Vec<Type>>,
    ) -> Result<Arc<Value<'a, T>>, EvalError> {
        let cache_key = (name.to_string(), type_args.clone());
        if let Some(v) = self.cache.lock().unwrap().get(&cache_key).cloned() {
            return Ok(v.clone());
        }

        println!("Cache miss: {name}");

        let value = match self.fixed_data.analyzed.intermediate_columns.get(name) {
            // Intermediate polynomials (which includes challenges) are not inlined in hints,
            // so we need to look them up here.
            Some((symbol, expressions)) => {
                if let Some(type_args) = &type_args {
                    assert!(type_args.is_empty());
                }
                if symbol.is_array() {
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
                .into()
            }
            None => Definitions::lookup_with_symbols(
                &self.fixed_data.analyzed.definitions,
                name,
                type_args,
                self,
            )?,
        };
        self.cache.lock().unwrap().insert(cache_key, value.clone());
        Ok(value)
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

const p_X_const_lookup: [u64; 77] = [
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    1,
    0,
    0,
    0,
    0,
    10000,
    1,
    0,
    0,
    18446744069414584259,
    18446744069414584261,
    18446744069414584278,
    18446744069414584276,
    18446744069414584277,
    18446744069414584275,
    18446744069414584230,
    18446744069414584228,
    0,
    0,
    0,
    1,
    0,
    0,
    1,
    0,
    1,
    0,
    18446744069414584230,
    18446744069414584228,
    0,
    1,
    0,
    18446744069414584320,
    0,
    0,
    0,
    0,
    0,
    18446744069414584320,
    18446744069414584320,
    0,
    1,
    0,
    18446744069414584320,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    10000,
    20000,
    30000,
    0,
    0,
    0,
    1,
    0,
    0,
];
const p_X_read_free_lookup: [u64; 77] = [
    0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_Y_const_lookup: [u64; 77] = [
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    1,
    0,
    0,
    18446744069414584320,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
];
const p_Y_read_free_lookup: [u64; 77] = [
    0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0,
    0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0,
    1, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0,
];
const p_Z_const_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_Z_read_free_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_instr__jump_to_operation_lookup: [u64; 77] = [
    0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_instr__loop_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
];
const p_instr__reset_lookup: [u64; 77] = [
    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_instr_branch_if_zero_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 0, 0, 0,
    0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
];
const p_instr_branch_if_zero_param_l_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 0, 16, 0, 0, 0, 0, 49, 51, 53, 56, 59, 62, 26, 45, 0, 0, 31,
    0, 0, 0, 0, 0, 0, 0, 38, 40, 0, 0, 0, 0, 43, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0,
];
const p_instr_fail_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_instr_jump_lookup: [u64; 77] = [
    0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
    0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0,
    1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0,
];
const p_instr_jump_dyn_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_instr_jump_param_l_lookup: [u64; 77] = [
    0, 0, 66, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 65,
    0, 0, 0, 0, 0, 0, 33, 0, 33, 0, 0, 33, 0, 65, 0, 0, 0, 65, 0, 65, 0, 65, 0, 0, 65, 0, 0, 65, 0,
    0, 65, 0, 0, 65, 0, 4, 0, 0, 0, 0, 0, 0, 17, 0, 71, 0,
];
const p_instr_mload_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
    0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0,
    0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
];
const p_instr_mstore_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_instr_return_lookup: [u64; 77] = [
    0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_read_X_A_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
    0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_read_X_CNT_lookup: [u64; 77] = [
    0,
    0,
    0,
    0,
    0,
    0,
    18446744069414584320,
    1,
    1,
    0,
    0,
    0,
    18446744069414584320,
    1,
    1,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    1,
    0,
    1,
    1,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
];
const p_read_X_b_pc_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0,
];
const p_read_X_dp_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 0, 1, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_read_X_in_ptr_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_read_X_loop_sp_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_read_X_op_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
];
const p_read_X_pc_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_read_X_ret_addr_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_read_X_tmp1_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_read_Y_A_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_read_Y_CNT_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_read_Y_b_pc_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_read_Y_dp_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_read_Y_in_ptr_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_read_Y_loop_sp_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_read_Y_op_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_read_Y_pc_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_read_Y_ret_addr_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_read_Y_tmp1_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_read_Z_A_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_read_Z_CNT_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_read_Z_b_pc_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_read_Z_dp_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_read_Z_in_ptr_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_read_Z_loop_sp_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_read_Z_op_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_read_Z_pc_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_read_Z_ret_addr_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_read_Z_tmp1_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_reg_write_X_A_lookup: [u64; 77] = [
    0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_reg_write_X_CNT_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
    0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_reg_write_X_b_pc_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0,
];
const p_reg_write_X_dp_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
];
const p_reg_write_X_in_ptr_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_reg_write_X_loop_sp_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
];
const p_reg_write_X_op_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_reg_write_X_ret_addr_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_reg_write_X_tmp1_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_reg_write_Y_A_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0,
    1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
];
const p_reg_write_Y_CNT_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_reg_write_Y_b_pc_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_reg_write_Y_dp_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_reg_write_Y_in_ptr_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_reg_write_Y_loop_sp_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_reg_write_Y_op_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
];
const p_reg_write_Y_ret_addr_lookup: [u64; 77] = [
    0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
];
const p_reg_write_Y_tmp1_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_reg_write_Z_A_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_reg_write_Z_CNT_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_reg_write_Z_b_pc_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_reg_write_Z_dp_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_reg_write_Z_in_ptr_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_reg_write_Z_loop_sp_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_reg_write_Z_op_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_reg_write_Z_ret_addr_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];
const p_reg_write_Z_tmp1_lookup: [u64; 77] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];

fn alg_ref<T: FieldElement>(name: &str, fixed_data: &FixedData<'_, T>) -> AlgebraicReference {
    AlgebraicReference {
        name: name.to_string(),
        poly_id: (&fixed_data.analyzed.definitions[name].0).into(),
        next: false,
    }
}

fn shortcut_exec<T: FieldElement>(
    fixed_data: &FixedData<T>,
    rows: &RowPair<T>,
) -> Result<(String, Vec<(AlgebraicReference, T)>), EvalError> {
    //let pc_val = std::convert::int::<fe>(std::prover::eval(main.pc));
    let pc_val = rows
        .get_value(&alg_ref("main.pc", fixed_data))
        .ok_or(EvalError::DataNotAvailable)?
        .to_arbitrary_integer();
    let pc_val = usize::try_from(pc_val).unwrap();
    let mut result = vec![];
    // let s = std::prover::set;
    // let _ = s(
    //     main.reg_write_X_b_pc,
    //     main.p_reg_write_X_b_pc_lookup[pc_val],
    // );
    result.push((
        alg_ref("main.reg_write_X_b_pc", fixed_data),
        T::from(p_reg_write_X_b_pc_lookup[pc_val]),
    ));

    // let _ = s(
    //     main.reg_write_Y_b_pc,
    //     main.p_reg_write_Y_b_pc_lookup[pc_val],
    // );
    result.push((
        alg_ref("main.reg_write_Y_b_pc", fixed_data),
        T::from(p_reg_write_Y_b_pc_lookup[pc_val]),
    ));
    // let _ = s(
    //     main.reg_write_Z_b_pc,
    //     main.p_reg_write_Z_b_pc_lookup[pc_val],
    // );
    result.push((
        alg_ref("main.reg_write_Z_b_pc", fixed_data),
        T::from(p_reg_write_Z_b_pc_lookup[pc_val]),
    ));
    // let _ = s(main.reg_write_X_op, main.p_reg_write_X_op_lookup[pc_val]);
    result.push((
        alg_ref("main.reg_write_X_op", fixed_data),
        T::from(p_reg_write_X_op_lookup[pc_val]),
    ));
    // let _ = s(main.reg_write_Y_op, main.p_reg_write_Y_op_lookup[pc_val]);
    result.push((
        alg_ref("main.reg_write_Y_op", fixed_data),
        T::from(p_reg_write_Y_op_lookup[pc_val]),
    ));
    // let _ = s(main.reg_write_Z_op, main.p_reg_write_Z_op_lookup[pc_val]);
    result.push((
        alg_ref("main.reg_write_Z_op", fixed_data),
        T::from(p_reg_write_Z_op_lookup[pc_val]),
    ));
    // let _ = s(main.reg_write_X_dp, main.p_reg_write_X_dp_lookup[pc_val]);
    result.push((
        alg_ref("main.reg_write_X_dp", fixed_data),
        T::from(p_reg_write_X_dp_lookup[pc_val]),
    ));
    // let _ = s(main.reg_write_Y_dp, main.p_reg_write_Y_dp_lookup[pc_val]);
    result.push((
        alg_ref("main.reg_write_Y_dp", fixed_data),
        T::from(p_reg_write_Y_dp_lookup[pc_val]),
    ));
    // let _ = s(main.reg_write_Z_dp, main.p_reg_write_Z_dp_lookup[pc_val]);
    result.push((
        alg_ref("main.reg_write_Z_dp", fixed_data),
        T::from(p_reg_write_Z_dp_lookup[pc_val]),
    ));
    // let _ = s(
    //     main.reg_write_X_in_ptr,
    //     main.p_reg_write_X_in_ptr_lookup[pc_val],
    // );
    result.push((
        alg_ref("main.reg_write_X_in_ptr", fixed_data),
        T::from(p_reg_write_X_in_ptr_lookup[pc_val]),
    ));
    // let _ = s(
    //     main.reg_write_Y_in_ptr,
    //     main.p_reg_write_Y_in_ptr_lookup[pc_val],
    // );
    result.push((
        alg_ref("main.reg_write_Y_in_ptr", fixed_data),
        T::from(p_reg_write_Y_in_ptr_lookup[pc_val]),
    ));
    // let _ = s(
    //     main.reg_write_Z_in_ptr,
    //     main.p_reg_write_Z_in_ptr_lookup[pc_val],
    // );
    result.push((
        alg_ref("main.reg_write_Z_in_ptr", fixed_data),
        T::from(p_reg_write_Z_in_ptr_lookup[pc_val]),
    ));
    // let _ = s(
    //     main.reg_write_X_loop_sp,
    //     main.p_reg_write_X_loop_sp_lookup[pc_val],
    // );
    result.push((
        alg_ref("main.reg_write_X_loop_sp", fixed_data),
        T::from(p_reg_write_X_loop_sp_lookup[pc_val]),
    ));
    // let _ = s(
    //     main.reg_write_Y_loop_sp,
    //     main.p_reg_write_Y_loop_sp_lookup[pc_val],
    // );
    result.push((
        alg_ref("main.reg_write_Y_loop_sp", fixed_data),
        T::from(p_reg_write_Y_loop_sp_lookup[pc_val]),
    ));
    // let _ = s(
    //     main.reg_write_Z_loop_sp,
    //     main.p_reg_write_Z_loop_sp_lookup[pc_val],
    // );
    result.push((
        alg_ref("main.reg_write_Z_loop_sp", fixed_data),
        T::from(p_reg_write_Z_loop_sp_lookup[pc_val]),
    ));
    // let _ = s(
    //     main.reg_write_X_ret_addr,
    //     main.p_reg_write_X_ret_addr_lookup[pc_val],
    // );
    result.push((
        alg_ref("main.reg_write_X_ret_addr", fixed_data),
        T::from(p_reg_write_X_ret_addr_lookup[pc_val]),
    ));
    // let _ = s(
    //     main.reg_write_Y_ret_addr,
    //     main.p_reg_write_Y_ret_addr_lookup[pc_val],
    // );
    result.push((
        alg_ref("main.reg_write_Y_ret_addr", fixed_data),
        T::from(p_reg_write_Y_ret_addr_lookup[pc_val]),
    ));
    // let _ = s(
    //     main.reg_write_Z_ret_addr,
    //     main.p_reg_write_Z_ret_addr_lookup[pc_val],
    // );
    result.push((
        alg_ref("main.reg_write_Z_ret_addr", fixed_data),
        T::from(p_reg_write_Z_ret_addr_lookup[pc_val]),
    ));
    // let _ = s(main.reg_write_X_A, main.p_reg_write_X_A_lookup[pc_val]);
    result.push((
        alg_ref("main.reg_write_X_A", fixed_data),
        T::from(p_reg_write_X_A_lookup[pc_val]),
    ));
    // let _ = s(main.reg_write_Y_A, main.p_reg_write_Y_A_lookup[pc_val]);
    result.push((
        alg_ref("main.reg_write_Y_A", fixed_data),
        T::from(p_reg_write_Y_A_lookup[pc_val]),
    ));
    // let _ = s(main.reg_write_Z_A, main.p_reg_write_Z_A_lookup[pc_val]);
    result.push((
        alg_ref("main.reg_write_Z_A", fixed_data),
        T::from(p_reg_write_Z_A_lookup[pc_val]),
    ));
    // let _ = s(main.reg_write_X_CNT, main.p_reg_write_X_CNT_lookup[pc_val]);
    result.push((
        alg_ref("main.reg_write_X_CNT", fixed_data),
        T::from(p_reg_write_X_CNT_lookup[pc_val]),
    ));
    // let _ = s(main.reg_write_Y_CNT, main.p_reg_write_Y_CNT_lookup[pc_val]);
    result.push((
        alg_ref("main.reg_write_Y_CNT", fixed_data),
        T::from(p_reg_write_Y_CNT_lookup[pc_val]),
    ));
    // let _ = s(main.reg_write_Z_CNT, main.p_reg_write_Z_CNT_lookup[pc_val]);
    result.push((
        alg_ref("main.reg_write_Z_CNT", fixed_data),
        T::from(p_reg_write_Z_CNT_lookup[pc_val]),
    ));
    // let _ = s(
    //     main.reg_write_X_tmp1,
    //     main.p_reg_write_X_tmp1_lookup[pc_val],
    // );
    result.push((
        alg_ref("main.reg_write_X_tmp1", fixed_data),
        T::from(p_reg_write_X_tmp1_lookup[pc_val]),
    ));
    // let _ = s(
    //     main.reg_write_Y_tmp1,
    //     main.p_reg_write_Y_tmp1_lookup[pc_val],
    // );
    result.push((
        alg_ref("main.reg_write_Y_tmp1", fixed_data),
        T::from(p_reg_write_Y_tmp1_lookup[pc_val]),
    ));
    // let _ = s(
    //     main.reg_write_Z_tmp1,
    //     main.p_reg_write_Z_tmp1_lookup[pc_val],
    // );
    result.push((
        alg_ref("main.reg_write_Z_tmp1", fixed_data),
        T::from(p_reg_write_Z_tmp1_lookup[pc_val]),
    ));
    // let _ = s(main.instr_jump, main.p_instr_jump_lookup[pc_val]);
    result.push((
        alg_ref("main.instr_jump", fixed_data),
        T::from(p_instr_jump_lookup[pc_val]),
    ));
    // let _ = s(
    //     main.instr_jump_param_l,
    //     main.p_instr_jump_param_l_lookup[pc_val],
    // );
    result.push((
        alg_ref("main.instr_jump_param_l", fixed_data),
        T::from(p_instr_jump_param_l_lookup[pc_val]),
    ));
    // let _ = s(main.instr_jump_dyn, main.p_instr_jump_dyn_lookup[pc_val]);
    result.push((
        alg_ref("main.instr_jump_dyn", fixed_data),
        T::from(p_instr_jump_dyn_lookup[pc_val]),
    ));
    // let _ = s(
    //     main.instr_branch_if_zero,
    //     main.p_instr_branch_if_zero_lookup[pc_val],
    // );
    result.push((
        alg_ref("main.instr_branch_if_zero", fixed_data),
        T::from(p_instr_branch_if_zero_lookup[pc_val]),
    ));
    // let _ = s(
    //     main.instr_branch_if_zero_param_l,
    //     main.p_instr_branch_if_zero_param_l_lookup[pc_val],
    // );
    result.push((
        alg_ref("main.instr_branch_if_zero_param_l", fixed_data),
        T::from(p_instr_branch_if_zero_param_l_lookup[pc_val]),
    ));
    // let _ = s(main.instr_fail, main.p_instr_fail_lookup[pc_val]);
    result.push((
        alg_ref("main.instr_fail", fixed_data),
        T::from(p_instr_fail_lookup[pc_val]),
    ));
    // let _ = s(main.instr_mload, main.p_instr_mload_lookup[pc_val]);
    result.push((
        alg_ref("main.instr_mload", fixed_data),
        T::from(p_instr_mload_lookup[pc_val]),
    ));
    // let _ = s(main.instr_mstore, main.p_instr_mstore_lookup[pc_val]);
    result.push((
        alg_ref("main.instr_mstore", fixed_data),
        T::from(p_instr_mstore_lookup[pc_val]),
    ));
    // let _ = s(
    //     main.instr__jump_to_operation,
    //     main.p_instr__jump_to_operation_lookup[pc_val],
    // );
    result.push((
        alg_ref("main.instr__jump_to_operation", fixed_data),
        T::from(p_instr__jump_to_operation_lookup[pc_val]),
    ));
    // let _ = s(main.instr__reset, main.p_instr__reset_lookup[pc_val]);
    result.push((
        alg_ref("main.instr__reset", fixed_data),
        T::from(p_instr__reset_lookup[pc_val]),
    ));
    // let _ = s(main.instr__loop, main.p_instr__loop_lookup[pc_val]);
    result.push((
        alg_ref("main.instr__loop", fixed_data),
        T::from(p_instr__loop_lookup[pc_val]),
    ));
    // let _ = s(main.instr_return, main.p_instr_return_lookup[pc_val]);
    result.push((
        alg_ref("main.instr_return", fixed_data),
        T::from(p_instr_return_lookup[pc_val]),
    ));
    // let _ = s(main.X_const, main.p_X_const_lookup[pc_val]);
    result.push((
        alg_ref("main.X_const", fixed_data),
        T::from(p_X_const_lookup[pc_val]),
    ));
    // let _ = s(main.X_read_free, main.p_X_read_free_lookup[pc_val]);
    result.push((
        alg_ref("main.X_read_free", fixed_data),
        T::from(p_X_read_free_lookup[pc_val]),
    ));
    // let _ = s(main.read_X_A, main.p_read_X_A_lookup[pc_val]);
    result.push((
        alg_ref("main.read_X_A", fixed_data),
        T::from(p_read_X_A_lookup[pc_val]),
    ));
    // let _ = s(main.read_X_CNT, main.p_read_X_CNT_lookup[pc_val]);
    result.push((
        alg_ref("main.read_X_CNT", fixed_data),
        T::from(p_read_X_CNT_lookup[pc_val]),
    ));
    // let _ = s(main.read_X_b_pc, main.p_read_X_b_pc_lookup[pc_val]);
    result.push((
        alg_ref("main.read_X_b_pc", fixed_data),
        T::from(p_read_X_b_pc_lookup[pc_val]),
    ));
    // let _ = s(main.read_X_dp, main.p_read_X_dp_lookup[pc_val]);
    result.push((
        alg_ref("main.read_X_dp", fixed_data),
        T::from(p_read_X_dp_lookup[pc_val]),
    ));
    // let _ = s(main.read_X_in_ptr, main.p_read_X_in_ptr_lookup[pc_val]);
    result.push((
        alg_ref("main.read_X_in_ptr", fixed_data),
        T::from(p_read_X_in_ptr_lookup[pc_val]),
    ));
    // let _ = s(main.read_X_loop_sp, main.p_read_X_loop_sp_lookup[pc_val]);
    result.push((
        alg_ref("main.read_X_loop_sp", fixed_data),
        T::from(p_read_X_loop_sp_lookup[pc_val]),
    ));
    // let _ = s(main.read_X_op, main.p_read_X_op_lookup[pc_val]);
    result.push((
        alg_ref("main.read_X_op", fixed_data),
        T::from(p_read_X_op_lookup[pc_val]),
    ));
    // let _ = s(main.read_X_ret_addr, main.p_read_X_ret_addr_lookup[pc_val]);
    result.push((
        alg_ref("main.read_X_ret_addr", fixed_data),
        T::from(p_read_X_ret_addr_lookup[pc_val]),
    ));
    // let _ = s(main.read_X_tmp1, main.p_read_X_tmp1_lookup[pc_val]);
    result.push((
        alg_ref("main.read_X_tmp1", fixed_data),
        T::from(p_read_X_tmp1_lookup[pc_val]),
    ));
    // let _ = s(main.read_X_pc, main.p_read_X_pc_lookup[pc_val]);
    result.push((
        alg_ref("main.read_X_pc", fixed_data),
        T::from(p_read_X_pc_lookup[pc_val]),
    ));
    // let _ = s(main.Y_const, main.p_Y_const_lookup[pc_val]);
    result.push((
        alg_ref("main.Y_const", fixed_data),
        T::from(p_Y_const_lookup[pc_val]),
    ));
    // let _ = s(main.Y_read_free, main.p_Y_read_free_lookup[pc_val]);
    result.push((
        alg_ref("main.Y_read_free", fixed_data),
        T::from(p_Y_read_free_lookup[pc_val]),
    ));
    // let _ = s(main.read_Y_A, main.p_read_Y_A_lookup[pc_val]);
    result.push((
        alg_ref("main.read_Y_A", fixed_data),
        T::from(p_read_Y_A_lookup[pc_val]),
    ));
    // let _ = s(main.read_Y_CNT, main.p_read_Y_CNT_lookup[pc_val]);
    result.push((
        alg_ref("main.read_Y_CNT", fixed_data),
        T::from(p_read_Y_CNT_lookup[pc_val]),
    ));
    // let _ = s(main.read_Y_b_pc, main.p_read_Y_b_pc_lookup[pc_val]);
    result.push((
        alg_ref("main.read_Y_b_pc", fixed_data),
        T::from(p_read_Y_b_pc_lookup[pc_val]),
    ));
    // let _ = s(main.read_Y_dp, main.p_read_Y_dp_lookup[pc_val]);
    result.push((
        alg_ref("main.read_Y_dp", fixed_data),
        T::from(p_read_Y_dp_lookup[pc_val]),
    ));
    // let _ = s(main.read_Y_in_ptr, main.p_read_Y_in_ptr_lookup[pc_val]);
    result.push((
        alg_ref("main.read_Y_in_ptr", fixed_data),
        T::from(p_read_Y_in_ptr_lookup[pc_val]),
    ));
    // let _ = s(main.read_Y_loop_sp, main.p_read_Y_loop_sp_lookup[pc_val]);
    result.push((
        alg_ref("main.read_Y_loop_sp", fixed_data),
        T::from(p_read_Y_loop_sp_lookup[pc_val]),
    ));
    // let _ = s(main.read_Y_op, main.p_read_Y_op_lookup[pc_val]);
    result.push((
        alg_ref("main.read_Y_op", fixed_data),
        T::from(p_read_Y_op_lookup[pc_val]),
    ));
    // let _ = s(main.read_Y_ret_addr, main.p_read_Y_ret_addr_lookup[pc_val]);
    result.push((
        alg_ref("main.read_Y_ret_addr", fixed_data),
        T::from(p_read_Y_ret_addr_lookup[pc_val]),
    ));
    // let _ = s(main.read_Y_tmp1, main.p_read_Y_tmp1_lookup[pc_val]);
    result.push((
        alg_ref("main.read_Y_tmp1", fixed_data),
        T::from(p_read_Y_tmp1_lookup[pc_val]),
    ));
    // let _ = s(main.read_Y_pc, main.p_read_Y_pc_lookup[pc_val]);
    result.push((
        alg_ref("main.read_Y_pc", fixed_data),
        T::from(p_read_Y_pc_lookup[pc_val]),
    ));
    // let _ = s(main.Z_const, main.p_Z_const_lookup[pc_val]);
    result.push((
        alg_ref("main.Z_const", fixed_data),
        T::from(p_Z_const_lookup[pc_val]),
    ));
    // let _ = s(main.Z_read_free, main.p_Z_read_free_lookup[pc_val]);
    result.push((
        alg_ref("main.Z_read_free", fixed_data),
        T::from(p_Z_read_free_lookup[pc_val]),
    ));
    // let _ = s(main.read_Z_A, main.p_read_Z_A_lookup[pc_val]);
    result.push((
        alg_ref("main.read_Z_A", fixed_data),
        T::from(p_read_Z_A_lookup[pc_val]),
    ));
    // let _ = s(main.read_Z_CNT, main.p_read_Z_CNT_lookup[pc_val]);
    result.push((
        alg_ref("main.read_Z_CNT", fixed_data),
        T::from(p_read_Z_CNT_lookup[pc_val]),
    ));
    // let _ = s(main.read_Z_b_pc, main.p_read_Z_b_pc_lookup[pc_val]);
    result.push((
        alg_ref("main.read_Z_b_pc", fixed_data),
        T::from(p_read_Z_b_pc_lookup[pc_val]),
    ));
    // let _ = s(main.read_Z_dp, main.p_read_Z_dp_lookup[pc_val]);
    result.push((
        alg_ref("main.read_Z_dp", fixed_data),
        T::from(p_read_Z_dp_lookup[pc_val]),
    ));
    // let _ = s(main.read_Z_in_ptr, main.p_read_Z_in_ptr_lookup[pc_val]);
    result.push((
        alg_ref("main.read_Z_in_ptr", fixed_data),
        T::from(p_read_Z_in_ptr_lookup[pc_val]),
    ));
    // let _ = s(main.read_Z_loop_sp, main.p_read_Z_loop_sp_lookup[pc_val]);
    result.push((
        alg_ref("main.read_Z_loop_sp", fixed_data),
        T::from(p_read_Z_loop_sp_lookup[pc_val]),
    ));
    // let _ = s(main.read_Z_op, main.p_read_Z_op_lookup[pc_val]);
    result.push((
        alg_ref("main.read_Z_op", fixed_data),
        T::from(p_read_Z_op_lookup[pc_val]),
    ));
    // let _ = s(main.read_Z_ret_addr, main.p_read_Z_ret_addr_lookup[pc_val]);
    result.push((
        alg_ref("main.read_Z_ret_addr", fixed_data),
        T::from(p_read_Z_ret_addr_lookup[pc_val]),
    ));
    // let _ = s(main.read_Z_tmp1, main.p_read_Z_tmp1_lookup[pc_val]);
    result.push((
        alg_ref("main.read_Z_tmp1", fixed_data),
        T::from(p_read_Z_tmp1_lookup[pc_val]),
    ));
    // let _ = s(main.read_Z_pc, main.p_read_Z_pc_lookup[pc_val]);
    result.push((
        alg_ref("main.read_Z_pc", fixed_data),
        T::from(p_read_Z_pc_lookup[pc_val]),
    ));

    Ok(("None".to_string(), result))
}
