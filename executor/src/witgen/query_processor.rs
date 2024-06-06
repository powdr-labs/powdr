use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use lazy_static::lazy_static;
use powdr_ast::analyzed::{AlgebraicExpression, Challenge, PolynomialReference, Reference};
use powdr_ast::analyzed::{AlgebraicReference, Expression, PolyID, PolynomialType};
use powdr_ast::parsed::types::Type;
use powdr_ast::parsed::{FunctionCall, LambdaExpression};
use powdr_number::{BigInt, FieldElement, GoldilocksField};
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
    //shortcut_exec: &'c ShortcutExec<'a, T>,
}

impl<'a, 'b, 'c, T: FieldElement, QueryCallback: super::QueryCallback<T>>
    QueryProcessor<'a, 'b, 'c, T, QueryCallback>
{
    pub fn new(
        fixed_data: &'a FixedData<'a, T>,
        query_callback: &'b mut QueryCallback,
        cache: &'c Mutex<BTreeMap<(String, Option<Vec<Type>>), Arc<Value<'a, T>>>>,
        //shortcut_exec: &'c ShortcutExec<'a, T>,
    ) -> Self {
        Self {
            fixed_data,
            query_callback,
            cache,
            //shortcut_exec,
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
                        poly, //&self.fixed_data.witness_cols[&poly.poly_id].poly,
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
    ) -> Result<(String, Vec<(&'a AlgebraicReference, T)>), EvalError> {
        let use_rust = false;
        // if use_rust {
        //     match query {
        //         Expression::LambdaExpression(_, LambdaExpression { body, .. }) => {
        //             match body.as_ref() {
        //                 Expression::FunctionCall(_, FunctionCall { function, .. }) => {
        //                     match function.as_ref() {
        //                         Expression::Reference(
        //                             _,
        //                             Reference::Poly(PolynomialReference { name, .. }),
        //                         ) if name == "main.all_prover_hints" => {
        //                             return self.shortcut_exec.shortcut_exec(self.fixed_data, rows);
        //                         }
        //                         _ => {}
        //                     }
        //                 }
        //                 _ => {}
        //             }
        //         }
        //         _ => {}
        //     }
        // }
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

        let assignments = symbols
            .assignments
            .into_iter()
            .map(|(poly_id, v)| {
                let r = &self.fixed_data.witness_cols[&poly_id].poly;
                (r, v)
            })
            .collect::<Vec<_>>();

        // TODO this ignores all assignments done trough set().
        Ok((result.to_string(), assignments))
    }
}

struct Symbols<'a, 'c, 'd, T: FieldElement> {
    fixed_data: &'a FixedData<'a, T>,
    rows: &'d RowPair<'d, 'd, T>,
    assignments: Vec<(PolyID, T)>,
    cache: &'c Mutex<BTreeMap<(String, Option<Vec<Type>>), Arc<Value<'a, T>>>>,
}

impl<'a, 'c, 'd, T: FieldElement> SymbolLookup<'a, T> for Symbols<'a, 'c, 'd, T> {
    fn lookup<'b>(
        &mut self,
        name: &'a str,
        type_args: Option<Vec<Type>>,
    ) -> Result<Arc<Value<'a, T>>, EvalError> {
        // TODO avoid the to_string here?
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
        // TODO avoid cloning the string? Maybe just store the poly id?
        assert!(!reference.next);
        self.assignments.push((reference.poly_id, value));
        Ok(())
    }
}

pub struct ShortcutExec<'a, T: FieldElement> {
    p_X_const_lookup: Vec<T>,
    p_X_read_free_lookup: Vec<T>,
    p_Y_const_lookup: Vec<T>,
    p_Y_read_free_lookup: Vec<T>,
    p_Z_const_lookup: Vec<T>,
    p_Z_read_free_lookup: Vec<T>,
    p_instr__jump_to_operation_lookup: Vec<T>,
    p_instr__loop_lookup: Vec<T>,
    p_instr__reset_lookup: Vec<T>,
    p_instr_branch_if_zero_lookup: Vec<T>,
    p_instr_branch_if_zero_param_l_lookup: Vec<T>,
    p_instr_fail_lookup: Vec<T>,
    p_instr_jump_lookup: Vec<T>,
    p_instr_jump_dyn_lookup: Vec<T>,
    p_instr_jump_param_l_lookup: Vec<T>,
    p_instr_mload_lookup: Vec<T>,
    p_instr_mstore_lookup: Vec<T>,
    p_instr_return_lookup: Vec<T>,
    p_read_X_A_lookup: Vec<T>,
    p_read_X_CNT_lookup: Vec<T>,
    p_read_X_b_pc_lookup: Vec<T>,
    p_read_X_dp_lookup: Vec<T>,
    p_read_X_in_ptr_lookup: Vec<T>,
    p_read_X_loop_sp_lookup: Vec<T>,
    p_read_X_op_lookup: Vec<T>,
    p_read_X_pc_lookup: Vec<T>,
    p_read_X_ret_addr_lookup: Vec<T>,
    p_read_X_tmp1_lookup: Vec<T>,
    p_read_Y_A_lookup: Vec<T>,
    p_read_Y_CNT_lookup: Vec<T>,
    p_read_Y_b_pc_lookup: Vec<T>,
    p_read_Y_dp_lookup: Vec<T>,
    p_read_Y_in_ptr_lookup: Vec<T>,
    p_read_Y_loop_sp_lookup: Vec<T>,
    p_read_Y_op_lookup: Vec<T>,
    p_read_Y_pc_lookup: Vec<T>,
    p_read_Y_ret_addr_lookup: Vec<T>,
    p_read_Y_tmp1_lookup: Vec<T>,
    p_read_Z_A_lookup: Vec<T>,
    p_read_Z_CNT_lookup: Vec<T>,
    p_read_Z_b_pc_lookup: Vec<T>,
    p_read_Z_dp_lookup: Vec<T>,
    p_read_Z_in_ptr_lookup: Vec<T>,
    p_read_Z_loop_sp_lookup: Vec<T>,
    p_read_Z_op_lookup: Vec<T>,
    p_read_Z_pc_lookup: Vec<T>,
    p_read_Z_ret_addr_lookup: Vec<T>,
    p_read_Z_tmp1_lookup: Vec<T>,
    p_reg_write_X_A_lookup: Vec<T>,
    p_reg_write_X_CNT_lookup: Vec<T>,
    p_reg_write_X_b_pc_lookup: Vec<T>,
    p_reg_write_X_dp_lookup: Vec<T>,
    p_reg_write_X_in_ptr_lookup: Vec<T>,
    p_reg_write_X_loop_sp_lookup: Vec<T>,
    p_reg_write_X_op_lookup: Vec<T>,
    p_reg_write_X_ret_addr_lookup: Vec<T>,
    p_reg_write_X_tmp1_lookup: Vec<T>,
    p_reg_write_Y_A_lookup: Vec<T>,
    p_reg_write_Y_CNT_lookup: Vec<T>,
    p_reg_write_Y_b_pc_lookup: Vec<T>,
    p_reg_write_Y_dp_lookup: Vec<T>,
    p_reg_write_Y_in_ptr_lookup: Vec<T>,
    p_reg_write_Y_loop_sp_lookup: Vec<T>,
    p_reg_write_Y_op_lookup: Vec<T>,
    p_reg_write_Y_ret_addr_lookup: Vec<T>,
    p_reg_write_Y_tmp1_lookup: Vec<T>,
    p_reg_write_Z_A_lookup: Vec<T>,
    p_reg_write_Z_CNT_lookup: Vec<T>,
    p_reg_write_Z_b_pc_lookup: Vec<T>,
    p_reg_write_Z_dp_lookup: Vec<T>,
    p_reg_write_Z_in_ptr_lookup: Vec<T>,
    p_reg_write_Z_loop_sp_lookup: Vec<T>,
    p_reg_write_Z_op_lookup: Vec<T>,
    p_reg_write_Z_ret_addr_lookup: Vec<T>,
    p_reg_write_Z_tmp1_lookup: Vec<T>,
    pc: &'a AlgebraicReference,
    X_const: &'a AlgebraicReference,
    X_read_free: &'a AlgebraicReference,
    Y_const: &'a AlgebraicReference,
    Y_read_free: &'a AlgebraicReference,
    Z_const: &'a AlgebraicReference,
    Z_read_free: &'a AlgebraicReference,
    instr__jump_to_operation: &'a AlgebraicReference,
    instr__loop: &'a AlgebraicReference,
    instr__reset: &'a AlgebraicReference,
    instr_branch_if_zero: &'a AlgebraicReference,
    instr_branch_if_zero_param_l: &'a AlgebraicReference,
    instr_fail: &'a AlgebraicReference,
    instr_jump: &'a AlgebraicReference,
    instr_jump_dyn: &'a AlgebraicReference,
    instr_jump_param_l: &'a AlgebraicReference,
    instr_mload: &'a AlgebraicReference,
    instr_mstore: &'a AlgebraicReference,
    instr_return: &'a AlgebraicReference,
    read_X_A: &'a AlgebraicReference,
    read_X_CNT: &'a AlgebraicReference,
    read_X_b_pc: &'a AlgebraicReference,
    read_X_dp: &'a AlgebraicReference,
    read_X_in_ptr: &'a AlgebraicReference,
    read_X_loop_sp: &'a AlgebraicReference,
    read_X_op: &'a AlgebraicReference,
    read_X_pc: &'a AlgebraicReference,
    read_X_ret_addr: &'a AlgebraicReference,
    read_X_tmp1: &'a AlgebraicReference,
    read_Y_A: &'a AlgebraicReference,
    read_Y_CNT: &'a AlgebraicReference,
    read_Y_b_pc: &'a AlgebraicReference,
    read_Y_dp: &'a AlgebraicReference,
    read_Y_in_ptr: &'a AlgebraicReference,
    read_Y_loop_sp: &'a AlgebraicReference,
    read_Y_op: &'a AlgebraicReference,
    read_Y_pc: &'a AlgebraicReference,
    read_Y_ret_addr: &'a AlgebraicReference,
    read_Y_tmp1: &'a AlgebraicReference,
    read_Z_A: &'a AlgebraicReference,
    read_Z_CNT: &'a AlgebraicReference,
    read_Z_b_pc: &'a AlgebraicReference,
    read_Z_dp: &'a AlgebraicReference,
    read_Z_in_ptr: &'a AlgebraicReference,
    read_Z_loop_sp: &'a AlgebraicReference,
    read_Z_op: &'a AlgebraicReference,
    read_Z_pc: &'a AlgebraicReference,
    read_Z_ret_addr: &'a AlgebraicReference,
    read_Z_tmp1: &'a AlgebraicReference,
    reg_write_X_A: &'a AlgebraicReference,
    reg_write_X_CNT: &'a AlgebraicReference,
    reg_write_X_b_pc: &'a AlgebraicReference,
    reg_write_X_dp: &'a AlgebraicReference,
    reg_write_X_in_ptr: &'a AlgebraicReference,
    reg_write_X_loop_sp: &'a AlgebraicReference,
    reg_write_X_op: &'a AlgebraicReference,
    reg_write_X_ret_addr: &'a AlgebraicReference,
    reg_write_X_tmp1: &'a AlgebraicReference,
    reg_write_Y_A: &'a AlgebraicReference,
    reg_write_Y_CNT: &'a AlgebraicReference,
    reg_write_Y_b_pc: &'a AlgebraicReference,
    reg_write_Y_dp: &'a AlgebraicReference,
    reg_write_Y_in_ptr: &'a AlgebraicReference,
    reg_write_Y_loop_sp: &'a AlgebraicReference,
    reg_write_Y_op: &'a AlgebraicReference,
    reg_write_Y_ret_addr: &'a AlgebraicReference,
    reg_write_Y_tmp1: &'a AlgebraicReference,
    reg_write_Z_A: &'a AlgebraicReference,
    reg_write_Z_CNT: &'a AlgebraicReference,
    reg_write_Z_b_pc: &'a AlgebraicReference,
    reg_write_Z_dp: &'a AlgebraicReference,
    reg_write_Z_in_ptr: &'a AlgebraicReference,
    reg_write_Z_loop_sp: &'a AlgebraicReference,
    reg_write_Z_op: &'a AlgebraicReference,
    reg_write_Z_ret_addr: &'a AlgebraicReference,
    reg_write_Z_tmp1: &'a AlgebraicReference,
}

impl<'a, T: FieldElement> ShortcutExec<'a, T> {
    pub fn new(fixed_data: &'a FixedData<'a, T>) -> Self {
        println!("Created exec");
        Self {
            p_X_const_lookup: vec![
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
            ]
            .into_iter()
            .map(|x: u64| T::from(x))
            .collect(),
            p_X_read_free_lookup: vec![
                0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_Y_const_lookup: vec![
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
            ]
            .into_iter()
            .map(|x: u64| T::from(x))
            .collect(),
            p_Y_read_free_lookup: vec![
                0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
                0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 1,
                1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_Z_const_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_Z_read_free_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_instr__jump_to_operation_lookup: vec![
                0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_instr__loop_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_instr__reset_lookup: vec![
                1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_instr_branch_if_zero_lookup: vec![
                0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1,
                0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_instr_branch_if_zero_param_l_lookup: vec![
                0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 0, 16, 0, 0, 0, 0, 49, 51, 53, 56, 59, 62, 26,
                45, 0, 0, 31, 0, 0, 0, 0, 0, 0, 0, 38, 40, 0, 0, 0, 0, 43, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_instr_fail_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_instr_jump_lookup: vec![
                0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 1,
                0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_instr_jump_dyn_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_instr_jump_param_l_lookup: vec![
                0, 0, 66, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 65, 0, 0, 0, 0, 0, 0, 33, 0, 33, 0, 0, 33, 0, 65, 0, 0, 0, 65, 0, 65, 0,
                65, 0, 0, 65, 0, 0, 65, 0, 0, 65, 0, 0, 65, 0, 4, 0, 0, 0, 0, 0, 0, 17, 0, 71, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_instr_mload_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
                0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
                1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_instr_mstore_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
                0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_instr_return_lookup: vec![
                0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_read_X_A_lookup: vec![
                0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
                0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_read_X_CNT_lookup: vec![
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
            ]
            .into_iter()
            .map(|x: u64| T::from(x))
            .collect(),
            p_read_X_b_pc_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_read_X_dp_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0,
                1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_read_X_in_ptr_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_read_X_loop_sp_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_read_X_op_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_read_X_pc_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_read_X_ret_addr_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_read_X_tmp1_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_read_Y_A_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
                0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_read_Y_CNT_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_read_Y_b_pc_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_read_Y_dp_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_read_Y_in_ptr_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_read_Y_loop_sp_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_read_Y_op_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_read_Y_pc_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_read_Y_ret_addr_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_read_Y_tmp1_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_read_Z_A_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_read_Z_CNT_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_read_Z_b_pc_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_read_Z_dp_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_read_Z_in_ptr_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_read_Z_loop_sp_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_read_Z_op_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_read_Z_pc_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_read_Z_ret_addr_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_read_Z_tmp1_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_reg_write_X_A_lookup: vec![
                0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_reg_write_X_CNT_lookup: vec![
                0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_reg_write_X_b_pc_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_reg_write_X_dp_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_reg_write_X_in_ptr_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_reg_write_X_loop_sp_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_reg_write_X_op_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_reg_write_X_ret_addr_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_reg_write_X_tmp1_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_reg_write_Y_A_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
                0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 1,
                1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_reg_write_Y_CNT_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_reg_write_Y_b_pc_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_reg_write_Y_dp_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_reg_write_Y_in_ptr_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_reg_write_Y_loop_sp_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_reg_write_Y_op_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_reg_write_Y_ret_addr_lookup: vec![
                0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_reg_write_Y_tmp1_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_reg_write_Z_A_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_reg_write_Z_CNT_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_reg_write_Z_b_pc_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_reg_write_Z_dp_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_reg_write_Z_in_ptr_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_reg_write_Z_loop_sp_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_reg_write_Z_op_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_reg_write_Z_ret_addr_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            p_reg_write_Z_tmp1_lookup: vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]
            .into_iter()
            .map(|x| T::from(x))
            .collect(),
            pc: alg_ref("main.pc", fixed_data),
            X_const: alg_ref("main.X_const", fixed_data),
            X_read_free: alg_ref("main.X_read_free", fixed_data),
            Y_const: alg_ref("main.Y_const", fixed_data),
            Y_read_free: alg_ref("main.Y_read_free", fixed_data),
            Z_const: alg_ref("main.Z_const", fixed_data),
            Z_read_free: alg_ref("main.Z_read_free", fixed_data),
            instr__jump_to_operation: alg_ref("main.instr__jump_to_operation", fixed_data),
            instr__loop: alg_ref("main.instr__loop", fixed_data),
            instr__reset: alg_ref("main.instr__reset", fixed_data),
            instr_branch_if_zero: alg_ref("main.instr_branch_if_zero", fixed_data),
            instr_branch_if_zero_param_l: alg_ref("main.instr_branch_if_zero_param_l", fixed_data),
            instr_fail: alg_ref("main.instr_fail", fixed_data),
            instr_jump: alg_ref("main.instr_jump", fixed_data),
            instr_jump_dyn: alg_ref("main.instr_jump_dyn", fixed_data),
            instr_jump_param_l: alg_ref("main.instr_jump_param_l", fixed_data),
            instr_mload: alg_ref("main.instr_mload", fixed_data),
            instr_mstore: alg_ref("main.instr_mstore", fixed_data),
            instr_return: alg_ref("main.instr_return", fixed_data),
            read_X_A: alg_ref("main.read_X_A", fixed_data),
            read_X_CNT: alg_ref("main.read_X_CNT", fixed_data),
            read_X_b_pc: alg_ref("main.read_X_b_pc", fixed_data),
            read_X_dp: alg_ref("main.read_X_dp", fixed_data),
            read_X_in_ptr: alg_ref("main.read_X_in_ptr", fixed_data),
            read_X_loop_sp: alg_ref("main.read_X_loop_sp", fixed_data),
            read_X_op: alg_ref("main.read_X_op", fixed_data),
            read_X_pc: alg_ref("main.read_X_pc", fixed_data),
            read_X_ret_addr: alg_ref("main.read_X_ret_addr", fixed_data),
            read_X_tmp1: alg_ref("main.read_X_tmp1", fixed_data),
            read_Y_A: alg_ref("main.read_Y_A", fixed_data),
            read_Y_CNT: alg_ref("main.read_Y_CNT", fixed_data),
            read_Y_b_pc: alg_ref("main.read_Y_b_pc", fixed_data),
            read_Y_dp: alg_ref("main.read_Y_dp", fixed_data),
            read_Y_in_ptr: alg_ref("main.read_Y_in_ptr", fixed_data),
            read_Y_loop_sp: alg_ref("main.read_Y_loop_sp", fixed_data),
            read_Y_op: alg_ref("main.read_Y_op", fixed_data),
            read_Y_pc: alg_ref("main.read_Y_pc", fixed_data),
            read_Y_ret_addr: alg_ref("main.read_Y_ret_addr", fixed_data),
            read_Y_tmp1: alg_ref("main.read_Y_tmp1", fixed_data),
            read_Z_A: alg_ref("main.read_Z_A", fixed_data),
            read_Z_CNT: alg_ref("main.read_Z_CNT", fixed_data),
            read_Z_b_pc: alg_ref("main.read_Z_b_pc", fixed_data),
            read_Z_dp: alg_ref("main.read_Z_dp", fixed_data),
            read_Z_in_ptr: alg_ref("main.read_Z_in_ptr", fixed_data),
            read_Z_loop_sp: alg_ref("main.read_Z_loop_sp", fixed_data),
            read_Z_op: alg_ref("main.read_Z_op", fixed_data),
            read_Z_pc: alg_ref("main.read_Z_pc", fixed_data),
            read_Z_ret_addr: alg_ref("main.read_Z_ret_addr", fixed_data),
            read_Z_tmp1: alg_ref("main.read_Z_tmp1", fixed_data),
            reg_write_X_A: alg_ref("main.reg_write_X_A", fixed_data),
            reg_write_X_CNT: alg_ref("main.reg_write_X_CNT", fixed_data),
            reg_write_X_b_pc: alg_ref("main.reg_write_X_b_pc", fixed_data),
            reg_write_X_dp: alg_ref("main.reg_write_X_dp", fixed_data),
            reg_write_X_in_ptr: alg_ref("main.reg_write_X_in_ptr", fixed_data),
            reg_write_X_loop_sp: alg_ref("main.reg_write_X_loop_sp", fixed_data),
            reg_write_X_op: alg_ref("main.reg_write_X_op", fixed_data),
            reg_write_X_ret_addr: alg_ref("main.reg_write_X_ret_addr", fixed_data),
            reg_write_X_tmp1: alg_ref("main.reg_write_X_tmp1", fixed_data),
            reg_write_Y_A: alg_ref("main.reg_write_Y_A", fixed_data),
            reg_write_Y_CNT: alg_ref("main.reg_write_Y_CNT", fixed_data),
            reg_write_Y_b_pc: alg_ref("main.reg_write_Y_b_pc", fixed_data),
            reg_write_Y_dp: alg_ref("main.reg_write_Y_dp", fixed_data),
            reg_write_Y_in_ptr: alg_ref("main.reg_write_Y_in_ptr", fixed_data),
            reg_write_Y_loop_sp: alg_ref("main.reg_write_Y_loop_sp", fixed_data),
            reg_write_Y_op: alg_ref("main.reg_write_Y_op", fixed_data),
            reg_write_Y_ret_addr: alg_ref("main.reg_write_Y_ret_addr", fixed_data),
            reg_write_Y_tmp1: alg_ref("main.reg_write_Y_tmp1", fixed_data),
            reg_write_Z_A: alg_ref("main.reg_write_Z_A", fixed_data),
            reg_write_Z_CNT: alg_ref("main.reg_write_Z_CNT", fixed_data),
            reg_write_Z_b_pc: alg_ref("main.reg_write_Z_b_pc", fixed_data),
            reg_write_Z_dp: alg_ref("main.reg_write_Z_dp", fixed_data),
            reg_write_Z_in_ptr: alg_ref("main.reg_write_Z_in_ptr", fixed_data),
            reg_write_Z_loop_sp: alg_ref("main.reg_write_Z_loop_sp", fixed_data),
            reg_write_Z_op: alg_ref("main.reg_write_Z_op", fixed_data),
            reg_write_Z_ret_addr: alg_ref("main.reg_write_Z_ret_addr", fixed_data),
            reg_write_Z_tmp1: alg_ref("main.reg_write_Z_tmp1", fixed_data),
        }
    }
}

fn alg_ref<'a, T: FieldElement>(
    name: &str,
    fixed_data: &'a FixedData<'a, T>,
) -> &'a AlgebraicReference {
    &fixed_data.witness_cols[&(&fixed_data.analyzed.definitions[name].0).into()].poly
}

impl<'a, T: FieldElement> ShortcutExec<'a, T> {
    pub fn shortcut_exec(
        &self,
        fixed_data: &FixedData<T>,
        rows: &RowPair<T>,
    ) -> Result<(String, Vec<(&'a AlgebraicReference, T)>), EvalError> {
        //let pc_val = std::convert::int::<fe>(std::prover::eval(main.pc));
        let pc_val = rows
            .get_value(self.pc)
            .ok_or(EvalError::DataNotAvailable)?
            .to_arbitrary_integer();
        let pc_val = usize::try_from(pc_val).unwrap();
        let mut result: Vec<(&'a AlgebraicReference, T)> = vec![];
        // let s = std::prover::set;
        // let _ = s(
        //     main.reg_write_X_b_pc,
        //     main.p_reg_write_X_b_pc_lookup[pc_val],
        // );
        result.push((
            self.reg_write_X_b_pc,
            self.p_reg_write_X_b_pc_lookup[pc_val],
        ));

        // let _ = s(
        //     main.reg_write_Y_b_pc,
        //     main.p_reg_write_Y_b_pc_lookup[pc_val],
        // );
        result.push((
            self.reg_write_Y_b_pc,
            self.p_reg_write_Y_b_pc_lookup[pc_val],
        ));
        // let _ = s(
        //     main.reg_write_Z_b_pc,
        //     main.p_reg_write_Z_b_pc_lookup[pc_val],
        // );
        result.push((
            self.reg_write_Z_b_pc,
            self.p_reg_write_Z_b_pc_lookup[pc_val],
        ));
        // let _ = s(main.reg_write_X_op, main.p_reg_write_X_op_lookup[pc_val]);
        result.push((self.reg_write_X_op, self.p_reg_write_X_op_lookup[pc_val]));
        // let _ = s(main.reg_write_Y_op, main.p_reg_write_Y_op_lookup[pc_val]);
        result.push((self.reg_write_Y_op, self.p_reg_write_Y_op_lookup[pc_val]));
        // let _ = s(main.reg_write_Z_op, main.p_reg_write_Z_op_lookup[pc_val]);
        result.push((self.reg_write_Z_op, self.p_reg_write_Z_op_lookup[pc_val]));
        // let _ = s(main.reg_write_X_dp, main.p_reg_write_X_dp_lookup[pc_val]);
        result.push((self.reg_write_X_dp, self.p_reg_write_X_dp_lookup[pc_val]));
        // let _ = s(main.reg_write_Y_dp, main.p_reg_write_Y_dp_lookup[pc_val]);
        result.push((self.reg_write_Y_dp, self.p_reg_write_Y_dp_lookup[pc_val]));
        // let _ = s(main.reg_write_Z_dp, main.p_reg_write_Z_dp_lookup[pc_val]);
        result.push((self.reg_write_Z_dp, self.p_reg_write_Z_dp_lookup[pc_val]));
        // let _ = s(
        //     main.reg_write_X_in_ptr,
        //     main.p_reg_write_X_in_ptr_lookup[pc_val],
        // );
        result.push((
            self.reg_write_X_in_ptr,
            self.p_reg_write_X_in_ptr_lookup[pc_val],
        ));
        // let _ = s(
        //     main.reg_write_Y_in_ptr,
        //     main.p_reg_write_Y_in_ptr_lookup[pc_val],
        // );
        result.push((
            self.reg_write_Y_in_ptr,
            self.p_reg_write_Y_in_ptr_lookup[pc_val],
        ));
        // let _ = s(
        //     main.reg_write_Z_in_ptr,
        //     main.p_reg_write_Z_in_ptr_lookup[pc_val],
        // );
        result.push((
            self.reg_write_Z_in_ptr,
            self.p_reg_write_Z_in_ptr_lookup[pc_val],
        ));
        // let _ = s(
        //     main.reg_write_X_loop_sp,
        //     main.p_reg_write_X_loop_sp_lookup[pc_val],
        // );
        result.push((
            self.reg_write_X_loop_sp,
            self.p_reg_write_X_loop_sp_lookup[pc_val],
        ));
        // let _ = s(
        //     main.reg_write_Y_loop_sp,
        //     main.p_reg_write_Y_loop_sp_lookup[pc_val],
        // );
        result.push((
            self.reg_write_Y_loop_sp,
            self.p_reg_write_Y_loop_sp_lookup[pc_val],
        ));
        // let _ = s(
        //     main.reg_write_Z_loop_sp,
        //     main.p_reg_write_Z_loop_sp_lookup[pc_val],
        // );
        result.push((
            self.reg_write_Z_loop_sp,
            self.p_reg_write_Z_loop_sp_lookup[pc_val],
        ));
        // let _ = s(
        //     main.reg_write_X_ret_addr,
        //     main.p_reg_write_X_ret_addr_lookup[pc_val],
        // );
        result.push((
            self.reg_write_X_ret_addr,
            self.p_reg_write_X_ret_addr_lookup[pc_val],
        ));
        // let _ = s(
        //     main.reg_write_Y_ret_addr,
        //     main.p_reg_write_Y_ret_addr_lookup[pc_val],
        // );
        result.push((
            self.reg_write_Y_ret_addr,
            self.p_reg_write_Y_ret_addr_lookup[pc_val],
        ));
        // let _ = s(
        //     main.reg_write_Z_ret_addr,
        //     main.p_reg_write_Z_ret_addr_lookup[pc_val],
        // );
        result.push((
            self.reg_write_Z_ret_addr,
            self.p_reg_write_Z_ret_addr_lookup[pc_val],
        ));
        // let _ = s(main.reg_write_X_A, main.p_reg_write_X_A_lookup[pc_val]);
        result.push((self.reg_write_X_A, self.p_reg_write_X_A_lookup[pc_val]));
        // let _ = s(main.reg_write_Y_A, main.p_reg_write_Y_A_lookup[pc_val]);
        result.push((self.reg_write_Y_A, self.p_reg_write_Y_A_lookup[pc_val]));
        // let _ = s(main.reg_write_Z_A, main.p_reg_write_Z_A_lookup[pc_val]);
        result.push((self.reg_write_Z_A, self.p_reg_write_Z_A_lookup[pc_val]));
        // let _ = s(main.reg_write_X_CNT, main.p_reg_write_X_CNT_lookup[pc_val]);
        result.push((self.reg_write_X_CNT, self.p_reg_write_X_CNT_lookup[pc_val]));
        // let _ = s(main.reg_write_Y_CNT, main.p_reg_write_Y_CNT_lookup[pc_val]);
        result.push((self.reg_write_Y_CNT, self.p_reg_write_Y_CNT_lookup[pc_val]));
        // let _ = s(main.reg_write_Z_CNT, main.p_reg_write_Z_CNT_lookup[pc_val]);
        result.push((self.reg_write_Z_CNT, self.p_reg_write_Z_CNT_lookup[pc_val]));
        // let _ = s(
        //     main.reg_write_X_tmp1,
        //     main.p_reg_write_X_tmp1_lookup[pc_val],
        // );
        result.push((
            self.reg_write_X_tmp1,
            self.p_reg_write_X_tmp1_lookup[pc_val],
        ));
        // let _ = s(
        //     main.reg_write_Y_tmp1,
        //     main.p_reg_write_Y_tmp1_lookup[pc_val],
        // );
        result.push((
            self.reg_write_Y_tmp1,
            self.p_reg_write_Y_tmp1_lookup[pc_val],
        ));
        // let _ = s(
        //     main.reg_write_Z_tmp1,
        //     main.p_reg_write_Z_tmp1_lookup[pc_val],
        // );
        result.push((
            self.reg_write_Z_tmp1,
            self.p_reg_write_Z_tmp1_lookup[pc_val],
        ));
        // let _ = s(main.instr_jump, main.p_instr_jump_lookup[pc_val]);
        result.push((self.instr_jump, self.p_instr_jump_lookup[pc_val]));
        // let _ = s(
        //     main.instr_jump_param_l,
        //     main.p_instr_jump_param_l_lookup[pc_val],
        // );
        result.push((
            self.instr_jump_param_l,
            self.p_instr_jump_param_l_lookup[pc_val],
        ));
        // let _ = s(main.instr_jump_dyn, main.p_instr_jump_dyn_lookup[pc_val]);
        result.push((self.instr_jump_dyn, self.p_instr_jump_dyn_lookup[pc_val]));
        // let _ = s(
        //     main.instr_branch_if_zero,
        //     main.p_instr_branch_if_zero_lookup[pc_val],
        // );
        result.push((
            self.instr_branch_if_zero,
            self.p_instr_branch_if_zero_lookup[pc_val],
        ));
        // let _ = s(
        //     main.instr_branch_if_zero_param_l,
        //     main.p_instr_branch_if_zero_param_l_lookup[pc_val],
        // );
        result.push((
            self.instr_branch_if_zero_param_l,
            self.p_instr_branch_if_zero_param_l_lookup[pc_val],
        ));
        // let _ = s(main.instr_fail, main.p_instr_fail_lookup[pc_val]);
        result.push((self.instr_fail, self.p_instr_fail_lookup[pc_val]));
        // let _ = s(main.instr_mload, main.p_instr_mload_lookup[pc_val]);
        result.push((self.instr_mload, self.p_instr_mload_lookup[pc_val]));
        // let _ = s(main.instr_mstore, main.p_instr_mstore_lookup[pc_val]);
        result.push((self.instr_mstore, self.p_instr_mstore_lookup[pc_val]));
        // let _ = s(
        //     main.instr__jump_to_operation,
        //     main.p_instr__jump_to_operation_lookup[pc_val],
        // );
        result.push((
            self.instr__jump_to_operation,
            self.p_instr__jump_to_operation_lookup[pc_val],
        ));
        // let _ = s(main.instr__reset, main.p_instr__reset_lookup[pc_val]);
        result.push((self.instr__reset, self.p_instr__reset_lookup[pc_val]));
        // let _ = s(main.instr__loop, main.p_instr__loop_lookup[pc_val]);
        result.push((self.instr__loop, self.p_instr__loop_lookup[pc_val]));
        // let _ = s(main.instr_return, main.p_instr_return_lookup[pc_val]);
        result.push((self.instr_return, self.p_instr_return_lookup[pc_val]));
        // let _ = s(main.X_const, main.p_X_const_lookup[pc_val]);
        result.push((self.X_const, self.p_X_const_lookup[pc_val]));
        // let _ = s(main.X_read_free, main.p_X_read_free_lookup[pc_val]);
        result.push((self.X_read_free, self.p_X_read_free_lookup[pc_val]));
        // let _ = s(main.read_X_A, main.p_read_X_A_lookup[pc_val]);
        result.push((self.read_X_A, self.p_read_X_A_lookup[pc_val]));
        // let _ = s(main.read_X_CNT, main.p_read_X_CNT_lookup[pc_val]);
        result.push((self.read_X_CNT, self.p_read_X_CNT_lookup[pc_val]));
        // let _ = s(main.read_X_b_pc, main.p_read_X_b_pc_lookup[pc_val]);
        result.push((self.read_X_b_pc, self.p_read_X_b_pc_lookup[pc_val]));
        // let _ = s(main.read_X_dp, main.p_read_X_dp_lookup[pc_val]);
        result.push((self.read_X_dp, self.p_read_X_dp_lookup[pc_val]));
        // let _ = s(main.read_X_in_ptr, main.p_read_X_in_ptr_lookup[pc_val]);
        result.push((self.read_X_in_ptr, self.p_read_X_in_ptr_lookup[pc_val]));
        // let _ = s(main.read_X_loop_sp, main.p_read_X_loop_sp_lookup[pc_val]);
        result.push((self.read_X_loop_sp, self.p_read_X_loop_sp_lookup[pc_val]));
        // let _ = s(main.read_X_op, main.p_read_X_op_lookup[pc_val]);
        result.push((self.read_X_op, self.p_read_X_op_lookup[pc_val]));
        // let _ = s(main.read_X_ret_addr, main.p_read_X_ret_addr_lookup[pc_val]);
        result.push((self.read_X_ret_addr, self.p_read_X_ret_addr_lookup[pc_val]));
        // let _ = s(main.read_X_tmp1, main.p_read_X_tmp1_lookup[pc_val]);
        result.push((self.read_X_tmp1, self.p_read_X_tmp1_lookup[pc_val]));
        // let _ = s(main.read_X_pc, main.p_read_X_pc_lookup[pc_val]);
        result.push((self.read_X_pc, self.p_read_X_pc_lookup[pc_val]));
        // let _ = s(main.Y_const, main.p_Y_const_lookup[pc_val]);
        result.push((self.Y_const, self.p_Y_const_lookup[pc_val]));
        // let _ = s(main.Y_read_free, main.p_Y_read_free_lookup[pc_val]);
        result.push((self.Y_read_free, self.p_Y_read_free_lookup[pc_val]));
        // let _ = s(main.read_Y_A, main.p_read_Y_A_lookup[pc_val]);
        result.push((self.read_Y_A, self.p_read_Y_A_lookup[pc_val]));
        // let _ = s(main.read_Y_CNT, main.p_read_Y_CNT_lookup[pc_val]);
        result.push((self.read_Y_CNT, self.p_read_Y_CNT_lookup[pc_val]));
        // let _ = s(main.read_Y_b_pc, main.p_read_Y_b_pc_lookup[pc_val]);
        result.push((self.read_Y_b_pc, self.p_read_Y_b_pc_lookup[pc_val]));
        // let _ = s(main.read_Y_dp, main.p_read_Y_dp_lookup[pc_val]);
        result.push((self.read_Y_dp, self.p_read_Y_dp_lookup[pc_val]));
        // let _ = s(main.read_Y_in_ptr, main.p_read_Y_in_ptr_lookup[pc_val]);
        result.push((self.read_Y_in_ptr, self.p_read_Y_in_ptr_lookup[pc_val]));
        // let _ = s(main.read_Y_loop_sp, main.p_read_Y_loop_sp_lookup[pc_val]);
        result.push((self.read_Y_loop_sp, self.p_read_Y_loop_sp_lookup[pc_val]));
        // let _ = s(main.read_Y_op, main.p_read_Y_op_lookup[pc_val]);
        result.push((self.read_Y_op, self.p_read_Y_op_lookup[pc_val]));
        // let _ = s(main.read_Y_ret_addr, main.p_read_Y_ret_addr_lookup[pc_val]);
        result.push((self.read_Y_ret_addr, self.p_read_Y_ret_addr_lookup[pc_val]));
        // let _ = s(main.read_Y_tmp1, main.p_read_Y_tmp1_lookup[pc_val]);
        result.push((self.read_Y_tmp1, self.p_read_Y_tmp1_lookup[pc_val]));
        // let _ = s(main.read_Y_pc, main.p_read_Y_pc_lookup[pc_val]);
        result.push((self.read_Y_pc, self.p_read_Y_pc_lookup[pc_val]));
        // let _ = s(main.Z_const, main.p_Z_const_lookup[pc_val]);
        result.push((self.Z_const, self.p_Z_const_lookup[pc_val]));
        // let _ = s(main.Z_read_free, main.p_Z_read_free_lookup[pc_val]);
        result.push((self.Z_read_free, self.p_Z_read_free_lookup[pc_val]));
        // let _ = s(main.read_Z_A, main.p_read_Z_A_lookup[pc_val]);
        result.push((self.read_Z_A, self.p_read_Z_A_lookup[pc_val]));
        // let _ = s(main.read_Z_CNT, main.p_read_Z_CNT_lookup[pc_val]);
        result.push((self.read_Z_CNT, self.p_read_Z_CNT_lookup[pc_val]));
        // let _ = s(main.read_Z_b_pc, main.p_read_Z_b_pc_lookup[pc_val]);
        result.push((self.read_Z_b_pc, self.p_read_Z_b_pc_lookup[pc_val]));
        // let _ = s(main.read_Z_dp, main.p_read_Z_dp_lookup[pc_val]);
        result.push((self.read_Z_dp, self.p_read_Z_dp_lookup[pc_val]));
        // let _ = s(main.read_Z_in_ptr, main.p_read_Z_in_ptr_lookup[pc_val]);
        result.push((self.read_Z_in_ptr, self.p_read_Z_in_ptr_lookup[pc_val]));
        // let _ = s(main.read_Z_loop_sp, main.p_read_Z_loop_sp_lookup[pc_val]);
        result.push((self.read_Z_loop_sp, self.p_read_Z_loop_sp_lookup[pc_val]));
        // let _ = s(main.read_Z_op, main.p_read_Z_op_lookup[pc_val]);
        result.push((self.read_Z_op, self.p_read_Z_op_lookup[pc_val]));
        // let _ = s(main.read_Z_ret_addr, main.p_read_Z_ret_addr_lookup[pc_val]);
        result.push((self.read_Z_ret_addr, self.p_read_Z_ret_addr_lookup[pc_val]));
        // let _ = s(main.read_Z_tmp1, main.p_read_Z_tmp1_lookup[pc_val]);
        result.push((self.read_Z_tmp1, self.p_read_Z_tmp1_lookup[pc_val]));
        // let _ = s(main.read_Z_pc, main.p_read_Z_pc_lookup[pc_val]);
        result.push((self.read_Z_pc, self.p_read_Z_pc_lookup[pc_val]));

        Ok(("None".to_string(), result))
    }
}
