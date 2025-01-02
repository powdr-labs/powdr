use super::compiler::{written_vars_in_effect, WitgenFunctionParams};
use super::effect::{Assertion, Effect};

use super::symbolic_expression::{
    BinaryOperator, BitOperator, RPNExpression, RPNExpressionElem, UnaryOperator,
};
use super::variable::{Cell, Variable};
use crate::witgen::data_structures::mutable_state::MutableState;
use crate::witgen::jit::effect::MachineCallArgument;
use crate::witgen::machines::LookupCell;
use crate::witgen::QueryCallback;
use powdr_number::{FieldElement, LargeInt};

use std::collections::HashMap;

// Witgen effects compiled into interpreter instructions.
pub struct EffectsInterpreter<T: FieldElement> {
    first_column_id: u64,
    column_count: usize,
    vars: usize,
    actions: Vec<InterpreterAction<T>>,
}

// Witgen effects compiled into "instructions".
// Variables have been removed and replaced by their index in the variable list.
enum InterpreterAction<T: FieldElement> {
    ReadCell(usize, Cell),
    ReadParam(usize, usize),
    AssignExpression(usize, RPNExpression<T, usize>),
    WriteCell(usize, Cell),
    WriteParam(usize, usize),
    WriteKnown(Cell),
    MachineCall(Vec<usize>, u64, Vec<MachineCallArgument<T, usize>>),
    Assertion(RPNExpression<T, usize>, RPNExpression<T, usize>, bool),
}

impl<T: FieldElement> EffectsInterpreter<T> {
    pub fn new(
        first_column_id: u64,
        column_count: usize,
        known_inputs: &[Variable],
        effects: &[Effect<T, Variable>],
    ) -> Self {
        let mut actions = vec![];
        let mut var_idx = HashMap::new();
        let mut vars = 0;

        // map variables to indices, so they can be quickly accessed as sequential memory (kept in a vec) during execution
        let mut map_var_idx = |var: &Variable| -> usize {
            let idx = *var_idx.entry(var.clone()).or_insert_with(|| {
                let idx = vars;
                vars += 1;
                idx
            });
            idx
        };

        // load known inputs
        for var in known_inputs.iter() {
            match var {
                Variable::Cell(c) => {
                    let idx = map_var_idx(var);
                    actions.push(InterpreterAction::ReadCell(idx, c.clone()));
                }
                Variable::Param(i) => {
                    let idx = map_var_idx(var);
                    actions.push(InterpreterAction::ReadParam(idx, *i));
                }
                Variable::MachineCallReturnValue(_) => unreachable!(),
            }
        }

        // process effect
        for effect in effects.iter() {
            match effect {
                Effect::Assignment(var, e) => {
                    let idx = map_var_idx(var);
                    actions.push(InterpreterAction::AssignExpression(
                        idx,
                        e.map_variables(&mut map_var_idx).to_rpn(),
                    ));
                }
                Effect::RangeConstraint(..) => {
                    unreachable!("Final code should not contain pure range constraints.")
                }
                Effect::Assertion(Assertion {
                    lhs,
                    rhs,
                    expected_equal,
                }) => {
                    actions.push(InterpreterAction::Assertion(
                        lhs.map_variables(&mut map_var_idx).to_rpn(),
                        rhs.map_variables(&mut map_var_idx).to_rpn(),
                        *expected_equal,
                    ));
                }
                Effect::MachineCall(id, arguments) => {
                    let mut result_vars = vec![];

                    arguments.iter().for_each(|a| match a {
                        MachineCallArgument::Unknown(v) => {
                            let idx = map_var_idx(v);
                            result_vars.push(idx);
                        }
                        MachineCallArgument::Known(_) => {}
                    });

                    actions.push(InterpreterAction::MachineCall(
                        result_vars,
                        *id,
                        arguments
                            .iter()
                            .map(|a| match a {
                                MachineCallArgument::Unknown(_) => MachineCallArgument::Unknown(0),
                                MachineCallArgument::Known(v) => {
                                    MachineCallArgument::Known(v.map_variables(&mut map_var_idx))
                                }
                            })
                            .collect(),
                    ));
                }
            }
        }

        // write variables back
        let vars_known: Vec<_> = effects.iter().flat_map(written_vars_in_effect).collect();
        vars_known.iter().for_each(|var| {
            match var {
                Variable::Cell(cell) => {
                    let idx = var_idx.get(var).unwrap();
                    actions.push(InterpreterAction::WriteCell(*idx, cell.clone()));
                    actions.push(InterpreterAction::WriteKnown(cell.clone()));
                }
                Variable::Param(i) => {
                    let idx = var_idx.get(var).unwrap();
                    actions.push(InterpreterAction::WriteParam(*idx, *i));
                }
                Variable::MachineCallReturnValue(_) => {
                    // This is just an internal variable.
                }
            }
        });

        Self {
            first_column_id,
            column_count,
            vars,
            actions,
        }
    }

    // Execute the machine effects for the given the parameters
    pub fn call<Q: QueryCallback<T>>(&self, params: WitgenFunctionParams<'_, T>) {
        let known = known_to_slice(self.column_count, params.known, params.data.len);
        let data = params.data.into_mut_slice();
        let pparams = params.params.into_mut_slice();

        let mut vars = vec![None; self.vars];

        let mut eval_stack = vec![];
        for action in &self.actions {
            match action {
                InterpreterAction::AssignExpression(idx, e) => {
                    let val = self.evaluate_expression(&mut eval_stack, &vars, e);
                    assert!(vars[*idx].replace(val).is_none());
                }
                InterpreterAction::ReadCell(idx, c) => {
                    assert!(vars[*idx]
                        .replace(
                            data[index(
                                self.first_column_id,
                                self.column_count,
                                params.row_offset,
                                c.row_offset,
                                c.id,
                            )]
                        )
                        .is_none())
                }
                InterpreterAction::ReadParam(idx, i) => {
                    assert!(vars[*idx].replace(get_param(pparams, *i)).is_none());
                }
                InterpreterAction::WriteCell(idx, c) => {
                    set(
                        self.first_column_id,
                        self.column_count,
                        data,
                        params.row_offset,
                        c.row_offset,
                        c.id,
                        vars[*idx].unwrap(),
                    );
                }
                InterpreterAction::WriteParam(idx, i) => {
                    set_param(pparams, *i, vars[*idx].unwrap());
                }
                InterpreterAction::WriteKnown(c) => {
                    set_known(
                        self.first_column_id,
                        self.column_count,
                        known,
                        params.row_offset,
                        c.row_offset,
                        c.id,
                    );
                }
                InterpreterAction::MachineCall(result_vars, id, arguments) => {
                    let mutable_state =
                        unsafe { &*(params.mutable_state as *const MutableState<T, Q>) };

                    let mut arg_values: Vec<_> = arguments
                        .iter()
                        .map(|a| match a {
                            MachineCallArgument::Unknown(v) => (Some(v), Default::default()),
                            MachineCallArgument::Known(v) => (
                                None,
                                self.evaluate_expression(&mut eval_stack, &vars, &v.to_rpn()),
                            ),
                        })
                        .collect();

                    // call machine
                    let mut args = arguments
                        .iter()
                        .zip(arg_values.iter_mut())
                        .map(|(a, v)| match a {
                            MachineCallArgument::Unknown(_) => LookupCell::Output(&mut v.1),
                            MachineCallArgument::Known(_) => LookupCell::Input(&v.1),
                        })
                        .collect::<Vec<_>>();

                    mutable_state.call_direct(*id, &mut args[..]).unwrap();

                    // write output to variables
                    let mut var_idx = 0;
                    args.into_iter().for_each(|arg| {
                        if let LookupCell::Output(v) = arg {
                            assert!(vars[result_vars[var_idx]].replace(*v).is_none());
                            var_idx += 1;
                        }
                    });
                }
                InterpreterAction::Assertion(e1, e2, expected_equal) => {
                    let lhs_value = self.evaluate_expression(&mut eval_stack, &vars, e1);
                    let rhs_value = self.evaluate_expression(&mut eval_stack, &vars, e2);
                    if *expected_equal {
                        assert_eq!(lhs_value, rhs_value, "Assertion failed");
                    } else {
                        assert_ne!(lhs_value, rhs_value, "Assertion failed");
                    }
                }
            }
        }
    }

    // evaluates an expression using the provided variable values
    fn evaluate_expression(
        &self,
        stack: &mut Vec<T>,
        vars: &[Option<T>],
        expr: &RPNExpression<T, usize>,
    ) -> T {
        for elem in expr.elems.iter() {
            match elem {
                RPNExpressionElem::Concrete(v) => stack.push(*v),
                RPNExpressionElem::Symbol(idx) => stack.push(vars[*idx].unwrap()),
                RPNExpressionElem::BinaryOperation(op) => {
                    let right = stack.pop().unwrap();
                    let left = stack.pop().unwrap();
                    let result = match op {
                        BinaryOperator::Add => left + right,
                        BinaryOperator::Sub => left - right,
                        BinaryOperator::Mul => left * right,
                        BinaryOperator::Div => left / right,
                        BinaryOperator::IntegerDiv => T::from(
                            left.to_integer().try_into_u64().unwrap()
                                / right.to_integer().try_into_u64().unwrap(),
                        ),
                    };
                    stack.push(result);
                }
                RPNExpressionElem::UnaryOperation(op) => {
                    let inner = stack.pop().unwrap();
                    let result = match op {
                        UnaryOperator::Neg => -inner,
                    };
                    stack.push(result);
                }
                RPNExpressionElem::BitOperation(op, right) => {
                    let left = stack.pop().unwrap();
                    let result = match op {
                        BitOperator::And => T::from(left.to_integer() & *right),
                    };
                    stack.push(result);
                }
            }
        }
        stack.pop().unwrap()
    }
}

// the following functions come from the interface.rs file also included in the compiled jit code

#[inline]
fn known_to_slice<'a>(column_count: usize, known: *mut u32, len: u64) -> &'a mut [u32] {
    let words_per_row = (column_count as u64 + 31) / 32;
    let rows = len / column_count as u64;
    let known_len = rows * words_per_row;
    unsafe { std::slice::from_raw_parts_mut(known, known_len as usize) }
}

#[inline]
fn index(
    first_column_id: u64,
    column_count: usize,
    global_offset: u64,
    local_offset: i32,
    column: u64,
) -> usize {
    let column = column - first_column_id;
    let row = (global_offset as i64 + local_offset as i64) as u64;
    (row * column_count as u64 + column) as usize
}

#[inline]
fn index_known(
    first_column_id: u64,
    column_count: usize,
    global_offset: u64,
    local_offset: i32,
    column: u64,
) -> (u64, u64) {
    let column = column - first_column_id;
    let row = (global_offset as i64 + local_offset as i64) as u64;
    let words_per_row = (column_count as u64 + 31) / 32;
    (row * words_per_row + column / 32, column % 32)
}

#[inline]
fn set<T: FieldElement>(
    first_column_id: u64,
    column_count: usize,
    data: &mut [T],
    global_offset: u64,
    local_offset: i32,
    column: u64,
    value: T,
) {
    let i = index(
        first_column_id,
        column_count,
        global_offset,
        local_offset,
        column,
    );
    data[i] = value;
}

#[inline]
fn set_known(
    first_column_id: u64,
    column_count: usize,
    known: &mut [u32],
    global_offset: u64,
    local_offset: i32,
    column: u64,
) {
    let (known_idx, known_bit) = index_known(
        first_column_id,
        column_count,
        global_offset,
        local_offset,
        column,
    );
    known[known_idx as usize] |= 1 << (known_bit);
}

#[inline]
fn get_param<T: FieldElement>(params: &[LookupCell<T>], i: usize) -> T {
    match params[i] {
        LookupCell::Input(v) => *v,
        LookupCell::Output(_) => panic!("Output cell used as input"),
    }
}
#[inline]
fn set_param<T: FieldElement>(params: &mut [LookupCell<T>], i: usize, value: T) {
    match &mut params[i] {
        LookupCell::Input(_) => panic!("Input cell used as output"),
        LookupCell::Output(v) => **v = value,
    }
}
