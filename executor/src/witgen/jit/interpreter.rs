use super::compiler::written_vars_in_effect;
use super::effect::{Assertion, Effect};

use super::symbolic_expression::{BinaryOperator, BitOperator, SymbolicExpression, UnaryOperator};
use super::variable::{Cell, Variable};
use crate::witgen::data_structures::finalizable_data::CompactDataRef;
use crate::witgen::data_structures::mutable_state::MutableState;
use crate::witgen::jit::effect::MachineCallArgument;
use crate::witgen::machines::LookupCell;
use crate::witgen::QueryCallback;
use powdr_number::FieldElement;

use std::collections::{BTreeSet, HashMap};

// Witgen effects compiled into interpreter instructions.
pub struct EffectsInterpreter<T: FieldElement> {
    first_column_id: u64,
    column_count: usize,
    var_count: usize,
    actions: Vec<InterpreterAction<T>>,
}

// Witgen effects compiled into "instructions".
// Variables have been removed and replaced by their index in the variable list.
#[derive(Debug)]
enum InterpreterAction<T: FieldElement> {
    ReadCell(usize, Cell),
    ReadParam(usize, usize),
    AssignExpression(usize, RPNExpression<T, usize>),
    WriteCell(usize, Cell),
    WriteParam(usize, usize),
    MachineCall(u64, Vec<MachineCallArgumentIdx>),
    Assertion(RPNExpression<T, usize>, RPNExpression<T, usize>, bool),
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum MachineCallArgumentIdx {
    // var index of the evaluated known argument expression
    Known(usize),
    // var index of the unknown
    Unknown(usize),
}

pub struct EffectsInterpreterBuilder<T: FieldElement> {
    var_mapper: VariableMapper,
    expr_idx: HashMap<RPNExpression<T, usize>, usize>,
    actions: Vec<InterpreterAction<T>>,
    stack: Vec<RPNExpressionElem<T, usize>>,
}

impl<T: FieldElement> EffectsInterpreterBuilder<T> {
    fn new() -> Self {
        Self {
            var_mapper: VariableMapper::new(),
            expr_idx: HashMap::new(),
            actions: vec![],
            stack: vec![],
        }
    }

    fn build(
        mut self,
        first_column_id: u64,
        column_count: usize,
        known_inputs: &[Variable],
        effects: &[Effect<T, Variable>],
    ) -> EffectsInterpreter<T> {
        self.load_known_inputs(known_inputs);
        self.process_effects(effects);
        self.write_data(effects);
        self.replace_redundant_vars();

        assert!(self.is_valid());
        assert!(self.stack.is_empty());

        EffectsInterpreter {
            first_column_id,
            column_count,
            var_count: self.var_mapper.var_count(),
            actions: self.actions,
        }
    }

    fn load_known_inputs(&mut self, known_inputs: &[Variable]) {
        let mut reads: Vec<_> = known_inputs
            .iter()
            .map(|var| match var {
                Variable::Cell(c) => {
                    let idx = self.var_mapper.map_var(var);
                    InterpreterAction::ReadCell(idx, c.clone())
                }
                Variable::Param(i) => {
                    let idx = self.var_mapper.map_var(var);
                    InterpreterAction::ReadParam(idx, *i)
                }
                Variable::MachineCallReturnValue(_) => unreachable!(),
            })
            .collect();

        // sort by row offset and then by column id, for memory access locality
        reads.sort_by_key(|a| match a {
            InterpreterAction::ReadCell(_, c) => (c.row_offset, c.id),
            InterpreterAction::ReadParam(_, idx) => (-1, *idx as u64),
            _ => unreachable!(),
        });

        self.actions.extend(reads);
    }

    /// convert expression to three-access code
    fn tac_convert(&mut self, expr: RPNExpression<T, usize>) -> RPNExpression<T, usize> {
        for e in expr.elems {
            match e {
                RPNExpressionElem::Constant(_) => self.stack.push(e),
                RPNExpressionElem::Symbol(_) => self.stack.push(e),
                RPNExpressionElem::BinaryAdd
                | RPNExpressionElem::BinaryMul
                | RPNExpressionElem::BinarySub
                | RPNExpressionElem::BinaryDiv
                | RPNExpressionElem::BinaryIntegerDiv => {
                    let mut right = self.stack.pop().unwrap();
                    assert!(matches!(
                        right,
                        RPNExpressionElem::Symbol(_) | RPNExpressionElem::Constant(_)
                    ));
                    let mut left = self.stack.pop().unwrap();
                    assert!(matches!(
                        left,
                        RPNExpressionElem::Symbol(_) | RPNExpressionElem::Constant(_)
                    ));
                    // order commutative operations so e.g., a+b == b+a
                    if matches!(
                        e,
                        RPNExpressionElem::BinaryAdd | RPNExpressionElem::BinaryMul
                    ) && right < left
                    {
                        std::mem::swap(&mut right, &mut left);
                    }
                    let expr = RPNExpression {
                        elems: vec![left, right, e],
                    };
                    let idx = self.expr_idx.entry(expr.clone()).or_insert_with(|| {
                        let new_var = self.var_mapper.reserve_idx();
                        self.actions
                            .push(InterpreterAction::AssignExpression(new_var, expr));
                        new_var
                    });
                    self.stack.push(RPNExpressionElem::Symbol(*idx));
                }
                RPNExpressionElem::UnaryNeg | RPNExpressionElem::UnaryBitAnd(_) => {
                    let inner = self.stack.pop().unwrap();
                    assert!(matches!(
                        inner,
                        RPNExpressionElem::Symbol(_) | RPNExpressionElem::Constant(_)
                    ));
                    let expr = RPNExpression {
                        elems: vec![inner, e],
                    };
                    let idx = self.expr_idx.entry(expr.clone()).or_insert_with(|| {
                        let new_var = self.var_mapper.reserve_idx();
                        self.actions
                            .push(InterpreterAction::AssignExpression(new_var, expr));
                        new_var
                    });
                    self.stack.push(RPNExpressionElem::Symbol(*idx));
                }
            }
        }
        let var = self.stack.pop().unwrap();
        assert!(matches!(
            var,
            RPNExpressionElem::Symbol(_) | RPNExpressionElem::Constant(_)
        ));
        RPNExpression { elems: vec![var] }
    }

    fn process_effects(&mut self, effects: &[Effect<T, Variable>]) {
        effects.iter().for_each(|effect| {
            let action = match effect {
                Effect::Assignment(var, e) => {
                    let expr = self.var_mapper.map_expr_to_rpn(e);
                    let result = self.tac_convert(expr);
                    let idx = self.var_mapper.map_var(var);
                    InterpreterAction::AssignExpression(idx, result)
                }
                Effect::RangeConstraint(..) => {
                    unreachable!("Final code should not contain pure range constraints.")
                }
                Effect::Assertion(Assertion {
                    lhs,
                    rhs,
                    expected_equal,
                }) => {
                    let e1 = self.var_mapper.map_expr_to_rpn(lhs);
                    let e2 = self.var_mapper.map_expr_to_rpn(rhs);
                    let r1 = self.tac_convert(e1);
                    let r2 = self.tac_convert(e2);
                    InterpreterAction::Assertion(r1, r2, *expected_equal)
                }
                Effect::MachineCall(id, arguments) => {
                    InterpreterAction::MachineCall(
                        *id,
                        arguments
                            .iter()
                            .map(|a| match a {
                                MachineCallArgument::Unknown(v) => {
                                    MachineCallArgumentIdx::Unknown(self.var_mapper.map_var(v))
                                }
                                MachineCallArgument::Known(e) => {
                                    // convert known arguments into variable assignments that are then referenced
                                    let idx = self.var_mapper.reserve_idx();
                                    let expr = self.var_mapper.map_expr_to_rpn(e);
                                    let result = self.tac_convert(expr);
                                    self.actions
                                        .push(InterpreterAction::AssignExpression(idx, result));
                                    MachineCallArgumentIdx::Known(idx)
                                }
                            })
                            .collect(),
                    )
                }
            };
            self.actions.push(action);
            assert!(self.is_valid());
        })
    }

    fn write_data(&mut self, effects: &[Effect<T, Variable>]) {
        let mut writes = vec![];
        effects
            .iter()
            .flat_map(written_vars_in_effect)
            .for_each(|var| {
                match var {
                    Variable::Cell(cell) => {
                        let idx = self.var_mapper.get_var(var).unwrap();
                        writes.push(InterpreterAction::WriteCell(idx, cell.clone()));
                    }
                    Variable::Param(i) => {
                        let idx = self.var_mapper.get_var(var).unwrap();
                        writes.push(InterpreterAction::WriteParam(idx, *i));
                    }
                    Variable::MachineCallReturnValue(_) => {
                        // This is just an internal variable.
                    }
                }
            });

        // sort by row offset and then by column id, for memory access locality
        writes.sort_by_key(|a| match a {
            InterpreterAction::WriteCell(_, c) => (c.row_offset, c.id),
            InterpreterAction::WriteParam(idx, _) => (-1, *idx as u64),
            _ => unreachable!(),
        });

        self.actions.extend(writes);
    }

    // remove simple variable assignments like a=b, replacing a by b in all expressions
    fn replace_redundant_vars(&mut self) {
        let replacements = self
            .actions
            .iter()
            .filter_map(|action| match action {
                InterpreterAction::AssignExpression(idx, expr) if expr.elems.len() == 1 => {
                    if let RPNExpressionElem::Symbol(new_idx) = &expr.elems[0] {
                        Some((*idx, *new_idx))
                    } else {
                        None
                    }
                }
                _ => None,
            })
            .collect::<HashMap<_, _>>();

        // remove assignments to variables with a replacement
        self.actions.retain(|action| match action {
            InterpreterAction::AssignExpression(idx, _) => !replacements.contains_key(idx),
            _ => true,
        });

        // replace variables in all expressions
        for action in &mut self.actions {
            match action {
                InterpreterAction::AssignExpression(_, expr) => {
                    expr.elems.iter_mut().for_each(|e| {
                        if let RPNExpressionElem::Symbol(old_idx) = e {
                            while let Some(new_idx) = replacements.get(old_idx) {
                                *old_idx = *new_idx;
                            }
                        }
                    });
                }
                InterpreterAction::Assertion(lhs, rhs, _) => {
                    lhs.elems.iter_mut().for_each(|e| {
                        if let RPNExpressionElem::Symbol(old_idx) = e {
                            while let Some(new_idx) = replacements.get(old_idx) {
                                *old_idx = *new_idx;
                            }
                        }
                    });
                    rhs.elems.iter_mut().for_each(|e| {
                        if let RPNExpressionElem::Symbol(old_idx) = e {
                            while let Some(new_idx) = replacements.get(old_idx) {
                                *old_idx = *new_idx;
                            }
                        }
                    });
                }
                InterpreterAction::MachineCall(_, params) => {
                    for param in params {
                        if let MachineCallArgumentIdx::Known(old_idx) = param {
                            while let Some(new_idx) = replacements.get(old_idx) {
                                *old_idx = *new_idx;
                            }
                        }
                    }
                }
                InterpreterAction::WriteCell(old_idx, _) => {
                    while let Some(new_idx) = replacements.get(old_idx) {
                        *old_idx = *new_idx;
                    }
                }
                InterpreterAction::WriteParam(old_idx, _) => {
                    while let Some(new_idx) = replacements.get(old_idx) {
                        *old_idx = *new_idx;
                    }
                }
                _ => {}
            }
        }
    }

    // Check that actions are valid (e.g., variables writen to only once, and only read after being written to)
    fn is_valid(&self) -> bool {
        let mut prev_writes = BTreeSet::new();
        for action in &self.actions {
            let writes = action.writes();
            // writing to a variable already written?
            if !writes.is_disjoint(&prev_writes) {
                return false;
            }
            // reading a variable that was not written to?
            if !action.reads().is_subset(&prev_writes) {
                return false;
            }
            prev_writes.extend(writes);
        }
        true
    }
}

impl<T: FieldElement> EffectsInterpreter<T> {
    pub fn new(
        first_column_id: u64,
        column_count: usize,
        known_inputs: &[Variable],
        effects: &[Effect<T, Variable>],
    ) -> Self {
        EffectsInterpreterBuilder::new().build(first_column_id, column_count, known_inputs, effects)
    }

    // Execute the machine effects for the given the parameters
    pub fn call<Q: QueryCallback<T>>(
        &self,
        mutable_state: &MutableState<'_, T, Q>,
        params: &mut [LookupCell<T>],
        mut data: CompactDataRef<'_, T>,
    ) {
        let row_offset = data.row_offset().try_into().unwrap();
        let (data, known) = data.as_mut_slices();

        let mut vars = vec![T::zero(); self.var_count];

        for action in &self.actions {
            match action {
                InterpreterAction::AssignExpression(idx, e) => {
                    let val = e.evaluate_tac(&vars[..]);
                    vars[*idx] = val;
                }
                InterpreterAction::ReadCell(idx, c) => {
                    vars[*idx] = data[index(
                        self.first_column_id,
                        self.column_count,
                        row_offset,
                        c.row_offset,
                        c.id,
                    )];
                }
                InterpreterAction::ReadParam(idx, i) => {
                    vars[*idx] = get_param(params, *i);
                }
                InterpreterAction::WriteCell(idx, c) => {
                    set(
                        self.first_column_id,
                        self.column_count,
                        data,
                        row_offset,
                        c.row_offset,
                        c.id,
                        vars[*idx],
                    );
                    set_known(
                        self.first_column_id,
                        self.column_count,
                        known,
                        row_offset,
                        c.row_offset,
                        c.id,
                    );
                }
                InterpreterAction::WriteParam(idx, i) => {
                    set_param(params, *i, vars[*idx]);
                }
                InterpreterAction::MachineCall(id, arguments) => {
                    // we know it's safe to escape the references here, but the compiler doesn't, so we use unsafe
                    let mut args = arguments
                        .iter()
                        .map(|a| match a {
                            MachineCallArgumentIdx::Unknown(idx) => {
                                let var = &mut vars[*idx] as *mut T;
                                LookupCell::Output(unsafe { var.as_mut().unwrap() })
                            }
                            MachineCallArgumentIdx::Known(idx) => {
                                let var = &vars[*idx] as *const T;
                                LookupCell::Input(unsafe { var.as_ref().unwrap() })
                            }
                        })
                        .collect::<Vec<_>>();
                    mutable_state.call_direct(*id, &mut args[..]).unwrap();
                }
                InterpreterAction::Assertion(e1, e2, expected_equal) => {
                    let lhs_value = e1.evaluate_tac(&vars);
                    let rhs_value = e2.evaluate_tac(&vars);
                    if *expected_equal {
                        assert_eq!(lhs_value, rhs_value, "Assertion failed");
                    } else {
                        assert_ne!(lhs_value, rhs_value, "Assertion failed");
                    }
                }
            }
        }
    }
}

impl<T: FieldElement> InterpreterAction<T> {
    // variable indexes written by the action
    fn writes(&self) -> BTreeSet<usize> {
        let mut set = BTreeSet::new();
        match self {
            InterpreterAction::ReadCell(idx, _) => {
                set.insert(*idx);
            }
            InterpreterAction::ReadParam(idx, _) => {
                set.insert(*idx);
            }
            InterpreterAction::AssignExpression(idx, _) => {
                set.insert(*idx);
            }
            InterpreterAction::MachineCall(_, params) => params.iter().for_each(|p| {
                if let MachineCallArgumentIdx::Unknown(v) = p {
                    set.insert(*v);
                }
            }),
            _ => {}
        }
        set
    }

    // variable indexes read by the action
    fn reads(&self) -> BTreeSet<usize> {
        let mut set = BTreeSet::new();
        match self {
            InterpreterAction::WriteCell(idx, _) => {
                set.insert(*idx);
            }
            InterpreterAction::WriteParam(idx, _) => {
                set.insert(*idx);
            }
            InterpreterAction::AssignExpression(_, expr) => expr.elems.iter().for_each(|e| {
                if let RPNExpressionElem::Symbol(idx) = e {
                    set.insert(*idx);
                }
            }),
            InterpreterAction::MachineCall(_, params) => params.iter().for_each(|p| {
                if let MachineCallArgumentIdx::Known(v) = p {
                    set.insert(*v);
                }
            }),
            InterpreterAction::Assertion(lhs, rhs, _) => {
                lhs.elems.iter().for_each(|e| {
                    if let RPNExpressionElem::Symbol(idx) = e {
                        set.insert(*idx);
                    }
                });
                rhs.elems.iter().for_each(|e| {
                    if let RPNExpressionElem::Symbol(idx) = e {
                        set.insert(*idx);
                    }
                });
            }
            _ => {}
        }
        set
    }
}

/// Helper struct to map variables to unique indices, so they can be kept in
/// sequential memory and quickly refered to during execution.
pub struct VariableMapper {
    var_idx: HashMap<Variable, usize>,
    count: usize,
}

impl VariableMapper {
    pub fn new() -> Self {
        Self {
            var_idx: HashMap::new(),
            count: 0,
        }
    }

    pub fn var_count(&self) -> usize {
        self.count
    }

    pub fn map_var(&mut self, var: &Variable) -> usize {
        let idx = *self.var_idx.entry(var.clone()).or_insert_with(|| {
            self.count += 1;
            self.count - 1
        });
        idx
    }

    // reserve a new variable index
    pub fn reserve_idx(&mut self) -> usize {
        let idx = self.count;
        self.count += 1;
        idx
    }

    // get the index of a variable if it was previously mapped
    pub fn get_var(&mut self, var: &Variable) -> Option<usize> {
        self.var_idx.get(var).copied()
    }

    pub fn map_expr_to_rpn<T: FieldElement>(
        &mut self,
        expr: &SymbolicExpression<T, Variable>,
    ) -> RPNExpression<T, usize> {
        RPNExpression::from(&expr.map_variables(&mut |var| self.map_var(var)))
    }
}

/// An expression in Reverse Polish Notation.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct RPNExpression<T: FieldElement, S> {
    pub elems: Vec<RPNExpressionElem<T, S>>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub enum RPNExpressionElem<T: FieldElement, S> {
    Constant(T),
    Symbol(S),
    BinaryAdd,
    BinarySub,
    BinaryMul,
    BinaryDiv,
    BinaryIntegerDiv,
    UnaryNeg,
    UnaryBitAnd(T::Integer),
}

impl<T: FieldElement, S: Clone> From<&SymbolicExpression<T, S>> for RPNExpression<T, S> {
    fn from(expr: &SymbolicExpression<T, S>) -> Self {
        fn from_inner<T: FieldElement, S: Clone>(
            expr: &SymbolicExpression<T, S>,
            elems: &mut Vec<RPNExpressionElem<T, S>>,
        ) {
            match expr {
                SymbolicExpression::Concrete(n) => {
                    elems.push(RPNExpressionElem::Constant(*n));
                }
                SymbolicExpression::Symbol(s, _) => {
                    elems.push(RPNExpressionElem::Symbol(s.clone()));
                }
                SymbolicExpression::BinaryOperation(lhs, op, rhs, _) => {
                    from_inner(lhs, elems);
                    from_inner(rhs, elems);
                    match op {
                        BinaryOperator::Add => {
                            elems.push(RPNExpressionElem::BinaryAdd);
                        }
                        BinaryOperator::Sub => {
                            elems.push(RPNExpressionElem::BinarySub);
                        }
                        BinaryOperator::Mul => {
                            elems.push(RPNExpressionElem::BinaryMul);
                        }
                        BinaryOperator::Div => {
                            elems.push(RPNExpressionElem::BinaryDiv);
                        }
                        BinaryOperator::IntegerDiv => {
                            elems.push(RPNExpressionElem::BinaryIntegerDiv);
                        }
                    }
                }
                SymbolicExpression::UnaryOperation(op, expr, _) => {
                    from_inner(expr, elems);
                    elems.push(match op {
                        UnaryOperator::Neg => RPNExpressionElem::UnaryNeg,
                    });
                }
                SymbolicExpression::BitOperation(expr, op, n, _) => {
                    from_inner(expr, elems);
                    elems.push(match op {
                        BitOperator::And => RPNExpressionElem::UnaryBitAnd(*n),
                    });
                }
            }
        }
        let mut elems = Vec::new();
        from_inner(expr, &mut elems);
        RPNExpression { elems }
    }
}

impl<T: FieldElement> RPNExpression<T, usize> {
    /// Evaluate the expression (in three-acess code form) using the provided variables
    fn evaluate_tac(&self, vars: &[T]) -> T {
        match self.elems.len() {
            1 => match &self.elems[0] {
                RPNExpressionElem::Constant(v) => *v,
                RPNExpressionElem::Symbol(idx) => vars[*idx],
                _ => panic!("Invalid expression"),
            },
            2 => {
                let inner = match &self.elems[0] {
                    RPNExpressionElem::Constant(v) => *v,
                    RPNExpressionElem::Symbol(idx) => vars[*idx],
                    _ => panic!("Invalid expression"),
                };
                match &self.elems[1] {
                    RPNExpressionElem::UnaryNeg => -inner,
                    RPNExpressionElem::UnaryBitAnd(right) => T::from(inner.to_integer() & *right),
                    _ => panic!("Invalid expression"),
                }
            }
            3 => {
                let left = match &self.elems[0] {
                    RPNExpressionElem::Constant(v) => *v,
                    RPNExpressionElem::Symbol(idx) => vars[*idx],
                    _ => panic!("Invalid expression"),
                };
                let right = match &self.elems[1] {
                    RPNExpressionElem::Constant(v) => *v,
                    RPNExpressionElem::Symbol(idx) => vars[*idx],
                    _ => panic!("Invalid expression"),
                };
                match &self.elems[2] {
                    RPNExpressionElem::BinaryAdd => left + right,
                    RPNExpressionElem::BinarySub => left - right,
                    RPNExpressionElem::BinaryMul => left * right,
                    RPNExpressionElem::BinaryDiv => left / right,
                    RPNExpressionElem::BinaryIntegerDiv => {
                        T::from(left.to_arbitrary_integer() / right.to_arbitrary_integer())
                    }
                    _ => panic!("Invalid expression"),
                }
            }
            _ => panic!("Invalid expression"),
        }
    }
}

// the following functions come from the interface.rs file also included in the compiled jit code

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
