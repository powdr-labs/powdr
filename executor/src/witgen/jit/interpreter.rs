use super::effect::{Assertion, BranchCondition, Effect};

use super::symbolic_expression::{BinaryOperator, BitOperator, SymbolicExpression, UnaryOperator};
use super::variable::{Cell, Variable};
use crate::witgen::data_structures::finalizable_data::CompactDataRef;
use crate::witgen::data_structures::mutable_state::MutableState;
use crate::witgen::machines::LookupCell;
use crate::witgen::{FixedData, QueryCallback};

use itertools::Itertools;
use powdr_ast::analyzed::{PolyID, PolynomialType};
use powdr_number::FieldElement;

use std::cmp::Ordering;
use std::collections::{BTreeSet, HashMap};

/// Interpreter for instructions compiled from witgen effects.
pub struct EffectsInterpreter<T: FieldElement> {
    var_count: usize,
    actions: Vec<InterpreterAction<T>>,
}

/// Witgen effects compiled into instructions for a stack machine.
/// Variables have been removed and replaced by their index in the variable list.
#[derive(Debug)]
enum InterpreterAction<T: FieldElement> {
    ReadCell(usize, Cell),
    ReadParam(usize, usize),
    ReadFixedColumn(usize, Cell),
    AssignExpression(usize, RPNExpression<T, usize>),
    WriteCell(usize, Cell),
    WriteParam(usize, usize),
    MachineCall(T, Vec<MachineCallArgumentIdx>),
    Assertion(RPNExpression<T, usize>, RPNExpression<T, usize>, bool),
    Branch(
        BranchTest<T>,
        Vec<InterpreterAction<T>>,
        Vec<InterpreterAction<T>>,
    ),
}

#[derive(Debug)]
enum BranchTest<T: FieldElement> {
    Equal { var: usize, value: T },
    Inside { var: usize, min: T, max: T },
    Outside { var: usize, min: T, max: T },
}

impl<T: FieldElement> BranchTest<T> {
    fn new(
        var_mapper: &mut VariableMapper,
        BranchCondition {
            variable,
            condition,
        }: &BranchCondition<T, Variable>,
    ) -> Self {
        let (min, max) = condition.range();
        let var = var_mapper.map_var(variable);
        match min.cmp(&max) {
            Ordering::Equal => BranchTest::Equal { var, value: min },
            Ordering::Less => BranchTest::Inside { var, min, max },
            Ordering::Greater => BranchTest::Outside { var, min, max },
        }
    }

    fn var(&self) -> usize {
        match self {
            BranchTest::Equal { var, .. }
            | BranchTest::Inside { var, .. }
            | BranchTest::Outside { var, .. } => *var,
        }
    }

    fn test(&self, vars: &[T]) -> bool {
        match self {
            BranchTest::Equal { var, value } => vars[*var] == *value,
            BranchTest::Inside { var, min, max } => *min <= vars[*var] && vars[*var] <= *max,
            BranchTest::Outside { var, min, max } => vars[*var] <= *min || vars[*var] >= *max,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd)]
enum MachineCallArgumentIdx {
    /// var index of the evaluated known argument expression
    Known(usize),
    /// var index of the unknown
    Unknown(usize),
}

impl<T: FieldElement> EffectsInterpreter<T> {
    pub fn try_new(known_inputs: &[Variable], effects: &[Effect<T, Variable>]) -> Option<Self> {
        // TODO: interpreter doesn't support prover functions yet
        fn has_prover_fn<T: FieldElement>(effect: &Effect<T, Variable>) -> bool {
            match effect {
                Effect::ProverFunctionCall(..) => true,
                Effect::Branch(_, if_branch, else_branch) => {
                    if if_branch.iter().any(has_prover_fn) || else_branch.iter().any(has_prover_fn)
                    {
                        return true;
                    }
                    false
                }
                _ => false,
            }
        }
        if effects.iter().any(has_prover_fn) {
            return None;
        }

        let mut actions = vec![];
        let mut var_mapper = VariableMapper::new();

        actions.extend(Self::load_fixed_column_values(&mut var_mapper, effects));
        actions.extend(Self::load_known_inputs(&mut var_mapper, known_inputs));
        actions.extend(Self::process_effects(&mut var_mapper, effects));
        actions.extend(Self::write_data(&mut var_mapper, effects));

        let ret = Self {
            var_count: var_mapper.var_count(),
            actions,
        };
        assert!(actions_are_valid(&ret.actions, BTreeSet::new()));
        Some(ret)
    }

    /// Returns an iterator of actions to load all accessed fixed column values into variables.
    fn load_fixed_column_values<'a>(
        var_mapper: &'a mut VariableMapper,
        effects: &'a [Effect<T, Variable>],
    ) -> impl Iterator<Item = InterpreterAction<T>> + 'a {
        effects
            .iter()
            .flat_map(|e| e.referenced_variables())
            .filter_map(|v| match v {
                Variable::FixedCell(c) => Some((v, c)),
                _ => None,
            })
            .unique()
            .map(|(var, cell)| {
                let idx = var_mapper.map_var(var);
                InterpreterAction::ReadFixedColumn(idx, cell.clone())
            })
    }

    /// Returns an iterator of actions to load all known inputs into variables.
    fn load_known_inputs<'a>(
        var_mapper: &'a mut VariableMapper,
        known_inputs: &'a [Variable],
    ) -> impl Iterator<Item = InterpreterAction<T>> + 'a {
        known_inputs.iter().map(|var| match var {
            Variable::WitnessCell(c) => {
                let idx = var_mapper.map_var(var);
                InterpreterAction::ReadCell(idx, c.clone())
            }
            Variable::Param(i) => {
                let idx = var_mapper.map_var(var);
                InterpreterAction::ReadParam(idx, *i)
            }
            Variable::FixedCell(_)
            | Variable::MachineCallParam(_)
            | Variable::IntermediateCell(_) => unreachable!(),
        })
    }

    /// Returns an iterator of actions equivalent to the effects.
    fn process_effects<'a>(
        var_mapper: &'a mut VariableMapper,
        effects: &'a [Effect<T, Variable>],
    ) -> impl Iterator<Item = InterpreterAction<T>> + 'a {
        effects.iter().map(|effect| {
            match effect {
                Effect::Assignment(var, e) => {
                    let idx = var_mapper.map_var(var);
                    InterpreterAction::AssignExpression(idx, var_mapper.map_expr_to_rpn(e))
                }
                Effect::RangeConstraint(..) => {
                    unreachable!("Final code should not contain pure range constraints.")
                }
                Effect::Assertion(Assertion {
                    lhs,
                    rhs,
                    expected_equal,
                }) => InterpreterAction::Assertion(
                    var_mapper.map_expr_to_rpn(lhs),
                    var_mapper.map_expr_to_rpn(rhs),
                    *expected_equal,
                ),
                Effect::MachineCall(id, known_inputs, arguments) => {
                    let arguments = known_inputs
                        .iter()
                        .zip(arguments)
                        .map(|(is_input, var)| {
                            if is_input {
                                MachineCallArgumentIdx::Known(var_mapper.map_var(var))
                            } else {
                                MachineCallArgumentIdx::Unknown(var_mapper.map_var(var))
                            }
                        })
                        .collect();
                    InterpreterAction::MachineCall(*id, arguments)
                }
                Effect::ProverFunctionCall(..) => {
                    // TODO We cannot compile them here, but we should be able to use the PIL evaluator.
                    unimplemented!("Prover function calls are not supported in the interpreter yet")
                }
                Effect::Branch(condition, if_branch, else_branch) => {
                    let if_actions = Self::process_effects(var_mapper, if_branch).collect();
                    let else_actions = Self::process_effects(var_mapper, else_branch).collect();
                    let test = BranchTest::new(var_mapper, condition);
                    InterpreterAction::Branch(test, if_actions, else_actions)
                }
            }
        })
    }

    /// Returns an iterator of actions to write all written variables to the data.
    fn write_data<'a>(
        var_mapper: &'a mut VariableMapper,
        effects: &'a [Effect<T, Variable>],
    ) -> impl Iterator<Item = InterpreterAction<T>> + 'a {
        effects
            .iter()
            .flat_map(Effect::written_vars)
            .filter_map(|(var, _mutable)| {
                match var {
                    Variable::WitnessCell(cell) => {
                        let idx = var_mapper.get_var(var).unwrap();
                        Some(InterpreterAction::WriteCell(idx, cell.clone()))
                    }
                    Variable::Param(i) => {
                        let idx = var_mapper.get_var(var).unwrap();
                        Some(InterpreterAction::WriteParam(idx, *i))
                    }
                    Variable::FixedCell(_) => panic!("Should not write to fixed column."),
                    Variable::IntermediateCell(_) => {
                        // Intermediate cells are not stored permanently
                        None
                    }
                    Variable::MachineCallParam(_) => {
                        // This is just an internal variable.
                        None
                    }
                }
            })
    }

    /// Execute the machine effects for the given the parameters
    pub fn call<Q: QueryCallback<T>>(
        &self,
        fixed_data: &FixedData<'_, T>,
        mutable_state: &MutableState<'_, T, Q>,
        params: &mut [LookupCell<T>],
        data: CompactDataRef<'_, T>,
    ) {
        let mut vars = vec![T::zero(); self.var_count];

        let row_offset: i64 = data.row_offset.try_into().unwrap();
        let mut eval_stack = vec![];
        let mut block_stack = vec![self.actions.iter()];
        // - while there are blocks on the stack:
        //   - pop a block (an action iterator) and iterate over the actions
        //     - in case of a branch action:
        //       - push the current iterator back on the stack
        //       - test the condition
        //       - push the if or else block on the stack
        //       - break the current block execution loop
        while let Some(mut iter) = block_stack.pop() {
            while let Some(action) = iter.next() {
                match action {
                    InterpreterAction::AssignExpression(idx, e) => {
                        let val = e.evaluate(&mut eval_stack, &vars[..]);
                        vars[*idx] = val;
                    }
                    InterpreterAction::ReadCell(idx, c) => {
                        vars[*idx] = data
                            .data
                            .get((row_offset + c.row_offset as i64).try_into().unwrap(), c.id)
                            .0;
                    }
                    InterpreterAction::ReadFixedColumn(idx, c) => {
                        let poly_id = PolyID {
                            id: c.id,
                            ptype: PolynomialType::Constant,
                        };
                        vars[*idx] = fixed_data.fixed_cols[&poly_id].values_max_size()
                            [usize::try_from(row_offset + c.row_offset as i64).unwrap()];
                    }
                    InterpreterAction::ReadParam(idx, i) => {
                        vars[*idx] = get_param(params, *i);
                    }
                    InterpreterAction::WriteCell(idx, c) => {
                        data.data.set(
                            (row_offset + c.row_offset as i64).try_into().unwrap(),
                            c.id,
                            vars[*idx],
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
                        let lhs_value = e1.evaluate(&mut eval_stack, &vars);
                        let rhs_value = e2.evaluate(&mut eval_stack, &vars);
                        if *expected_equal {
                            assert_eq!(lhs_value, rhs_value, "Assertion failed");
                        } else {
                            assert_ne!(lhs_value, rhs_value, "Assertion failed");
                        }
                    }
                    InterpreterAction::Branch(condition, if_branch, else_branch) => {
                        // push the current block on the stack to continue execution once the branch is done
                        block_stack.push(iter);
                        // test the condition
                        block_stack.push(if condition.test(&vars) {
                            if_branch.iter()
                        } else {
                            else_branch.iter()
                        });
                        // stop the currently executing block
                        break;
                    }
                }
            }
        }
        assert!(eval_stack.is_empty());
    }
}

/// Check if an action is valid: it doesn't overwrite a variable and doesn't read it before it's been written to
fn action_is_valid<T: FieldElement>(
    action: &InterpreterAction<T>,
    prev_writes: &BTreeSet<usize>,
) -> bool {
    if let InterpreterAction::Branch(cond, if_actions, else_actions) = action {
        actions_are_valid(if_actions, prev_writes.clone())
            && actions_are_valid(else_actions, prev_writes.clone())
            && prev_writes.contains(&cond.var())
    } else {
        action.writes().is_disjoint(prev_writes) && action.reads().is_subset(prev_writes)
    }
}

fn actions_are_valid<T: FieldElement>(
    actions: &Vec<InterpreterAction<T>>,
    mut prev_writes: BTreeSet<usize>,
) -> bool {
    for action in actions {
        if !action_is_valid(action, &prev_writes) {
            return false;
        }
        prev_writes.extend(action.writes());
    }
    true
}

impl<T: FieldElement> InterpreterAction<T> {
    /// variable indexes written by the action
    fn writes(&self) -> BTreeSet<usize> {
        let mut set = BTreeSet::new();
        match self {
            InterpreterAction::ReadCell(idx, _)
            | InterpreterAction::ReadParam(idx, _)
            | InterpreterAction::AssignExpression(idx, _) => {
                set.insert(*idx);
            }
            InterpreterAction::MachineCall(_, params) => params.iter().for_each(|p| {
                if let MachineCallArgumentIdx::Unknown(v) = p {
                    set.insert(*v);
                }
            }),
            InterpreterAction::Branch(_branch_test, if_actions, else_actions) => {
                set.extend(
                    if_actions
                        .iter()
                        .chain(else_actions)
                        .flat_map(InterpreterAction::writes),
                );
            }
            InterpreterAction::WriteCell(_, _)
            | InterpreterAction::WriteParam(_, _)
            | InterpreterAction::ReadFixedColumn(_, _)
            | InterpreterAction::Assertion(..) => {}
        }
        set
    }

    /// variable indexes read by the action
    fn reads(&self) -> BTreeSet<usize> {
        let mut set = BTreeSet::new();
        match self {
            InterpreterAction::WriteCell(idx, _) | InterpreterAction::WriteParam(idx, _) => {
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
            InterpreterAction::Branch(branch_test, if_actions, else_actions) => {
                set.insert(branch_test.var());

                set.extend(
                    if_actions
                        .iter()
                        .chain(else_actions)
                        .flat_map(InterpreterAction::reads),
                );
            }
            InterpreterAction::ReadCell(_, _)
            | InterpreterAction::ReadParam(_, _)
            | InterpreterAction::ReadFixedColumn(_, _) => {}
        }
        set
    }
}

/// Helper struct to map variables to contiguous indices, so they can be kept in
/// sequential memory and quickly refered to during execution.
struct VariableMapper {
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

    /// Returns the index of the variable, allocates it if it does not exist.
    pub fn map_var(&mut self, var: &Variable) -> usize {
        let idx = *self.var_idx.entry(var.clone()).or_insert_with(|| {
            self.count += 1;
            self.count - 1
        });
        idx
    }

    /// get the index of a variable if it was previously mapped
    pub fn get_var(&mut self, var: &Variable) -> Option<usize> {
        self.var_idx.get(var).copied()
    }

    pub fn map_expr_to_rpn<T: FieldElement>(
        &mut self,
        expr: &SymbolicExpression<T, Variable>,
    ) -> RPNExpression<T, usize> {
        RPNExpression::map_from(expr, self)
    }
}

/// An expression in Reverse Polish Notation.
#[derive(Debug)]
struct RPNExpression<T: FieldElement, S> {
    pub elems: Vec<RPNExpressionElem<T, S>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum RPNExpressionElem<T: FieldElement, S> {
    Concrete(T),
    Symbol(S),
    BinaryOperation(BinaryOperator),
    UnaryOperation(UnaryOperator),
    BitOperation(BitOperator, T::Integer),
}

impl<T: FieldElement> RPNExpression<T, usize> {
    /// Convert a symbolic expression to RPN, mapping variables to indices
    fn map_from(expr: &SymbolicExpression<T, Variable>, var_mapper: &mut VariableMapper) -> Self {
        fn inner<T: FieldElement>(
            expr: &SymbolicExpression<T, Variable>,
            elems: &mut Vec<RPNExpressionElem<T, usize>>,
            var_mapper: &mut VariableMapper,
        ) {
            match expr {
                SymbolicExpression::Concrete(n) => {
                    elems.push(RPNExpressionElem::Concrete(*n));
                }
                SymbolicExpression::Symbol(s, _) => {
                    elems.push(RPNExpressionElem::Symbol(var_mapper.map_var(s)));
                }
                SymbolicExpression::BinaryOperation(lhs, op, rhs, _) => {
                    inner(lhs, elems, var_mapper);
                    inner(rhs, elems, var_mapper);
                    elems.push(RPNExpressionElem::BinaryOperation(op.clone()));
                }
                SymbolicExpression::UnaryOperation(op, expr, _) => {
                    inner(expr, elems, var_mapper);
                    elems.push(RPNExpressionElem::UnaryOperation(op.clone()));
                }
                SymbolicExpression::BitOperation(expr, op, n, _) => {
                    inner(expr, elems, var_mapper);
                    elems.push(RPNExpressionElem::BitOperation(op.clone(), *n));
                }
            }
        }
        let mut elems = Vec::new();
        inner(expr, &mut elems, var_mapper);
        RPNExpression { elems }
    }

    /// Evaluate the expression using the provided variables.
    /// The stack is used to store intermediate results, it's taken as
    /// a parameter to avoid allocating on every call to evaluate.
    fn evaluate(&self, stack: &mut Vec<T>, vars: &[T]) -> T {
        self.elems.iter().for_each(|elem| match elem {
            RPNExpressionElem::Concrete(v) => stack.push(*v),
            RPNExpressionElem::Symbol(idx) => stack.push(vars[*idx]),
            RPNExpressionElem::BinaryOperation(op) => {
                let right = stack.pop().unwrap();
                let left = stack.pop().unwrap();
                let result = match op {
                    BinaryOperator::Add => left + right,
                    BinaryOperator::Sub => left - right,
                    BinaryOperator::Mul => left * right,
                    BinaryOperator::Div => left / right,
                    BinaryOperator::IntegerDiv => {
                        T::from(left.to_arbitrary_integer() / right.to_arbitrary_integer())
                    }
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
        });
        stack.pop().unwrap()
    }
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

#[cfg(test)]
mod test {
    use std::fs::read_to_string;

    use super::*;
    use crate::witgen::data_structures::{
        finalizable_data::{CompactData, CompactDataRef},
        mutable_state::MutableState,
    };
    use crate::witgen::global_constraints;
    use crate::witgen::jit::block_machine_processor::BlockMachineProcessor;
    use crate::witgen::jit::effect::Effect;
    use crate::witgen::jit::test_util::read_pil;
    use crate::witgen::jit::variable::Variable;
    use crate::witgen::machines::{
        machine_extractor::MachineExtractor, KnownMachine, LookupCell, Machine,
    };
    use crate::witgen::FixedData;

    use powdr_ast::analyzed::Analyzed;

    use pretty_assertions::assert_eq;
    use test_log::test;

    use bit_vec::BitVec;
    use itertools::Itertools;
    use powdr_number::{FieldElement, GoldilocksField};

    struct TestInterpreter<'a, T: FieldElement, Q: QueryCallback<T>> {
        analyzed: &'a Analyzed<T>,
        num_inputs: usize,
        num_outputs: usize,
        fixed_data: &'a FixedData<'a, T>,
        mutable_state: MutableState<'a, T, Q>,
        block_size: usize,
        interpreter: EffectsInterpreter<T>,
        code: Vec<Effect<T, Variable>>,
    }

    impl<'a, T: FieldElement, Q: QueryCallback<T>> TestInterpreter<'a, T, Q> {
        pub fn new(
            analyzed: &'a Analyzed<T>,
            fixed_data: &'a FixedData<'a, T>,
            machine_name: &str,
            num_inputs: usize,
            num_outputs: usize,
            query_callback: &'a Q,
        ) -> Self {
            let machines: Vec<KnownMachine<'a, _>> =
                MachineExtractor::new(fixed_data).split_out_machines();
            let [KnownMachine::BlockMachine(machine)] = machines
                .iter()
                .filter(|m| m.name().contains(machine_name))
                .collect_vec()
                .as_slice()
            else {
                panic!("Expected exactly one matching block machine")
            };
            let (machine_parts, block_size, latch_row) = machine.machine_info();

            let processor = BlockMachineProcessor::new(
                fixed_data,
                machine_parts.clone(),
                block_size,
                latch_row,
            );

            let mutable_state: MutableState<'a, _, Q> =
                MutableState::new(machines.into_iter(), query_callback);

            // generate code for the call
            assert_eq!(machine_parts.bus_receives.len(), 1);
            let bus_id = *machine_parts.bus_receives.keys().next().unwrap();
            let known_values = BitVec::from_iter(
                (0..num_inputs)
                    .map(|_| true)
                    .chain((0..num_outputs).map(|_| false)),
            );
            let known_inputs = (0..num_inputs).map(Variable::Param).collect::<Vec<_>>();

            let (result, _prover_functions) = processor
                .generate_code(&mutable_state, bus_id, &known_values, None)
                .unwrap();

            // generate and call the interpreter
            let interpreter = EffectsInterpreter::try_new(&known_inputs, &result.code).unwrap();

            Self {
                analyzed,
                num_inputs,
                num_outputs,
                fixed_data,
                mutable_state,
                block_size,
                interpreter,
                code: result.code,
            }
        }

        pub fn code(&self) -> &Vec<Effect<T, Variable>> {
            &self.code
        }

        pub fn test(&self, params: &[u64], expected_out: &[u64]) {
            assert_eq!(params.len(), self.num_inputs + self.num_outputs);
            assert_eq!(expected_out.len(), self.num_inputs + self.num_outputs);
            let mut params = params.iter().map(|v| T::from(*v)).collect::<Vec<_>>();
            let expected_out = expected_out.iter().map(|v| T::from(*v)).collect::<Vec<_>>();
            let poly_ids = self
                .analyzed
                .committed_polys_in_source_order()
                .flat_map(|p| p.0.array_elements().map(|e| e.1))
                .collect_vec();
            let mut data = CompactData::new(poly_ids.iter());
            data.append_new_rows(self.block_size);

            let data_ref = CompactDataRef::new(&mut data, 0);
            let mut param_lookups = params
                .iter_mut()
                .enumerate()
                .map(|(i, p)| {
                    if i < self.num_inputs {
                        LookupCell::Input(p)
                    } else {
                        LookupCell::Output(p)
                    }
                })
                .collect::<Vec<_>>();
            self.interpreter.call(
                self.fixed_data,
                &self.mutable_state,
                param_lookups.as_mut_slice(),
                data_ref,
            );

            assert_eq!(params, expected_out);
        }
    }

    #[test]
    fn branching() {
        let pil = r"
namespace main(128);
    col witness a, b, add, mul, sub, res;
    [a, b, add, mul, sub, res] is [arith::a, arith::b, arith::add, arith::mul, arith::sub, arith::res];

namespace arith(8);
    let a;
    let b;
    let add;
    let mul;
    let sub;
    let res;

    add + mul + sub = 1;

    add * (1 - add) = 0;
    mul * (1 - mul) = 0;
    sub * (1 - sub) = 0;

    add * (res - (a + b)) + mul * (res - (a * b)) + sub * (res - (a - b)) = 0;

    ";
        let (analyzed, fixed_col_vals) = read_pil::<GoldilocksField>(pil);
        let fixed_data = FixedData::new(&analyzed, &fixed_col_vals, &[], Default::default(), 0);
        let fixed_data = global_constraints::set_global_constraints(fixed_data);
        let interpreter = TestInterpreter::new(&analyzed, &fixed_data, "arith", 5, 1, &|_| {
            Err("Query not implemented".to_string())
        });
        // ensure there's a branch in the code
        assert!(interpreter
            .code()
            .iter()
            .any(|a| matches!(a, Effect::Branch(..))));

        // 2 + 3 = 5
        interpreter.test(&[2, 3, 1, 0, 0, 0], &[2, 3, 1, 0, 0, 5]);
        // 2 * 3 = 6
        interpreter.test(&[2, 3, 0, 1, 0, 0], &[2, 3, 0, 1, 0, 6]);
        // 3 - 2 = 1
        interpreter.test(&[3, 2, 0, 0, 1, 0], &[3, 2, 0, 0, 1, 1]);
    }

    #[test]
    fn call_poseidon() {
        let pil = read_to_string("../test_data/pil/poseidon_gl.pil").unwrap();
        let (analyzed, fixed_col_vals) = read_pil::<GoldilocksField>(&pil);
        let fixed_data = FixedData::new(&analyzed, &fixed_col_vals, &[], Default::default(), 0);
        let fixed_data = global_constraints::set_global_constraints(fixed_data);
        let interpreter =
            TestInterpreter::new(&analyzed, &fixed_data, "main_poseidon", 12, 4, &|_| {
                Err("Query not implemented".to_string())
            });

        interpreter.test(
            &[0; 16],
            &[
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
                4330397376401421145u64,
                14124799381142128323u64,
                8742572140681234676u64,
                14345658006221440202u64,
            ],
        );
    }
}
