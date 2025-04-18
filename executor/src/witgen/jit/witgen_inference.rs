use std::{
    collections::{HashMap, HashSet},
    fmt::{Display, Formatter},
};

use bit_vec::BitVec;
use itertools::Itertools;
use powdr_ast::analyzed::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression as Expression,
    AlgebraicReference, AlgebraicUnaryOperation, AlgebraicUnaryOperator,
};
use powdr_number::FieldElement;

use crate::witgen::{
    data_structures::{
        identity::{BusSend, Identity},
        mutable_state::MutableState,
    },
    global_constraints::RangeConstraintSet,
    range_constraints::RangeConstraint,
    FixedData, QueryCallback,
};

use super::{
    affine_symbolic_expression::{AffineSymbolicExpression, Error, ProcessResult},
    effect::{BranchCondition, Effect, ProverFunctionCall},
    prover_function_heuristics::ProverFunction,
    symbolic_expression::SymbolicExpression,
    variable::{Cell, MachineCallVariable, Variable},
};

/// This component can generate code that solves identities.
/// It needs a driver that tells it which identities to process on which rows.
#[derive(Clone)]
pub struct WitgenInference<'a, T: FieldElement, FixedEval> {
    fixed_data: &'a FixedData<'a, T>,
    fixed_evaluator: FixedEval,
    /// Sequences of branches taken in the past to get to the current state.
    branches_taken: Vec<(Variable, RangeConstraint<T>)>,
    derived_range_constraints: HashMap<Variable, RangeConstraint<T>>,
    known_variables: HashSet<Variable>,
    /// Submachine calls that have already been completed.
    /// These are still processed to propagate range constraints
    /// and concrete values.
    /// This avoids generating multiple submachine calls for the same
    /// connection on the same row.
    complete_calls: HashSet<(u64, i32)>,
    code: Vec<Effect<T, Variable>>,
}

#[derive(Debug, Clone, Copy)]
pub enum Value<T> {
    Concrete(T),
    Known,
    Unknown,
}

impl<T: Display> Display for Value<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Concrete(v) => write!(f, "{v}"),
            Value::Known => write!(f, "<known>"),
            Value::Unknown => write!(f, "???"),
        }
    }
}

/// Return type of the `branch_on` method.
pub struct BranchResult<'a, T: FieldElement, FixedEval> {
    /// The code common to both branches.
    pub common_code: Vec<Effect<T, Variable>>,
    /// The condition of the branch.
    pub condition: BranchCondition<T, Variable>,
    /// The two branches.
    pub branches: [WitgenInference<'a, T, FixedEval>; 2],
}

impl<'a, T: FieldElement, FixedEval: FixedEvaluator<T>> WitgenInference<'a, T, FixedEval> {
    pub fn new(
        fixed_data: &'a FixedData<'a, T>,
        fixed_evaluator: FixedEval,
        known_variables: impl IntoIterator<Item = Variable>,
        complete_calls: impl IntoIterator<Item = (u64, i32)>,
    ) -> Self {
        Self {
            fixed_data,
            fixed_evaluator,
            branches_taken: Default::default(),
            derived_range_constraints: Default::default(),
            known_variables: known_variables.into_iter().collect(),
            complete_calls: complete_calls.into_iter().collect(),
            code: Default::default(),
        }
    }

    pub fn finish(self) -> Vec<Effect<T, Variable>> {
        self.code
    }

    pub fn code(&self) -> &Vec<Effect<T, Variable>> {
        &self.code
    }

    pub fn branches_taken(&self) -> &[(Variable, RangeConstraint<T>)] {
        &self.branches_taken
    }

    pub fn known_variables(&self) -> &HashSet<Variable> {
        &self.known_variables
    }

    pub fn is_known(&self, variable: &Variable) -> bool {
        if let Variable::FixedCell(_) = variable {
            true
        } else {
            self.known_variables.contains(variable)
        }
    }

    pub fn is_complete_call(&self, identity: &Identity<T>, row_offset: i32) -> bool {
        assert!(matches!(identity, Identity::BusSend(_)));
        self.complete_calls.contains(&(identity.id(), row_offset))
    }

    pub fn value(&self, variable: &Variable) -> Value<T> {
        let rc = self.range_constraint(variable);
        if let Some(val) = rc.try_to_single_value() {
            Value::Concrete(val)
        } else if self.is_known(variable) {
            Value::Known
        } else {
            Value::Unknown
        }
    }

    /// Splits the current inference into two copies - one where the provided variable
    /// is in the "second half" of its range constraint and one where it is in the
    /// "first half" of its range constraint (determined by calling the `bisect` method).
    /// Returns the common code, the branch condition and the two branches.
    pub fn branch_on(mut self, variable: &Variable) -> BranchResult<'a, T, FixedEval> {
        // The variable needs to be known, we need to have a range constraint but
        // it cannot be a single value.
        assert!(self.is_known(variable));
        let rc = self.range_constraint(variable);
        assert!(rc.try_to_single_value().is_none());

        let (low_condition, high_condition) = rc.bisect();

        let common_code = std::mem::take(&mut self.code);
        let mut low_branch = self.clone();

        self.branch_to(variable, high_condition.clone());
        low_branch.branch_to(variable, low_condition.clone());

        BranchResult {
            common_code,
            condition: BranchCondition {
                value: SymbolicExpression::from_symbol(variable.clone(), rc),
                condition: high_condition,
            },
            branches: [self, low_branch],
        }
    }

    fn branch_to(&mut self, var: &Variable, range_constraint: RangeConstraint<T>) {
        self.branches_taken
            .push((var.clone(), range_constraint.clone()));
        self.add_range_constraint(var.clone(), range_constraint);
    }

    pub fn process_call(
        &mut self,
        can_process_call: impl CanProcessCall<T>,
        bus_send: &BusSend<T>,
        row_offset: i32,
    ) -> Result<Vec<Variable>, Error> {
        let result = self.process_call_inner(can_process_call, bus_send, row_offset);
        self.ingest_effects(result, Some((bus_send.identity_id, row_offset)))
    }

    /// Process a prover function on a row, i.e. determine if we can execute it and if it will
    /// help us to compute the value of previously unknown variables.
    /// Returns the list of updated variables.
    pub fn process_prover_function(
        &mut self,
        prover_function: &ProverFunction<'a, T>,
        row_offset: i32,
    ) -> Result<Vec<Variable>, Error> {
        let targets = prover_function
            .target
            .iter()
            .map(|t| Variable::from_reference(t, row_offset))
            .collect::<Vec<_>>();
        // Only continue if none of the targets are known.
        if targets.iter().any(|t| self.is_known(t)) {
            return Ok(vec![]);
        }
        let inputs = prover_function
            .input_columns
            .iter()
            .map(|c| Variable::from_reference(c, row_offset))
            .collect::<Vec<_>>();
        if !inputs.iter().all(|v| self.is_known(v)) {
            return Ok(vec![]);
        }

        // If there is a condition, only continue if the constraint
        // is known to hold.
        if let Some(condition) = prover_function.condition.as_ref() {
            if self.try_evaluate_to_known_number(condition, row_offset) != Some(T::zero()) {
                return Ok(vec![]);
            }
        }

        let effect = Effect::ProverFunctionCall(ProverFunctionCall {
            targets,
            function_index: prover_function.index,
            row_offset,
            inputs,
        });
        self.ingest_effects(
            ProcessResult {
                effects: vec![effect],
                complete: true,
            },
            None,
        )
    }

    /// Set a variable to a fixed value.
    pub fn set_variable(&mut self, variable: Variable, value: T) -> Result<Vec<Variable>, Error> {
        self.process_equation_on_row(
            &Expression::Number(0.into()),
            Some(variable),
            -value,
            // The row does not matter because the algebraic expression is empty
            0,
        )
    }

    /// Processes the equality constraint `lhs = variable + offset` with row offset `row_offset`.
    /// If `variable` is `None`, it is treated as zero.
    /// If this returns an error, it means we have conflicting constraints.
    pub fn process_equation_on_row(
        &mut self,
        lhs: &Expression<T>,
        variable: Option<Variable>,
        offset: T,
        row_offset: i32,
    ) -> Result<Vec<Variable>, Error> {
        // Try to find a new assignment to a variable in the equality.
        let mut result = self.process_equation_on_row_using_evaluator(
            lhs,
            variable.clone(),
            offset,
            row_offset,
            false,
        )?;
        // Try to propagate range constraints.
        let result_concrete =
            self.process_equation_on_row_using_evaluator(lhs, variable, offset, row_offset, true)?;

        // We only use the effects of the second evaluation,
        // its `complete` flag is ignored.
        result.effects.extend(result_concrete.effects);

        self.ingest_effects(result, None)
    }

    /// Processes the equality constraint `lhs = variable + offset`.
    /// If `variable` is `None`, it is treated as zero.
    /// If `only_concrete_known` is true, then only variables that are known to have a fixed value
    /// are considered known.
    /// If this returns an error, it means we have conflicting constraints.
    fn process_equation_on_row_using_evaluator(
        &self,
        lhs: &Expression<T>,
        variable: Option<Variable>,
        offset: T,
        row_offset: i32,
        only_concrete_known: bool,
    ) -> Result<ProcessResult<T, Variable>, Error> {
        let evaluator = if only_concrete_known {
            Evaluator::new(self).only_concrete_known()
        } else {
            Evaluator::new(self)
        };
        let Some(lhs_evaluated) = evaluator.evaluate(lhs, row_offset) else {
            return Ok(ProcessResult::empty());
        };
        let variable = variable
            .map(|v| evaluator.evaluate_variable(v.clone()))
            .unwrap_or_default();

        (lhs_evaluated - variable - offset.into()).solve()
    }

    fn process_call_inner(
        &mut self,
        can_process_call: impl CanProcessCall<T>,
        bus_send: &BusSend<T>,
        row_offset: i32,
    ) -> ProcessResult<T, Variable> {
        // We need to know the selector and bus ID.
        let (Some(selector), Some(bus_id)) = (
            self.try_evaluate_to_known_number(&bus_send.selected_payload.selector, row_offset),
            self.try_evaluate_to_known_number(&bus_send.bus_id, row_offset),
        ) else {
            return ProcessResult::empty();
        };
        if selector == 0.into() {
            return ProcessResult {
                effects: vec![],
                complete: true,
            };
        } else {
            assert_eq!(selector, 1.into(), "Selector is non-binary");
        }

        let arguments = (0..bus_send.selected_payload.expressions.len())
            .map(|index| {
                Variable::MachineCallParam(MachineCallVariable {
                    identity_id: bus_send.identity_id,
                    row_offset,
                    index,
                })
            })
            .collect_vec();
        let range_constraints = arguments
            .iter()
            .map(|v| self.range_constraint(v))
            .collect_vec();
        let known: BitVec = arguments.iter().map(|v| self.is_known(v)).collect();

        let (can_process, range_constraints) =
            can_process_call.can_process_call_fully(bus_id, &known, range_constraints);

        let mut effects = arguments
            .iter()
            .zip_eq(range_constraints)
            .map(|(var, new_rc)| Effect::RangeConstraint(var.clone(), new_rc.clone()))
            .collect_vec();
        if can_process {
            effects.push(Effect::MachineCall(bus_id, known, arguments.to_vec()));
        }
        ProcessResult {
            effects,
            complete: can_process,
        }
    }

    /// Analyze the effects and update the internal state.
    /// If the effect is the result of a machine call, `identity_id` must be given
    /// to avoid two calls to the same sub-machine on the same row.
    /// Returns the variables that have been updated.
    fn ingest_effects(
        &mut self,
        process_result: ProcessResult<T, Variable>,
        identity_id: Option<(u64, i32)>,
    ) -> Result<Vec<Variable>, Error> {
        let mut updated_variables = vec![];
        for e in process_result.effects {
            match &e {
                Effect::Assignment(variable, assignment) => {
                    // If the variable was determined to be a constant, we add this
                    // as a range constraint, so we can use it in future evaluations.
                    if self.add_range_constraint(variable.clone(), assignment.range_constraint()) {
                        updated_variables.push(variable.clone());
                    }
                    if self.record_known(variable.clone()) {
                        log::trace!("{variable} := {assignment}");
                        updated_variables.push(variable.clone());
                        self.code.push(e);
                    }
                }
                Effect::BitDecomposition(bit_decomp) => {
                    let mut something_updated = false;
                    for c in &bit_decomp.components {
                        if self.record_known(c.variable.clone()) {
                            updated_variables.push(c.variable.clone());
                            something_updated = true;
                        }
                    }
                    if something_updated {
                        log::trace!("{bit_decomp}");
                        self.code.push(e);
                    }
                }
                Effect::ProverFunctionCall(ProverFunctionCall {
                    targets,
                    function_index,
                    row_offset,
                    inputs,
                }) => {
                    let mut some_known = false;
                    for t in targets {
                        if self.record_known(t.clone()) {
                            some_known = true;
                            updated_variables.push(t.clone());
                        }
                    }
                    if some_known {
                        log::trace!(
                            "[{}] := prover_function_{function_index}({row_offset}, {})",
                            targets.iter().format(", "),
                            inputs.iter().format(", ")
                        );

                        self.code.push(e);
                    }
                }
                Effect::RangeConstraint(variable, rc) => {
                    if self.add_range_constraint(variable.clone(), rc.clone()) {
                        log::trace!("{variable}: {rc}");
                        updated_variables.push(variable.clone());
                    }
                }
                Effect::MachineCall(_, _, vars) => {
                    // If the machine call is already complete, it means that we should
                    // not create another submachine call. We might still process it
                    // multiple times to get better range constraints.
                    if self.complete_calls.insert(identity_id.unwrap()) {
                        log::trace!("Machine call: {:?}", identity_id.unwrap());
                        assert!(process_result.complete);
                        for v in vars {
                            // Inputs are already known, but it does not hurt to add all of them.
                            if self.record_known(v.clone()) {
                                updated_variables.push(v.clone());
                            }
                        }
                        self.code.push(e);
                    }
                }
                Effect::Assertion(_) => self.code.push(e),
                Effect::Branch(..) => unreachable!(),
            }
        }
        if process_result.complete {
            // If a machine call is complete because its selector is zero,
            // we will not get an `Effect::MachineCall` above and need to
            // insert here.
            if let Some((identity_id, row_offset)) = identity_id {
                self.complete_calls.insert((identity_id, row_offset));
            }
        }
        Ok(updated_variables)
    }

    /// Adds a range constraint to the set of derived range constraints. Returns true if progress was made.
    fn add_range_constraint(&mut self, variable: Variable, rc: RangeConstraint<T>) -> bool {
        let old_rc = self.range_constraint(&variable);
        let rc = old_rc.conjunction(&rc);
        if rc == old_rc {
            return false;
        }
        if let Some(v) = rc.try_to_single_value() {
            // Special case: Variable is fixed to a constant by range constraints only.
            if self.record_known(variable.clone()) {
                log::trace!("{variable} := {v}");
                self.code
                    .push(Effect::Assignment(variable.clone(), v.into()));
            }
        }
        self.derived_range_constraints.insert(variable.clone(), rc);
        true
    }

    /// Record a variable as known. Return true if it was not known before.
    fn record_known(&mut self, variable: Variable) -> bool {
        // We do not record fixed columns as known.
        if matches!(variable, Variable::FixedCell(_)) {
            false
        } else {
            self.known_variables.insert(variable)
        }
    }

    /// Returns the current best-known range constraint on the given variable
    /// combining global range constraints and newly derived local range constraints.
    /// For fixed columns, it also invokes the fixed evaluator.
    pub fn range_constraint(&self, variable: &Variable) -> RangeConstraint<T> {
        if let Variable::FixedCell(fixed_cell) = variable {
            if let Some(v) = self.fixed_evaluator.evaluate(fixed_cell) {
                return RangeConstraint::from_value(v);
            }
        }
        variable
            .try_to_poly_id()
            .and_then(|poly_id| {
                self.fixed_data
                    .global_range_constraints
                    .range_constraint(&AlgebraicReference {
                        name: Default::default(),
                        poly_id,
                        next: false,
                    })
            })
            .iter()
            .chain(self.derived_range_constraints.get(variable))
            .cloned()
            .reduce(|gc, rc| gc.conjunction(&rc))
            .unwrap_or_default()
    }

    pub fn evaluate(
        &self,
        expr: &Expression<T>,
        offset: i32,
    ) -> Option<AffineSymbolicExpression<T, Variable>> {
        Evaluator::new(self).evaluate(expr, offset)
    }

    pub fn try_evaluate_to_known_number(&self, expr: &Expression<T>, offset: i32) -> Option<T> {
        self.evaluate(expr, offset)?.try_to_known()?.try_to_number()
    }
}

struct Evaluator<'a, T: FieldElement, FixedEval: FixedEvaluator<T>> {
    witgen_inference: &'a WitgenInference<'a, T, FixedEval>,
    only_concrete_known: bool,
}

impl<'a, T: FieldElement, FixedEval: FixedEvaluator<T>> Evaluator<'a, T, FixedEval> {
    pub fn new(witgen_inference: &'a WitgenInference<'a, T, FixedEval>) -> Self {
        Self {
            witgen_inference,
            only_concrete_known: false,
        }
    }

    /// Sets this evaluator into the mode where only concrete variables are
    /// considered "known". This means even if we know how to compute a variable,
    /// as long as we cannot determine it to have a fixed value at compile-time,
    /// it is considered "unknown" and we can solve for it.
    pub fn only_concrete_known(self) -> Self {
        Self {
            witgen_inference: self.witgen_inference,
            only_concrete_known: true,
        }
    }

    pub fn evaluate(
        &self,
        expr: &Expression<T>,
        offset: i32,
    ) -> Option<AffineSymbolicExpression<T, Variable>> {
        Some(match expr {
            Expression::Reference(r) => self.evaluate_variable(Variable::from_reference(r, offset)),
            Expression::PublicReference(_) | Expression::Challenge(_) => {
                // TODO we need to introduce a variable type for those.
                return None;
            }
            Expression::Number(n) => (*n).into(),
            Expression::BinaryOperation(op) => self.evaluate_binary_operation(op, offset)?,
            Expression::UnaryOperation(op) => self.evaluate_unary_operation(op, offset)?,
        })
    }

    /// Turns the given variable either to a known symbolic value or an unknown symbolic value
    /// depending on if it is known or not.
    /// If it is known to be range-constrained to a single value, that value is used.
    pub fn evaluate_variable(&self, variable: Variable) -> AffineSymbolicExpression<T, Variable> {
        // If a variable is known and has a compile-time constant value,
        // that value is stored in the range constraints.
        let rc = self.witgen_inference.range_constraint(&variable);
        match self.witgen_inference.value(&variable) {
            Value::Concrete(val) => val.into(),
            Value::Unknown => AffineSymbolicExpression::from_unknown_variable(variable, rc),
            Value::Known if self.only_concrete_known => {
                AffineSymbolicExpression::from_unknown_variable(variable, rc)
            }
            Value::Known => AffineSymbolicExpression::from_known_symbol(variable, rc),
        }
    }

    fn evaluate_binary_operation(
        &self,
        op: &AlgebraicBinaryOperation<T>,
        offset: i32,
    ) -> Option<AffineSymbolicExpression<T, Variable>> {
        let left = self.evaluate(&op.left, offset);
        let right = self.evaluate(&op.right, offset);
        match op.op {
            AlgebraicBinaryOperator::Add => Some(&left? + &right?),
            AlgebraicBinaryOperator::Sub => Some(&left? - &right?),
            AlgebraicBinaryOperator::Mul => {
                if is_known_zero(&left) || is_known_zero(&right) {
                    Some(SymbolicExpression::from(T::from(0)).into())
                } else {
                    left?.try_mul(&right?)
                }
            }
            AlgebraicBinaryOperator::Pow => {
                let result = left?
                    .try_to_known()?
                    .try_to_number()?
                    .pow(right?.try_to_known()?.try_to_number()?.to_integer());
                Some(AffineSymbolicExpression::from(result))
            }
        }
    }

    fn evaluate_unary_operation(
        &self,
        op: &AlgebraicUnaryOperation<T>,
        offset: i32,
    ) -> Option<AffineSymbolicExpression<T, Variable>> {
        let expr = self.evaluate(&op.expr, offset)?;
        match op.op {
            AlgebraicUnaryOperator::Minus => Some(-&expr),
        }
    }
}

fn is_known_zero<T: FieldElement>(x: &Option<AffineSymbolicExpression<T, Variable>>) -> bool {
    x.as_ref()
        .and_then(|x| x.try_to_known().map(|x| x.is_known_zero()))
        .unwrap_or(false)
}

pub trait FixedEvaluator<T: FieldElement>: Clone {
    /// Evaluate a fixed column cell and returns its value if it is
    /// compile-time constant, otherwise return None.
    /// If this function returns `None`, the value of the fixed column will
    /// be treated as symbolically known but not compile-time constant
    /// (i.e. it depends on the row).
    fn evaluate(&self, _fixed_cell: &Cell) -> Option<T> {
        None
    }
}

pub trait CanProcessCall<T: FieldElement>: Clone {
    /// Returns (true, _) if a call to the machine that handles the given identity
    /// can always be processed with the given known inputs and range constraints
    /// on the parameters.
    /// The second return value is a vector of new range constraints.
    /// @see Machine::can_process_call
    fn can_process_call_fully(
        &self,
        _bus_id: T,
        _known_inputs: &BitVec,
        _range_constraints: Vec<RangeConstraint<T>>,
    ) -> (bool, Vec<RangeConstraint<T>>);
}

impl<T: FieldElement, Q: QueryCallback<T>> CanProcessCall<T> for &MutableState<'_, T, Q> {
    fn can_process_call_fully(
        &self,
        bus_id: T,
        known_inputs: &BitVec,
        range_constraints: Vec<RangeConstraint<T>>,
    ) -> (bool, Vec<RangeConstraint<T>>) {
        MutableState::can_process_call_fully(self, bus_id, known_inputs, range_constraints)
    }
}

#[cfg(test)]
mod test {
    use powdr_ast::analyzed::{PolyID, PolynomialIdentity, PolynomialType};
    use powdr_number::GoldilocksField;
    use pretty_assertions::assert_eq;
    use test_log::test;

    use crate::witgen::{
        global_constraints,
        jit::{effect::format_code, test_util::read_pil, variable::Cell},
        machines::{FixedLookup, KnownMachine},
        FixedData,
    };

    use super::*;

    #[derive(Clone)]
    pub struct FixedEvaluatorForFixedData<'a, T: FieldElement>(pub &'a FixedData<'a, T>);
    impl<T: FieldElement> FixedEvaluator<T> for FixedEvaluatorForFixedData<'_, T> {
        fn evaluate(&self, fixed_cell: &Cell) -> Option<T> {
            let poly_id = PolyID {
                id: fixed_cell.id,
                ptype: PolynomialType::Constant,
            };
            let values = self.0.fixed_cols[&poly_id].values_max_size();
            let row = (fixed_cell.row_offset + values.len() as i32) as usize % values.len();
            Some(values[row])
        }
    }

    fn solve_on_rows(input: &str, rows: &[i32], known_cells: Vec<(&str, i32)>) -> String {
        let (analyzed, fixed_col_vals) = read_pil::<GoldilocksField>(input);
        let fixed_data = FixedData::new(&analyzed, &fixed_col_vals, &[], Default::default(), 0);
        let fixed_data = global_constraints::set_global_constraints(fixed_data);

        let fixed_lookup_receives = fixed_data
            .bus_receives
            .iter()
            .filter(|(_, r)| FixedLookup::is_responsible(r))
            .map(|(bus_id, r)| (*bus_id, r))
            .collect();

        let global_constr = fixed_data.global_range_constraints.clone();
        let fixed_machine = FixedLookup::new(global_constr, &fixed_data, fixed_lookup_receives);
        let known_fixed = KnownMachine::FixedLookup(fixed_machine);
        let mutable_state = MutableState::new([known_fixed].into_iter(), &|_| {
            Err("Query not implemented".to_string())
        });

        let known_cells = known_cells.iter().map(|(name, row_offset)| {
            let id = fixed_data.try_column_by_name(name).unwrap().id;
            Variable::WitnessCell(Cell {
                column_name: name.to_string(),
                id,
                row_offset: *row_offset,
            })
        });

        let ref_eval = FixedEvaluatorForFixedData(&fixed_data);
        let mut witgen = WitgenInference::new(&fixed_data, ref_eval, known_cells, []);
        let mut counter = 0;

        loop {
            let mut progress = false;
            counter += 1;
            for row in rows {
                for id in fixed_data.identities.iter() {
                    let updated_vars = match id {
                        Identity::Polynomial(PolynomialIdentity { expression, .. }) => witgen
                            .process_equation_on_row(expression, None, 0.into(), *row)
                            .unwrap(),
                        Identity::BusSend(bus_send) => {
                            let mut updated_vars = vec![];
                            for (index, arg) in
                                bus_send.selected_payload.expressions.iter().enumerate()
                            {
                                let var = Variable::MachineCallParam(MachineCallVariable {
                                    identity_id: bus_send.identity_id,
                                    row_offset: *row,
                                    index,
                                });
                                updated_vars.extend(
                                    witgen
                                        .process_equation_on_row(arg, Some(var), 0.into(), *row)
                                        .unwrap(),
                                );
                            }
                            updated_vars.extend(
                                witgen.process_call(&mutable_state, bus_send, *row).unwrap(),
                            );
                            updated_vars
                        }
                        Identity::Connect(..) => vec![],
                    };
                    progress |= !updated_vars.is_empty();
                }
            }
            if !progress {
                break;
            }
            assert!(counter < 10000, "Solving took more than 10000 rounds.");
        }
        format_code(&witgen.finish())
    }

    #[test]
    fn simple_polynomial_solving() {
        let input = "let X; let Y; let Z; X = 1; Y = X + 1; Z * Y = X + 10;";
        let code = solve_on_rows(input, &[0], vec![]);
        assert_eq!(code, "X[0] = 1;\nY[0] = 2;\nZ[0] = -9223372034707292155;");
    }

    #[test]
    fn fib() {
        let input = "let X; let Y; X' = Y; Y' = X + Y;";
        let code = solve_on_rows(input, &[0, 1], vec![("X", 0), ("Y", 0)]);
        assert_eq!(
            code,
            "X[1] = Y[0];\nY[1] = (X[0] + Y[0]);\nX[2] = Y[1];\nY[2] = (X[1] + Y[1]);"
        );
    }

    #[test]
    fn fib_with_fixed() {
        let input = "
        namespace Fib(8);
            col fixed FIRST = [1] + [0]*;
            let x;
            let y;
            FIRST * (y - 1) = 0;
            FIRST * (x - 1) = 0;
            // This works in this test because we do not implement wrapping properly in this test.
            x' - y = 0;
            y' - (x + y) = 0;
        ";
        let code = solve_on_rows(input, &[0, 1, 2, 3], vec![]);
        assert_eq!(
            code,
            "Fib::y[0] = 1;
Fib::x[0] = 1;
Fib::x[1] = 1;
Fib::y[1] = 2;
Fib::x[2] = 2;
Fib::y[2] = 3;
Fib::x[3] = 3;
Fib::y[3] = 5;
Fib::x[4] = 5;
Fib::y[4] = 8;"
        );
    }

    #[test]
    fn xor() {
        let input = "
namespace Xor(256 * 256);
    let latch: col = |i| { if (i % 4) == 3 { 1 } else { 0 } };
    let FACTOR: col = |i| { 1 << (((i + 1) % 4) * 8) };

    let a: int -> int = |i| i % 256;
    let b: int -> int = |i| (i / 256) % 256;
    let P_A: col = a;
    let P_B: col = b;
    let P_C: col = |i| a(i) ^ b(i);

    let A_byte;
    let B_byte;
    let C_byte;

    [ A_byte, B_byte, C_byte ] in [ P_A, P_B, P_C ];

    let A;
    let B;
    let C;

    A' = A * (1 - latch) + A_byte * FACTOR;
    B' = B * (1 - latch) + B_byte * FACTOR;
    C' = C * (1 - latch) + C_byte * FACTOR;
";
        let code = solve_on_rows(
            input,
            // Use the second block to avoid wrap-around.
            &[3, 4, 5, 6, 7],
            vec![
                ("Xor::A", 7),
                ("Xor::C", 7), // We solve it in reverse, just for fun.
            ],
        );
        assert_eq!(
            code,
            "\
2**0 * Xor::A[6] + 2**24 * Xor::A_byte[6] := Xor::A[7];
2**0 * Xor::C[6] + 2**24 * Xor::C_byte[6] := Xor::C[7];
2**0 * Xor::A[5] + 2**16 * Xor::A_byte[5] := Xor::A[6];
2**0 * Xor::C[5] + 2**16 * Xor::C_byte[5] := Xor::C[6];
call_var(0, 6, 0) = Xor::A_byte[6];
call_var(0, 6, 2) = Xor::C_byte[6];
machine_call(1, [Known(call_var(0, 6, 0)), Unknown(call_var(0, 6, 1)), Known(call_var(0, 6, 2))]);
2**0 * Xor::A[4] + 2**8 * Xor::A_byte[4] := Xor::A[5];
2**0 * Xor::C[4] + 2**8 * Xor::C_byte[4] := Xor::C[5];
call_var(0, 5, 0) = Xor::A_byte[5];
call_var(0, 5, 2) = Xor::C_byte[5];
machine_call(1, [Known(call_var(0, 5, 0)), Unknown(call_var(0, 5, 1)), Known(call_var(0, 5, 2))]);
Xor::B_byte[6] = call_var(0, 6, 1);
Xor::A_byte[3] = Xor::A[4];
Xor::C_byte[3] = Xor::C[4];
call_var(0, 4, 0) = Xor::A_byte[4];
call_var(0, 4, 2) = Xor::C_byte[4];
machine_call(1, [Known(call_var(0, 4, 0)), Unknown(call_var(0, 4, 1)), Known(call_var(0, 4, 2))]);
Xor::B_byte[5] = call_var(0, 5, 1);
call_var(0, 3, 0) = Xor::A_byte[3];
call_var(0, 3, 2) = Xor::C_byte[3];
machine_call(1, [Known(call_var(0, 3, 0)), Unknown(call_var(0, 3, 1)), Known(call_var(0, 3, 2))]);
Xor::B_byte[4] = call_var(0, 4, 1);
Xor::B_byte[3] = call_var(0, 3, 1);
Xor::B[4] = Xor::B_byte[3];
Xor::B[5] = (Xor::B[4] + (Xor::B_byte[4] * 256));
Xor::B[6] = (Xor::B[5] + (Xor::B_byte[5] * 65536));
Xor::B[7] = (Xor::B[6] + (Xor::B_byte[6] * 16777216));"
        );
    }
}
