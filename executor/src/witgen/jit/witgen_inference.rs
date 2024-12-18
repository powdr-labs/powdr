use std::collections::{HashMap, HashSet};

use itertools::Itertools;
use powdr_ast::analyzed::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression as Expression,
    AlgebraicReference, AlgebraicUnaryOperation, AlgebraicUnaryOperator, Identity, LookupIdentity,
    PermutationIdentity, PhantomLookupIdentity, PhantomPermutationIdentity, PolynomialIdentity,
    PolynomialType, SelectedExpressions,
};
use powdr_number::FieldElement;

use crate::witgen::{
    global_constraints::RangeConstraintSet, jit::affine_symbolic_expression::MachineCallArgument,
};

use super::{
    super::{range_constraints::RangeConstraint, FixedData},
    affine_symbolic_expression::{AffineSymbolicExpression, Effect, ProcessResult},
    variable::Variable,
};

/// Summary of the effect of processing an action.
pub struct ProcessSummary {
    /// The action has been fully completed, processing it again will not have any effect.
    pub complete: bool,
    /// Processing the action changed the state of the inference.
    pub progress: bool,
}

/// This component can generate code that solves identities.
/// It needs a driver that tells it which identities to process on which rows.
pub struct WitgenInference<'a, T: FieldElement, FixedEval: FixedEvaluator<T>> {
    fixed_data: &'a FixedData<'a, T>,
    fixed_evaluator: FixedEval,
    derived_range_constraints: HashMap<Variable, RangeConstraint<T>>,
    known_variables: HashSet<Variable>,
    /// Internal equalities we were not able to solve yet.
    assignments: Vec<Assignment<'a, T>>,
    code: Vec<Effect<T, Variable>>,
}

impl<'a, T: FieldElement, FixedEval: FixedEvaluator<T>> WitgenInference<'a, T, FixedEval> {
    pub fn new(
        fixed_data: &'a FixedData<'a, T>,
        fixed_evaluator: FixedEval,
        known_variables: impl IntoIterator<Item = Variable>,
    ) -> Self {
        Self {
            fixed_data,
            fixed_evaluator,
            derived_range_constraints: Default::default(),
            known_variables: known_variables.into_iter().collect(),
            assignments: Default::default(),
            code: Default::default(),
        }
    }

    pub fn code(self) -> Vec<Effect<T, Variable>> {
        self.code
    }

    pub fn is_known(&self, variable: &Variable) -> bool {
        self.known_variables.contains(variable)
    }

    /// Process an identity on a certain row.
    pub fn process_identity(&mut self, id: &Identity<T>, row_offset: i32) -> ProcessSummary {
        let result = match id {
            Identity::Polynomial(PolynomialIdentity { expression, .. }) => {
                self.process_equality_on_row(expression, row_offset, T::from(0).into())
            }
            Identity::Lookup(LookupIdentity {
                id, left, right, ..
            })
            | Identity::Permutation(PermutationIdentity {
                id, left, right, ..
            })
            | Identity::PhantomPermutation(PhantomPermutationIdentity {
                id, left, right, ..
            })
            | Identity::PhantomLookup(PhantomLookupIdentity {
                id, left, right, ..
            }) => self.process_lookup(*id, left, right, row_offset),
            Identity::PhantomBusInteraction(_) => {
                // TODO(bus_interaction) Once we have a concept of "can_be_answered", bus interactions
                // should be as easy as lookups.
                ProcessResult::empty()
            }
            Identity::Connect(_) => ProcessResult::empty(),
        };
        self.ingest_effects(result)
    }

    /// Turns the given variable either to a known symbolic value or an unknown symbolic value
    /// depending on if it is known or not.
    /// If it is known to be range-constrained to a single value, that value is used.
    fn variable_to_expression(&self, variable: Variable) -> AffineSymbolicExpression<T, Variable> {
        // If a variable is known and has a compile-time constant value,
        // that value is stored in the range constraints.
        let rc = self.range_constraint(variable.clone());
        if let Some(val) = rc.as_ref().and_then(|rc| rc.try_to_single_value()) {
            val.into()
        } else if self.known_variables.contains(&variable) {
            AffineSymbolicExpression::from_known_symbol(variable, rc)
        } else {
            AffineSymbolicExpression::from_unknown_variable(variable, rc)
        }
    }

    /// Process the constraint that the expression evaluated at the given offset equals the given value.
    /// This does not have to be solvable right away, but is always processed as soon as we have progress.
    /// Note that all variables in the expression can be unknown and their status can also change over time.
    pub fn assign_constant(&mut self, expression: &'a Expression<T>, row_offset: i32, value: T) {
        self.assignments.push(Assignment {
            lhs: expression,
            row_offset,
            rhs: VariableOrValue::Value(value),
        });
        self.process_assignments();
    }

    /// Process the constraint that the expression evaluated at the given offset equals the given formal variable.
    /// This does not have to be solvable right away, but is always processed as soon as we have progress.
    /// Note that all variables in the expression can be unknown and their status can also change over time.
    pub fn assign_variable(
        &mut self,
        expression: &'a Expression<T>,
        row_offset: i32,
        variable: Variable,
    ) {
        self.assignments.push(Assignment {
            lhs: expression,
            row_offset,
            rhs: VariableOrValue::Variable(variable),
        });
        self.process_assignments();
    }

    fn process_equality_on_row(
        &self,
        lhs: &Expression<T>,
        offset: i32,
        rhs: AffineSymbolicExpression<T, Variable>,
    ) -> ProcessResult<T, Variable> {
        if let Some(r) = self.evaluate(lhs, offset) {
            // TODO propagate or report error properly.
            // If solve returns an error, it means that the constraint is conflicting.
            // In the future, we might run this in a runtime-conditional, so an error
            // could just mean that this case cannot happen in practice.
            (r - rhs).solve().unwrap()
        } else {
            ProcessResult::empty()
        }
    }

    fn process_lookup(
        &self,
        lookup_id: u64,
        left: &SelectedExpressions<T>,
        right: &SelectedExpressions<T>,
        offset: i32,
    ) -> ProcessResult<T, Variable> {
        // TODO: In the future, call the 'mutable state' to check if the
        // lookup can always be answered.

        // If the RHS is fully fixed columns...
        if right.expressions.iter().all(|e| match e {
            Expression::Reference(r) => r.is_fixed(),
            Expression::Number(_) => true,
            _ => false,
        }) {
            // and the selector is known to be 1...
            if self
                .evaluate(&left.selector, offset)
                .and_then(|s| s.try_to_known().map(|k| k.is_known_one()))
                == Some(true)
            {
                if let Some(lhs) = left
                    .expressions
                    .iter()
                    .map(|e| self.evaluate(e, offset))
                    .collect::<Option<Vec<_>>>()
                {
                    // and all except one expression is known on the LHS.
                    let unknown = lhs
                        .iter()
                        .filter(|e| e.try_to_known().is_none())
                        .collect_vec();
                    if unknown.len() == 1 && unknown[0].single_unknown_variable().is_some() {
                        let effects = vec![Effect::MachineCall(
                            lookup_id,
                            lhs.into_iter()
                                .map(|e| {
                                    if let Some(val) = e.try_to_known() {
                                        MachineCallArgument::Known(val.clone())
                                    } else {
                                        MachineCallArgument::Unknown(e)
                                    }
                                })
                                .collect(),
                        )];
                        return ProcessResult::complete(effects);
                    }
                }
            }
        }
        ProcessResult::empty()
    }

    fn process_assignments(&mut self) {
        loop {
            let mut progress = false;
            let new_assignments = std::mem::take(&mut self.assignments)
                .into_iter()
                .flat_map(|assignment| {
                    let rhs = match &assignment.rhs {
                        VariableOrValue::Variable(v) => self.variable_to_expression(v.clone()),
                        VariableOrValue::Value(v) => (*v).into(),
                    };
                    let r =
                        self.process_equality_on_row(assignment.lhs, assignment.row_offset, rhs);
                    let summary = self.ingest_effects(r);
                    progress |= summary.progress;
                    // If it is not complete, queue it again.
                    (!summary.complete).then_some(assignment)
                })
                .collect_vec();
            self.assignments.extend(new_assignments);
            if !progress {
                break;
            }
        }
    }

    fn ingest_effects(&mut self, process_result: ProcessResult<T, Variable>) -> ProcessSummary {
        let mut progress = false;
        for e in process_result.effects {
            match &e {
                Effect::Assignment(variable, assignment) => {
                    assert!(self.known_variables.insert(variable.clone()));
                    if let Some(rc) = assignment.range_constraint() {
                        // If the variable was determined to be a constant, we add this
                        // as a range constraint, so we can use it in future evaluations.
                        self.add_range_constraint(variable.clone(), rc);
                    }
                    progress = true;
                    self.code.push(e);
                }
                Effect::RangeConstraint(variable, rc) => {
                    progress |= self.add_range_constraint(variable.clone(), rc.clone());
                }
                Effect::MachineCall(_, arguments) => {
                    for arg in arguments {
                        if let MachineCallArgument::Unknown(expr) = arg {
                            let variable = expr.single_unknown_variable().unwrap();
                            self.known_variables.insert(variable.clone());
                        }
                    }
                    progress = true;
                    self.code.push(e);
                }
                Effect::Assertion(_) => self.code.push(e),
            }
        }
        if progress {
            self.process_assignments();
        }
        ProcessSummary {
            complete: process_result.complete,
            progress,
        }
    }

    /// Adds a range constraint to the set of derived range constraints. Returns true if progress was made.
    fn add_range_constraint(&mut self, variable: Variable, rc: RangeConstraint<T>) -> bool {
        let rc = self
            .range_constraint(variable.clone())
            .map_or(rc.clone(), |existing_rc| existing_rc.conjunction(&rc));
        if !self.known_variables.contains(&variable) {
            if let Some(v) = rc.try_to_single_value() {
                // Special case: Variable is fixed to a constant by range constraints only.
                self.known_variables.insert(variable.clone());
                self.code
                    .push(Effect::Assignment(variable.clone(), v.into()));
            }
        }
        let old_rc = self
            .derived_range_constraints
            .insert(variable.clone(), rc.clone());

        // If the range constraint changed, we made progress.
        old_rc != Some(rc)
    }

    fn evaluate(
        &self,
        expr: &Expression<T>,
        offset: i32,
    ) -> Option<AffineSymbolicExpression<T, Variable>> {
        Some(match expr {
            Expression::Reference(r) => match r.poly_id.ptype {
                PolynomialType::Constant => self.fixed_evaluator.evaluate(r, offset)?.into(),
                PolynomialType::Committed => {
                    let variable = Variable::from_reference(r, offset);
                    self.variable_to_expression(variable)
                }
                PolynomialType::Intermediate => {
                    let definition = &self.fixed_data.intermediate_definitions[&r.to_thin()];
                    self.evaluate(definition, offset)?
                }
            },
            Expression::PublicReference(_) | Expression::Challenge(_) => {
                // TODO we need to introduce a variable type for those.
                return None;
            }
            Expression::Number(n) => (*n).into(),
            Expression::BinaryOperation(op) => self.evaluate_binary_operation(op, offset)?,
            Expression::UnaryOperation(op) => self.evaluate_unary_operation(op, offset)?,
        })
    }

    fn evaluate_binary_operation(
        &self,
        op: &AlgebraicBinaryOperation<T>,
        offset: i32,
    ) -> Option<AffineSymbolicExpression<T, Variable>> {
        let left = self.evaluate(&op.left, offset)?;
        let right = self.evaluate(&op.right, offset)?;
        match op.op {
            AlgebraicBinaryOperator::Add => Some(&left + &right),
            AlgebraicBinaryOperator::Sub => Some(&left - &right),
            AlgebraicBinaryOperator::Mul => left.try_mul(&right),
            AlgebraicBinaryOperator::Pow => {
                let result = left
                    .try_to_known()?
                    .try_to_number()?
                    .pow(right.try_to_known()?.try_to_number()?.to_integer());
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

    /// Returns the current best-known range constraint on the given variable
    /// combining global range constraints and newly derived local range constraints.
    fn range_constraint(&self, variable: Variable) -> Option<RangeConstraint<T>> {
        variable
            .try_to_witness_poly_id()
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
            .chain(self.derived_range_constraints.get(&variable))
            .cloned()
            .reduce(|gc, rc| gc.conjunction(&rc))
    }
}

/// An equality constraint between an algebraic expression evaluated
/// on a certain row offset and a variable or fixed constant value.
struct Assignment<'a, T: FieldElement> {
    lhs: &'a Expression<T>,
    row_offset: i32,
    rhs: VariableOrValue<T, Variable>,
}

enum VariableOrValue<T, V> {
    Variable(V),
    Value(T),
}

pub trait FixedEvaluator<T: FieldElement> {
    fn evaluate(&self, _var: &AlgebraicReference, _row_offset: i32) -> Option<T> {
        None
    }
}

#[cfg(test)]
mod test {

    use pretty_assertions::assert_eq;

    use powdr_ast::analyzed::Analyzed;
    use powdr_number::GoldilocksField;

    use crate::{
        constant_evaluator,
        witgen::{
            global_constraints,
            jit::{test_util::format_code, variable::Cell},
            FixedData,
        },
    };

    use super::*;

    pub struct FixedEvaluatorForFixedData<'a, T: FieldElement>(pub &'a FixedData<'a, T>);
    impl<'a, T: FieldElement> FixedEvaluator<T> for FixedEvaluatorForFixedData<'a, T> {
        fn evaluate(&self, var: &AlgebraicReference, row_offset: i32) -> Option<T> {
            assert!(var.is_fixed());
            let values = self.0.fixed_cols[&var.poly_id].values_max_size();
            let row = (row_offset + var.next as i32 + values.len() as i32) as usize % values.len();
            Some(values[row])
        }
    }

    fn solve_on_rows(
        input: &str,
        rows: &[i32],
        known_cells: Vec<(&str, i32)>,
        expected_complete: Option<usize>,
    ) -> String {
        let analyzed: Analyzed<GoldilocksField> =
            powdr_pil_analyzer::analyze_string(input).unwrap();
        let fixed_col_vals = constant_evaluator::generate(&analyzed);
        let fixed_data = FixedData::new(&analyzed, &fixed_col_vals, &[], Default::default(), 0);
        let (fixed_data, retained_identities) =
            global_constraints::set_global_constraints(fixed_data, &analyzed.identities);

        let known_cells = known_cells.iter().map(|(name, row_offset)| {
            let id = fixed_data.try_column_by_name(name).unwrap().id;
            Variable::Cell(Cell {
                column_name: name.to_string(),
                id,
                row_offset: *row_offset,
            })
        });

        let ref_eval = FixedEvaluatorForFixedData(&fixed_data);
        let mut witgen = WitgenInference::new(&fixed_data, ref_eval, known_cells);
        let mut complete = HashSet::new();
        let mut counter = 0;
        let expected_complete = expected_complete.unwrap_or(retained_identities.len() * rows.len());
        while complete.len() != expected_complete {
            counter += 1;
            for row in rows {
                for id in retained_identities.iter() {
                    if !complete.contains(&(id.id(), *row))
                        && witgen.process_identity(id, *row).complete
                    {
                        complete.insert((id.id(), *row));
                    }
                }
            }
            assert!(counter < 10000, "Solving took more than 10000 rounds.");
        }
        format_code(&witgen.code())
    }

    #[test]
    fn simple_polynomial_solving() {
        let input = "let X; let Y; let Z; X = 1; Y = X + 1; Z * Y = X + 10;";
        let code = solve_on_rows(input, &[0], vec![], None);
        assert_eq!(code, "X[0] = 1;\nY[0] = 2;\nZ[0] = -9223372034707292155;");
    }

    #[test]
    fn fib() {
        let input = "let X; let Y; X' = Y; Y' = X + Y;";
        let code = solve_on_rows(input, &[0, 1], vec![("X", 0), ("Y", 0)], None);
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
        let code = solve_on_rows(input, &[0, 1, 2, 3], vec![], None);
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
            Some(16),
        );
        assert_eq!(
            code,
            "\
Xor::A_byte[6] = ((Xor::A[7] & 4278190080) // 16777216);
Xor::A[6] = (Xor::A[7] & 16777215);
assert (Xor::A[7] & 18446744069414584320) == 0;
Xor::C_byte[6] = ((Xor::C[7] & 4278190080) // 16777216);
Xor::C[6] = (Xor::C[7] & 16777215);
assert (Xor::C[7] & 18446744069414584320) == 0;
Xor::A_byte[5] = ((Xor::A[6] & 16711680) // 65536);
Xor::A[5] = (Xor::A[6] & 65535);
assert (Xor::A[6] & 18446744073692774400) == 0;
Xor::C_byte[5] = ((Xor::C[6] & 16711680) // 65536);
Xor::C[5] = (Xor::C[6] & 65535);
assert (Xor::C[6] & 18446744073692774400) == 0;
machine_call(0, [Known(Xor::A_byte[6]), Unknown(Xor::B_byte[6]), Known(Xor::C_byte[6])]);
Xor::A_byte[4] = ((Xor::A[5] & 65280) // 256);
Xor::A[4] = (Xor::A[5] & 255);
assert (Xor::A[5] & 18446744073709486080) == 0;
Xor::C_byte[4] = ((Xor::C[5] & 65280) // 256);
Xor::C[4] = (Xor::C[5] & 255);
assert (Xor::C[5] & 18446744073709486080) == 0;
machine_call(0, [Known(Xor::A_byte[5]), Unknown(Xor::B_byte[5]), Known(Xor::C_byte[5])]);
Xor::A_byte[3] = Xor::A[4];
Xor::C_byte[3] = Xor::C[4];
machine_call(0, [Known(Xor::A_byte[4]), Unknown(Xor::B_byte[4]), Known(Xor::C_byte[4])]);
machine_call(0, [Known(Xor::A_byte[3]), Unknown(Xor::B_byte[3]), Known(Xor::C_byte[3])]);
Xor::B[4] = Xor::B_byte[3];
Xor::B[5] = (Xor::B[4] + (Xor::B_byte[4] * 256));
Xor::B[6] = (Xor::B[5] + (Xor::B_byte[5] * 65536));
Xor::B[7] = (Xor::B[6] + (Xor::B_byte[6] * 16777216));"
        );
    }
}
