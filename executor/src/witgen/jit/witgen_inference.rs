use std::collections::{HashMap, HashSet};

use itertools::Itertools;
use powdr_ast::analyzed::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression as Expression,
    AlgebraicReference, AlgebraicUnaryOperation, AlgebraicUnaryOperator, Challenge, Identity,
    LookupIdentity, PermutationIdentity, PhantomBusInteractionIdentity, PhantomLookupIdentity,
    PhantomPermutationIdentity, PolyID, PolynomialIdentity, PolynomialType, SelectedExpressions,
};
use powdr_number::FieldElement;

use crate::witgen::{
    global_constraints::RangeConstraintSet, jit::affine_symbolic_expression::MachineCallArgument,
};

use super::{
    super::{range_constraints::RangeConstraint, FixedData},
    affine_symbolic_expression::{AffineSymbolicExpression, Effect, ProcessResult},
    cell::Cell,
};

/// This component can generate code that solves identities.
/// It needs a driver that tells it which identities to process on which rows.
pub struct WitgenInference<'a, T: FieldElement, RefEval: ReferenceEvaluator<T>> {
    fixed_data: &'a FixedData<'a, T>,
    reference_evaluator: RefEval,
    range_constraints: HashMap<Cell, RangeConstraint<T>>,
    known_cells: HashSet<Cell>,
    code: Vec<Effect<T, Cell>>,
}

impl<'a, T: FieldElement, RefEval: ReferenceEvaluator<T>> WitgenInference<'a, T, RefEval> {
    pub fn new(
        fixed_data: &'a FixedData<'a, T>,
        reference_evaluator: RefEval,
        known_cells: impl IntoIterator<Item = Cell>,
    ) -> Self {
        Self {
            fixed_data,
            reference_evaluator,
            range_constraints: Default::default(),
            known_cells: known_cells.into_iter().collect(),
            code: Default::default(),
        }
    }

    pub fn known_cells(&self) -> &HashSet<Cell> {
        &self.known_cells
    }

    pub fn code(self) -> Vec<Effect<T, Cell>> {
        self.code
    }

    /// Process an identity on a certain row.
    /// Returns true if this identity/row pair was fully processed and
    /// should not be considered again.
    pub fn process_identity(&mut self, id: &Identity<T>, row_offset: i32) -> bool {
        let result = match id {
            Identity::Polynomial(PolynomialIdentity { expression, .. }) => {
                self.process_polynomial_identity(expression, row_offset)
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
                // TODO Once we have a concept of "can_be_answered", bus interactions
                // should be as easy as lookups.
                ProcessResult::empty()
            }
            Identity::Connect(_) => ProcessResult::empty(),
        };
        self.ingest_effects(result.effects);
        result.complete
    }

    fn process_polynomial_identity(
        &self,
        expression: &'a Expression<T>,
        offset: i32,
    ) -> ProcessResult<T, Cell> {
        if let Some(r) = self.evaluate(expression, offset) {
            // TODO propagate or report error properly.
            // If solve returns an error, it means that the constraint is conflicting.
            // In the future, we might run this in a runtime-conditional, so an error
            // could just mean that this case cannot happen in practice.
            r.solve().unwrap()
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
    ) -> ProcessResult<T, Cell> {
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

    fn ingest_effects(&mut self, effects: Vec<Effect<T, Cell>>) {
        for e in effects {
            match &e {
                Effect::Assignment(cell, assignment) => {
                    self.known_cells.insert(cell.clone());
                    if let Some(rc) = assignment.range_constraint() {
                        // If the cell was determined to be a constant, we add this
                        // as a range constraint, so we can use it in future evaluations.
                        self.add_range_constraint(cell.clone(), rc);
                    }
                    self.code.push(e);
                }
                Effect::RangeConstraint(cell, rc) => {
                    self.add_range_constraint(cell.clone(), rc.clone());
                }
                Effect::MachineCall(_, arguments) => {
                    for arg in arguments {
                        if let MachineCallArgument::Unknown(expr) = arg {
                            let cell = expr.single_unknown_variable().unwrap();
                            self.known_cells.insert(cell.clone());
                        }
                    }
                    self.code.push(e);
                }
                Effect::Assertion(_) => self.code.push(e),
            }
        }
    }

    fn add_range_constraint(&mut self, cell: Cell, rc: RangeConstraint<T>) {
        let rc = self
            .range_constraint(cell.clone())
            .map_or(rc.clone(), |existing_rc| existing_rc.conjunction(&rc));
        if !self.known_cells.contains(&cell) {
            if let Some(v) = rc.try_to_single_value() {
                // Special case: Cell is fixed to a constant by range constraints only.
                self.known_cells.insert(cell.clone());
                self.code.push(Effect::Assignment(cell.clone(), v.into()));
            }
        }
        self.range_constraints.insert(cell.clone(), rc);
    }

    fn evaluate(
        &self,
        expr: &Expression<T>,
        offset: i32,
    ) -> Option<AffineSymbolicExpression<T, Cell>> {
        Some(match expr {
            Expression::Reference(r) => {
                if r.is_fixed() {
                    self.reference_evaluator.evaluate_fixed(r, offset)?.into()
                } else {
                    let cell = Cell::from_reference(r, offset);
                    // If a cell is known and has a compile-time constant value,
                    // that value is stored in the range constraints.
                    let rc = self.range_constraint(cell.clone());
                    if let Some(val) = rc.as_ref().and_then(|rc| rc.try_to_single_value()) {
                        val.into()
                    } else if self.known_cells.contains(&cell) {
                        AffineSymbolicExpression::from_known_symbol(cell, rc)
                    } else {
                        AffineSymbolicExpression::from_unknown_variable(cell, rc)
                    }
                }
            }
            Expression::PublicReference(public) => {
                self.reference_evaluator.evaluate_public(public)?.into()
            }
            Expression::Challenge(challenge) => self
                .reference_evaluator
                .evaluate_challenge(challenge)?
                .into(),
            Expression::Number(n) => (*n).into(),
            Expression::BinaryOperation(op) => self.evaluate_binary_operation(op, offset)?,
            Expression::UnaryOperation(op) => self.evaluate_unary_operation(op, offset)?,
        })
    }

    fn evaluate_binary_operation(
        &self,
        op: &AlgebraicBinaryOperation<T>,
        offset: i32,
    ) -> Option<AffineSymbolicExpression<T, Cell>> {
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
    ) -> Option<AffineSymbolicExpression<T, Cell>> {
        let expr = self.evaluate(&op.expr, offset)?;
        match op.op {
            AlgebraicUnaryOperator::Minus => Some(-&expr),
        }
    }

    /// Returns the current best-known range constraint on the given cell
    /// combining global range constraints and newly derived local range constraints.
    fn range_constraint(&self, cell: Cell) -> Option<RangeConstraint<T>> {
        self.fixed_data
            .global_range_constraints
            .range_constraint(&AlgebraicReference {
                name: Default::default(),
                poly_id: PolyID {
                    id: cell.id,
                    ptype: PolynomialType::Committed,
                },
                next: false,
            })
            .iter()
            .chain(self.range_constraints.get(&cell))
            .cloned()
            .reduce(|gc, rc| gc.conjunction(&rc))
    }
}

pub trait ReferenceEvaluator<T: FieldElement> {
    fn evaluate_fixed(&self, _var: &AlgebraicReference, _row_offset: i32) -> Option<T> {
        None
    }
    fn evaluate_challenge(&self, _challenge: &Challenge) -> Option<T> {
        None
    }
    fn evaluate_public(&self, _public: &String) -> Option<T> {
        None
    }
}

#[cfg(test)]
mod test {
    use std::{fs, str::from_utf8};

    use powdr_ast::analyzed::Analyzed;
    use powdr_number::GoldilocksField;

    use crate::{
        constant_evaluator,
        witgen::{jit::affine_symbolic_expression::Assertion, FixedData},
    };

    use super::*;

    fn format_code(effects: &[Effect<GoldilocksField, Cell>]) -> String {
        effects
            .iter()
            .map(|effect| match effect {
                Effect::Assignment(v, expr) => format!("{v} = {expr};"),
                Effect::Assertion(Assertion {
                    lhs,
                    rhs,
                    expected_equal,
                }) => {
                    format!(
                        "assert {lhs} {} {rhs};",
                        if *expected_equal { "==" } else { "!=" }
                    )
                }
                Effect::MachineCall(id, args) => {
                    format!(
                        "lookup({id}, [{}]);",
                        args.iter()
                            .map(|arg| match arg {
                                MachineCallArgument::Known(k) => format!("Known({k})"),
                                MachineCallArgument::Unknown(u) => format!("Unknown({u})"),
                            })
                            .join(", ")
                    )
                }
                Effect::RangeConstraint(..) => {
                    panic!("Range constraints should not be part of the code.")
                }
            })
            .join("\n")
    }

    struct ReferenceEvaluatorForFixedData<'a>(&'a FixedData<'a, GoldilocksField>);
    impl<'a> ReferenceEvaluator<GoldilocksField> for ReferenceEvaluatorForFixedData<'a> {
        fn evaluate_fixed(
            &self,
            var: &AlgebraicReference,
            row_offset: i32,
        ) -> Option<GoldilocksField> {
            assert!(var.is_fixed());
            let values = self.0.fixed_cols[&var.poly_id].values_max_size();
            let row = (row_offset as usize + var.next as usize) % values.len();
            Some(values[row])
        }
    }

    fn solve_on_rows(input: &str, rows: &[i32], known_cells: Vec<(&str, i32)>) -> String {
        let analyzed: Analyzed<GoldilocksField> =
            powdr_pil_analyzer::analyze_string(input).unwrap();
        let fixed_col_vals = constant_evaluator::generate(&analyzed);
        let fixed_data = FixedData::new(&analyzed, &fixed_col_vals, &[], Default::default(), 0);
        let known_cells = known_cells.iter().map(|(name, row_offset)| {
            let id = fixed_data.try_column_by_name(name).unwrap().id;
            Cell {
                column_name: name.to_string(),
                id,
                row_offset: *row_offset,
            }
        });

        let ref_eval = ReferenceEvaluatorForFixedData(&fixed_data);
        let mut witgen = WitgenInference::new(&fixed_data, ref_eval, known_cells);
        let mut complete = HashSet::new();
        while complete.len() != analyzed.identities.len() * rows.len() {
            for row in rows {
                for id in analyzed.identities.iter() {
                    if !complete.contains(&(id.id(), *row)) && witgen.process_identity(id, *row) {
                        complete.insert((id.id(), *row));
                    }
                }
            }
        }
        format_code(&witgen.code())
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
        let code = solve_on_rows(&input, &[0, 1, 2, 3], vec![]);
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
}
