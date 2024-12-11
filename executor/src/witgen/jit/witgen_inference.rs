use std::collections::{HashMap, HashSet};

use itertools::Itertools;
use powdr_ast::analyzed::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression as Expression,
    AlgebraicReference, AlgebraicUnaryOperation, AlgebraicUnaryOperator, Identity, LookupIdentity,
    PhantomLookupIdentity, PolyID, PolynomialIdentity, PolynomialType, SelectedExpressions,
};
use powdr_number::{FieldElement, LargeInt};

use crate::witgen::{
    global_constraints::RangeConstraintSet, jit::affine_symbolic_expression::LookupArgument,
};

use super::{
    super::{range_constraints::RangeConstraint, FixedData},
    affine_symbolic_expression::{AffineSymbolicExpression, Effect, ProcessResult},
    cell::Cell,
};

/// This component can generate code that solves identities.
/// It needs a driver that tells it which identities to process on which rows.
pub struct WitgenInference<'a, T: FieldElement> {
    fixed_data: &'a FixedData<'a, T>,
    range_constraints: HashMap<Cell, RangeConstraint<T>>,
    known_cells: HashSet<Cell>,
    code: Vec<Effect<T, Cell>>,
}

impl<'a, T: FieldElement> WitgenInference<'a, T> {
    pub fn new(
        fixed_data: &'a FixedData<'a, T>,
        known_cells: impl IntoIterator<Item = Cell>,
    ) -> Self {
        Self {
            fixed_data,
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
                id,
                source: _,
                left,
                right,
            })
            | Identity::PhantomLookup(PhantomLookupIdentity {
                id,
                source: _,
                left,
                right,
                multiplicity: _,
            }) => {
                // TODO multiplicity?
                self.process_lookup(*id, left, right, row_offset)
            }
            _ => {
                // TODO
                ProcessResult::empty()
            }
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
            // TODO this can also happen if we solve a constraint where all values are
            // known but symbolic.
            r.solve(self).unwrap()
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
                        let effects = vec![Effect::Lookup(
                            lookup_id,
                            lhs.into_iter()
                                .map(|e| {
                                    if let Some(val) = e.try_to_known() {
                                        LookupArgument::Known(val.clone())
                                    } else {
                                        LookupArgument::Unknown(e)
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
                    if let Some(rc) = assignment.range_constraint() {
                        // If the cell was determined to be a constant, we add this
                        // as a range constraint, so we can use it in future evaluations.
                        self.add_range_constraint(cell.clone(), rc);
                    }
                    self.known_cells.insert(cell.clone());
                    self.code.push(e);
                }
                Effect::RangeConstraint(cell, rc) => {
                    self.add_range_constraint(cell.clone(), rc.clone());
                }
                Effect::Lookup(_, arguments) => {
                    for arg in arguments {
                        if let LookupArgument::Unknown(expr) = arg {
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
        // TODO if the conjuntion results in a single value, make the cell known.
        // TODO but do we also need to generate code for the assignment?
        self.range_constraints.insert(cell, rc);
    }

    fn evaluate(
        &self,
        expr: &Expression<T>,
        offset: i32,
    ) -> Option<AffineSymbolicExpression<T, Cell>> {
        Some(match expr {
            Expression::Reference(r) => {
                if r.is_fixed() {
                    todo!()
                    // let mut row = self.latch_row as i64 + offset as i64;
                    // while row < 0 {
                    //     row += self.block_size as i64;
                    // }
                    // // TODO at some point we should check that all of the fixed columns are periodic.
                    // // TODO We can only do this for block machines.
                    // // For dynamic machines, fixed columns are "known but symbolic"
                    // let v = self.fixed_data.fixed_cols[&r.poly_id].values_max_size()[row as usize];
                    // EvalResult::from_number(v)
                } else {
                    let cell = Cell::from_reference(r, offset);
                    // If a cell is known and has a compile-time constant value,
                    // that value is stored in the range constraints.
                    if let Some(v) = self
                        .range_constraint(cell.clone())
                        .and_then(|rc| rc.try_to_single_value())
                    {
                        AffineSymbolicExpression::from_number(v)
                    } else if self.known_cells.contains(&cell) {
                        AffineSymbolicExpression::from_known_variable(cell)
                    } else {
                        AffineSymbolicExpression::from_unknown_variable(cell)
                    }
                }
            }
            Expression::PublicReference(_) => return None, // TODO
            Expression::Challenge(_) => return None,       // TODO
            Expression::Number(n) => AffineSymbolicExpression::from_number(*n),
            Expression::BinaryOperation(op) => self.evaulate_binary_operation(op, offset)?,
            Expression::UnaryOperation(op) => self.evaluate_unary_operation(op, offset)?,
        })
    }

    fn evaulate_binary_operation(
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
                Some(AffineSymbolicExpression::from_number(result))
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
}

impl<T: FieldElement> RangeConstraintSet<Cell, T> for WitgenInference<'_, T> {
    // TODO would be nice to use &Cell, but this leads to lifetime trouble
    // in the solve() function.
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

#[cfg(test)]
mod test {
    use powdr_ast::analyzed::Analyzed;
    use powdr_number::GoldilocksField;

    use crate::witgen::{jit::affine_symbolic_expression::Assertion, FixedData};

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
                Effect::Lookup(id, args) => {
                    format!(
                        "lookup({id}, [{}]);",
                        args.iter()
                            .map(|arg| match arg {
                                LookupArgument::Known(k) => format!("Known({k})"),
                                LookupArgument::Unknown(u) => format!("Unknown({u})"),
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

    fn solve_on_rows(input: &str, rows: &[i32], known_cells: Vec<(&str, i32)>) -> String {
        let analyzed: Analyzed<GoldilocksField> =
            powdr_pil_analyzer::analyze_string(input).unwrap();
        let fixed_data = FixedData::new(&analyzed, &[], &[], Default::default(), 0);
        let known_cells = known_cells.iter().map(|(name, row_offset)| {
            let id = fixed_data.try_column_by_name(name).unwrap().id;
            Cell {
                column_name: name.to_string(),
                id,
                row_offset: *row_offset,
            }
        });
        let mut witgen = WitgenInference::new(&fixed_data, known_cells);
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
}
