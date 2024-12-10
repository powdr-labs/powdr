use std::collections::{HashMap, HashSet};

use itertools::Itertools;
use powdr_ast::analyzed::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression as Expression,
    AlgebraicReference, AlgebraicUnaryOperation, AlgebraicUnaryOperator, Identity, LookupIdentity,
    PhantomLookupIdentity, PolyID, PolynomialIdentity, PolynomialType, SelectedExpressions,
};
use powdr_number::FieldElement;

use crate::witgen::{
    global_constraints::RangeConstraintSet, jit::affine_symbolic_expression::LookupArgument,
};

use super::{
    super::{range_constraints::RangeConstraint, FixedData},
    affine_symbolic_expression::{AffineSymbolicExpression, Effect},
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

    pub fn process_identity(&mut self, id: &Identity<T>, row_offset: i32) {
        let effects = match id {
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
                vec![]
            }
        };
        self.ingest_effects(effects);
    }

    fn process_polynomial_identity(
        &self,
        expression: &'a Expression<T>,
        offset: i32,
    ) -> Vec<Effect<T, Cell>> {
        if let Some(r) = self.evaluate(expression, offset) {
            // TODO propagate or report error properly.
            // If solve returns an error, it means that the constraint is conflicting.
            // In the future, we might run this in a runtime-conditional, so an error
            // could just mean that this case cannot happen in practice.
            r.solve(self).unwrap()
        } else {
            vec![]
        }
    }

    fn process_lookup(
        &self,
        lookup_id: u64,
        left: &SelectedExpressions<T>,
        right: &SelectedExpressions<T>,
        offset: i32,
    ) -> Vec<Effect<T, Cell>> {
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
                .map(|s| s.is_known_one())
                == Some(true)
            {
                if let Some(lhs) = left
                    .expressions
                    .iter()
                    .map(|e| self.evaluate(e, offset))
                    .collect::<Option<Vec<_>>>()
                {
                    // and all except one expression is known on the LHS.
                    let unknown = lhs.iter().filter(|e| !e.is_known()).collect_vec();
                    if unknown.len() == 1 && unknown[0].single_unknown_variable().is_some() {
                        return vec![Effect::Lookup(
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
                    }
                }
            }
        }
        vec![]
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
            _ => todo!(),
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
