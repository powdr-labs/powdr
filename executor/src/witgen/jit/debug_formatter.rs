use itertools::Itertools;
use powdr_ast::analyzed::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression as Expression,
    AlgebraicUnaryOperation, PolynomialIdentity, SelectedExpressions,
};
use powdr_constraint_solver::{
    range_constraint::RangeConstraint, runtime_constant::RuntimeConstant,
};
use powdr_number::FieldElement;

use crate::witgen::data_structures::identity::{BusSend, Identity};

use super::{
    variable::Variable,
    witgen_inference::{FixedEvaluator, WitgenInference},
};

/// Returns a human-readable summary of the polynomial identities.
pub fn format_polynomial_identities<T: FieldElement, FixedEval: FixedEvaluator<T>>(
    identities: &[(&Identity<T>, i32)],
    witgen: &WitgenInference<'_, T, FixedEval>,
) -> String {
    DebugFormatter { identities, witgen }.format_polynomial_identities()
}

/// Returns a human-readable summary of incomplete bus sends.
pub fn format_incomplete_bus_sends<T: FieldElement, FixedEval: FixedEvaluator<T>>(
    identities: &[(&Identity<T>, i32)],
    witgen: &WitgenInference<'_, T, FixedEval>,
) -> String {
    DebugFormatter { identities, witgen }.format_incomplete_bus_sends()
}

struct DebugFormatter<'a, T: FieldElement, FixedEval: FixedEvaluator<T>> {
    identities: &'a [(&'a Identity<T>, i32)],
    witgen: &'a WitgenInference<'a, T, FixedEval>,
}

impl<T: FieldElement, FixedEval: FixedEvaluator<T>> DebugFormatter<'_, T, FixedEval> {
    fn format_polynomial_identities(&self) -> String {
        self.identities
            .iter()
            .filter(|(id, _)| matches!(id, Identity::Polynomial(_)))
            .sorted_by_key(|(id, row)| (row, id.id()))
            .flat_map(|(id, row)| {
                let (skip, conflicting) = match &id {
                    Identity::Polynomial(PolynomialIdentity { expression, .. }) => {
                        let value = self
                            .witgen
                            .evaluate(expression, *row, false)
                            .try_to_known()
                            .cloned();
                        let conflict = value
                            .as_ref()
                            .and_then(|v| v.try_to_number().map(|n| n != 0.into()))
                            .unwrap_or(false);
                        // We can skip the identity if it does not have unknown variables
                        // but only if there is no conflict.
                        (value.is_some() && !conflict, conflict)
                    }
                    _ => unreachable!(),
                };
                if skip {
                    None
                } else {
                    Some(format!(
                        "{}--------------[ identity {} on row {row}: ]--------------\n{}",
                        if conflicting {
                            "--------------[ !!! CONFLICT !!! ]--------------\n"
                        } else {
                            ""
                        },
                        id.id(),
                        self.format_identity(id, *row)
                    ))
                }
            })
            .join("\n")
    }

    fn format_incomplete_bus_sends(&self) -> String {
        self.identities
            .iter()
            .filter(|(id, row)| match id {
                Identity::BusSend(..) => !self.witgen.is_complete_call(id, *row),
                Identity::Connect(..) => true,
                Identity::Polynomial(..) => false,
            })
            .sorted_by_key(|(id, row)| (row, id.id()))
            .map(|(id, row)| {
                format!(
                    "--------------[ identity {} on row {row}: ]--------------\n{}",
                    id.id(),
                    self.format_identity(id, *row)
                )
            })
            .join("\n")
    }

    /// Formats the identity in a human-readable way to contain as much information
    /// about the sub-expressions as possible.
    fn format_identity(&self, identity: &Identity<T>, row_offset: i32) -> String {
        match identity {
            Identity::BusSend(BusSend {
                selected_payload, ..
            }) => self.format_bus_send(selected_payload, row_offset),
            Identity::Connect(_) => {
                format!("{identity}")
            }
            Identity::Polynomial(PolynomialIdentity { expression, .. }) => {
                if let Expression::BinaryOperation(AlgebraicBinaryOperation {
                    left,
                    op: AlgebraicBinaryOperator::Sub,
                    right,
                }) = expression
                {
                    self.format_polynomial_identity(left, right, row_offset)
                } else {
                    self.format_polynomial_identity(
                        expression,
                        &Expression::Number(0.into()),
                        row_offset,
                    )
                }
            }
        }
    }

    fn format_bus_send(
        &self,
        selected_payload: &SelectedExpressions<T>,
        row_offset: i32,
    ) -> String {
        let sel =
            self.format_expression_full_and_simplified(&selected_payload.selector, row_offset);
        let exprs = selected_payload
            .expressions
            .iter()
            .map(|e| self.format_expression_full_and_simplified(e, row_offset))
            .reduce(|a, b| {
                let lines = a.into_iter().zip(b).enumerate();
                lines
                    .map(|(i, (a, b))| {
                        let comma = if i % 3 == 0 { "," } else { " " };
                        format!("{a}{comma} {b}")
                    })
                    .collect_vec()
                    .try_into()
                    .unwrap()
            })
            .unwrap();
        sel.into_iter()
            .zip(exprs)
            .enumerate()
            .map(|(i, (sel, exprs))| {
                if i % 3 == 0 {
                    format!("{sel} $ [ {exprs} ]")
                } else {
                    format!("{sel}     {exprs}  ")
                }
            })
            .format("\n")
            .to_string()
    }

    fn format_polynomial_identity(
        &self,
        left: &Expression<T>,
        right: &Expression<T>,
        row: i32,
    ) -> String {
        let left = self.format_expression_full_and_simplified(left, row);
        let right = self.format_expression_full_and_simplified(right, row);
        left.into_iter()
            .zip(right)
            .map(|(l, r)| format!("{l} = {r}"))
            .format("\n")
            .to_string()
    }

    fn format_expression_full_and_simplified(
        &self,
        e: &Expression<T>,
        row_offset: i32,
    ) -> [String; 6] {
        let full = self.format_expression(e, row_offset, false);
        let simplified = self.format_expression(e, row_offset, true);
        [full, simplified].concat().try_into().unwrap()
    }

    /// Returns three formatted strings of the same length.
    /// The first is the expression without any substitutions.
    /// The second is the expression with known variables substituted.
    /// The third is the expression with range constraints substituted.
    /// If `simplified` is true, tries to simplify sub-expressions like multiplication by
    /// zero or one or addition of zero.
    fn format_expression(
        &self,
        e: &Expression<T>,
        row_offset: i32,
        simplified: bool,
    ) -> [String; 3] {
        if simplified {
            if let Some(e) = self.try_to_known(e, row_offset) {
                return pad_center([format!("{e}"), String::new(), String::new()]);
            }
        }
        let [name, value, rc] = match e {
            Expression::Reference(r) => {
                let (value, range_constraint) = {
                    let variable = Variable::from_reference(r, row_offset);
                    let value = self.witgen.value(&variable).to_string();
                    let rc = self.witgen.range_constraint(&variable);
                    let rc =
                        if rc == RangeConstraint::default() || rc.try_to_single_value().is_some() {
                            // Empty string also for single value, since it is already
                            // printed in the "value" line.
                            String::new()
                        } else {
                            rc.to_string()
                        };
                    (value, rc)
                };
                [r.to_string(), value, range_constraint]
            }
            Expression::PublicReference(_) => {
                // TODO we need to introduce a variable type for those.
                [format!("{e}"), "<public>".to_string(), String::new()]
            }
            Expression::Challenge(_) => {
                // TODO we need to introduce a variable type for those.
                [format!("{e}"), "<challenge>".to_string(), String::new()]
            }
            Expression::Number(n) => [format!("{n}"), String::new(), String::new()],
            Expression::BinaryOperation(op) => {
                self.format_binary_operation(op, row_offset, simplified)
            }
            Expression::UnaryOperation(op) => {
                self.format_unary_operation(op, row_offset, simplified)
            }
        };
        pad_center([name, value, rc])
    }

    fn format_binary_operation(
        &self,
        op: &AlgebraicBinaryOperation<T>,
        row_offset: i32,
        simplified: bool,
    ) -> [String; 3] {
        // TODO precedence

        if simplified {
            if let Some(s) = self.try_format_binary_operation_simplified(op, row_offset) {
                return s;
            }
        }
        let [left, right] = [
            self.format_expression(&op.left, row_offset, simplified),
            self.format_expression(&op.right, row_offset, simplified),
        ];

        left.into_iter()
            .zip(right)
            .enumerate()
            .map(|(i, (l, r))| {
                if i == 0 {
                    format!("({l} {} {r})", op.op)
                } else {
                    format!(" {l}   {r} ")
                }
            })
            .collect_vec()
            .try_into()
            .unwrap()
    }

    fn try_format_binary_operation_simplified(
        &self,
        op: &AlgebraicBinaryOperation<T>,
        row_offset: i32,
    ) -> Option<[String; 3]> {
        let left = self.try_to_known(&op.left, row_offset);
        let right = self.try_to_known(&op.right, row_offset);

        match op.op {
            AlgebraicBinaryOperator::Add => {
                if left.map(|v| v == 0.into()).unwrap_or(false) {
                    Some(self.format_expression(&op.right, row_offset, true))
                } else if right.map(|v| v == 0.into()).unwrap_or(false) {
                    Some(self.format_expression(&op.left, row_offset, true))
                } else {
                    None
                }
            }
            AlgebraicBinaryOperator::Sub => {
                if left.map(|v| v == 0.into()).unwrap_or(false) {
                    Some(
                        self.format_expression(&op.right, row_offset, true)
                            .into_iter()
                            .map(|s| format!("-{s}"))
                            .collect_vec()
                            .try_into()
                            .unwrap(),
                    )
                } else if right.map(|v| v == 0.into()).unwrap_or(false) {
                    Some(self.format_expression(&op.left, row_offset, true))
                } else {
                    None
                }
            }
            AlgebraicBinaryOperator::Mul => {
                // We do not need to consider multiplication by zero, because
                // this case should have been formatted as zero already higher
                // up in the call chain due to the calls to `try_to_known`.
                if left.map(|v| v == 1.into()).unwrap_or(false) {
                    Some(self.format_expression(&op.right, row_offset, true))
                } else if right.map(|v| v == 1.into()).unwrap_or(false) {
                    Some(self.format_expression(&op.left, row_offset, true))
                } else {
                    None
                }
            }
            AlgebraicBinaryOperator::Pow => None,
        }
    }

    fn format_unary_operation(
        &self,
        op: &AlgebraicUnaryOperation<T>,
        row_offset: i32,
        simplified: bool,
    ) -> [String; 3] {
        // TODO precedence
        let inner = self.format_expression(&op.expr, row_offset, simplified);
        if op.op.is_prefix() {
            inner.into_iter().map(|s| format!("{op}{s}")).collect_vec()
        } else {
            inner.into_iter().map(|s| format!("{s}{op}")).collect_vec()
        }
        .try_into()
        .unwrap()
    }

    fn try_to_known(&self, e: &Expression<T>, row_offset: i32) -> Option<T> {
        self.witgen.try_evaluate_to_known_number(e, row_offset)
    }
}

/// Pads the strings with spaces to the left and right so that they have the same length.
fn pad_center<const N: usize>(s: [String; N]) -> [String; N] {
    let len = s.iter().map(|s| s.len()).max().unwrap();
    s.iter()
        .map(|s| format!("{s:^len$}"))
        .collect_vec()
        .try_into()
        .unwrap()
}
