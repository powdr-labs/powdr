use itertools::Itertools;
use powdr_ast::analyzed::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression as Expression,
    AlgebraicUnaryOperation, AlgebraicUnaryOperator, Identity, PolynomialIdentity, PolynomialType,
};
use powdr_number::FieldElement;

use crate::witgen::range_constraints::RangeConstraint;

use super::{
    variable::Variable,
    witgen_inference::{FixedEvaluator, Value, WitgenInference},
};

/// Returns a human-readable summary of the unfinished state in JIT-witgen.
pub fn format_error_summary<T: FieldElement, FixedEval: FixedEvaluator<T>>(
    identities: &[(&Identity<T>, i32)],
    witgen: &WitgenInference<'_, T, FixedEval>,
    fixed_evaluator: FixedEval,
) -> String {
    DebugFormatter {
        fixed_evaluator,
        identities,
        witgen,
    }
    .format_error_summary()
}

struct DebugFormatter<'a, T: FieldElement, FixedEval: FixedEvaluator<T>> {
    fixed_evaluator: FixedEval,
    identities: &'a [(&'a Identity<T>, i32)],
    witgen: &'a WitgenInference<'a, T, FixedEval>,
}

impl<T: FieldElement, FixedEval: FixedEvaluator<T>> DebugFormatter<'_, T, FixedEval> {
    fn format_error_summary(&self) -> String {
        let mut result = String::new();
        result = format!(
            "Incomplete identities:\n{}",
            self.identities
                .iter()
                .filter(|(id, row)| !self.witgen.is_complete(id, *row))
                .sorted_by_key(|(id, row)| (row, id.id()))
                // TODO group by row
                .map(|(id, row)| {
                    format!(
                        "--------------[ identity on row on row {row}: ]--------------\n{}",
                        self.formt_identity(id, *row)
                    )
                })
                .join("\n")
        );
        // TODO Also print all known variables?
        result
    }
    /// Formats the identity in a human-readable way to contain as much information
    /// about the sub-expressions as possible.
    fn formt_identity(&self, identity: &Identity<T>, row_offset: i32) -> String {
        match identity {
            Identity::Lookup(_)
            | Identity::Permutation(_)
            | Identity::PhantomPermutation(_)
            | Identity::PhantomLookup(_)
            | Identity::Connect(_)
            | Identity::PhantomBusInteraction(_) => {
                // TODO format the elements of the identity.
                // but we should only look ot the LHS.
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

    fn format_polynomial_identity(
        &self,
        left: &Expression<T>,
        right: &Expression<T>,
        row: i32,
    ) -> String {
        let left_full = self.format_expression(left, row, false);
        let right_full = self.format_expression(right, row, false);
        let left_simplified = self.format_expression(left, row, true);
        let right_simplified = self.format_expression(right, row, true);
        left_full
            .into_iter()
            .zip(right_full)
            .chain(left_simplified.into_iter().zip(right_simplified))
            .map(|(l, r)| format!("{l} = {r}"))
            .format("\n")
            .to_string()
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
        let [name, value, rc] = match e {
            Expression::Reference(r) => {
                let (value, range_constraint) = match r.poly_id.ptype {
                    PolynomialType::Constant => (
                        self.fixed_evaluator
                            .evaluate(r, row_offset)
                            .map(|v| v.to_string())
                            .unwrap_or("???".to_string()),
                        String::new(),
                    ),
                    PolynomialType::Committed => {
                        let variable = Variable::from_reference(r, row_offset);
                        let value = self.witgen.value(&variable).to_string();
                        let rc = self.witgen.range_constraint(&variable);
                        let rc = if rc == RangeConstraint::default() {
                            String::new()
                        } else {
                            rc.to_string()
                        };
                        (value, rc)
                    }
                    PolynomialType::Intermediate => {
                        // TODO we should format the contained intermediates separately.
                        ("<intermediate>".to_string(), String::new())
                    }
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
        pad_left([name, value, rc])
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
                    Some(self.format_expression(&op.right, row_offset, true))
                } else if right.map(|v| v == 0.into()).unwrap_or(false) {
                    Some(self.format_expression(&op.left, row_offset, true))
                } else {
                    None
                }
            }
            AlgebraicBinaryOperator::Mul => {
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
        match e {
            Expression::Reference(r) => {
                match r.poly_id.ptype {
                    PolynomialType::Constant => self.fixed_evaluator.evaluate(r, row_offset),
                    PolynomialType::Committed => {
                        let variable = Variable::from_reference(r, row_offset);
                        match self.witgen.value(&variable) {
                            Value::Concrete(v) => Some(v),
                            _ => None,
                        }
                    }
                    PolynomialType::Intermediate => {
                        // TODO
                        None
                    }
                }
            }
            Expression::PublicReference(_) => {
                // TODO we need to introduce a variable type for those.
                None
            }
            Expression::Challenge(_) => {
                // TODO we need to introduce a variable type for those.
                None
            }
            Expression::Number(n) => Some(*n),
            Expression::BinaryOperation(op) => {
                let left = self.try_to_known(&op.left, row_offset);
                let right = self.try_to_known(&op.right, row_offset);
                match op.op {
                    AlgebraicBinaryOperator::Add => Some(left? + right?),
                    AlgebraicBinaryOperator::Sub => Some(left? - right?),
                    AlgebraicBinaryOperator::Mul => match (left, right) {
                        (Some(a), _) | (_, Some(a)) if a == 0.into() => Some(0.into()),
                        (Some(a), b) | (b, Some(a)) if a == 1.into() => b,
                        (Some(a), b) | (b, Some(a)) if a == (-1).into() => b.map(|b| -b),
                        (Some(l), Some(r)) => Some(l * r),
                        _ => None,
                    },
                    AlgebraicBinaryOperator::Pow => Some(left?.pow(right?.to_integer())),
                }
            }
            Expression::UnaryOperation(op) => {
                let inner = self.try_to_known(&op.expr, row_offset);
                match op.op {
                    AlgebraicUnaryOperator::Minus => inner.map(|i| -i),
                }
            }
        }
    }
}

/// Pads the three strings with spaces to the left so that they have the same length.
fn pad_left(s: [String; 3]) -> [String; 3] {
    let len = s.iter().map(|s| s.len()).max().unwrap();
    s.iter()
        .map(|s| format!("{s:>len$}"))
        .collect_vec()
        .try_into()
        .unwrap()
}