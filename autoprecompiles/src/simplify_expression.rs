use powdr_expression::{
    visitors::ExpressionVisitable, AlgebraicBinaryOperation, AlgebraicBinaryOperator,
    AlgebraicUnaryOperation, AlgebraicUnaryOperator,
};
use powdr_number::FieldElement;

use crate::legacy_expression::AlgebraicExpression;

pub fn simplify_expression<T: FieldElement>(
    mut e: AlgebraicExpression<T>,
) -> AlgebraicExpression<T> {
    e.post_visit_expressions_mut(&mut simplify_expression_single);
    e
}

fn simplify_expression_single<T: FieldElement>(e: &mut AlgebraicExpression<T>) {
    if let AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation { left, op, right }) = e {
        if let (AlgebraicExpression::Number(l), AlgebraicExpression::Number(r)) =
            (left.as_ref(), right.as_ref())
        {
            // TODO: Make this pretty
            if let Some(v) = match op {
                AlgebraicBinaryOperator::Add => Some(*l + *r),
                AlgebraicBinaryOperator::Sub => Some(*l - *r),
                AlgebraicBinaryOperator::Mul => Some(*l * *r),
            } {
                *e = AlgebraicExpression::Number(v);
                return;
            }
        }
    }
    if let AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperation { op, expr: inner }) = e {
        if let AlgebraicExpression::Number(inner) = **inner {
            *e = AlgebraicExpression::Number(match op {
                AlgebraicUnaryOperator::Minus => -inner,
            });
            return;
        }
    }

    if let AlgebraicExpression::BinaryOperation(_) = e {
        try_simplify_associative_operation(e);
    }

    match e {
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
            left,
            op: AlgebraicBinaryOperator::Mul,
            right,
        }) => {
            if let AlgebraicExpression::Number(n) = left.as_mut() {
                if *n == 0.into() {
                    *e = AlgebraicExpression::Number(0.into());
                    return;
                }
            }
            if let AlgebraicExpression::Number(n) = right.as_mut() {
                if *n == 0.into() {
                    *e = AlgebraicExpression::Number(0.into());
                    return;
                }
            }
            if let AlgebraicExpression::Number(n) = left.as_mut() {
                if *n == 1.into() {
                    let mut tmp = AlgebraicExpression::Number(1.into());
                    std::mem::swap(&mut tmp, right);
                    std::mem::swap(e, &mut tmp);
                    return;
                }
            }
            if let AlgebraicExpression::Number(n) = right.as_mut() {
                if *n == 1.into() {
                    let mut tmp = AlgebraicExpression::Number(1.into());
                    std::mem::swap(&mut tmp, left);
                    std::mem::swap(e, &mut tmp);
                }
            }
        }
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
            left,
            op: AlgebraicBinaryOperator::Add,
            right,
        }) => {
            if let AlgebraicExpression::Number(n) = left.as_mut() {
                if *n == 0.into() {
                    let mut tmp = AlgebraicExpression::Number(1.into());
                    std::mem::swap(&mut tmp, right);
                    std::mem::swap(e, &mut tmp);
                    return;
                }
            }
            if let AlgebraicExpression::Number(n) = right.as_mut() {
                if *n == 0.into() {
                    let mut tmp = AlgebraicExpression::Number(1.into());
                    std::mem::swap(&mut tmp, left);
                    std::mem::swap(e, &mut tmp);
                }
            }
        }
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
            left,
            op: AlgebraicBinaryOperator::Sub,
            right,
        }) => {
            if let AlgebraicExpression::Number(n) = right.as_mut() {
                if *n == 0.into() {
                    let mut tmp = AlgebraicExpression::Number(1.into());
                    std::mem::swap(&mut tmp, left);
                    std::mem::swap(e, &mut tmp);
                }
            }
        }
        _ => {}
    }
}

fn try_simplify_associative_operation<T: FieldElement>(e: &mut AlgebraicExpression<T>) {
    if let AlgebraicExpression::BinaryOperation(binary_op) = e {
        if binary_op.op != AlgebraicBinaryOperator::Add {
            return;
        }

        // Find binary operation and other expression, handling both orderings:
        // (X1 + X2) + Other
        // Other + (X1 + X2)
        let (x1, x2, other_expr) = match (&mut *binary_op.left, &mut *binary_op.right) {
            (
                AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
                    left: x1,
                    right: x2,
                    op: AlgebraicBinaryOperator::Add,
                }),
                other,
            ) => (x1, x2, other),
            (
                other,
                AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
                    left: x1,
                    right: x2,
                    op: AlgebraicBinaryOperator::Add,
                }),
            ) => (x1, x2, other),
            _ => return,
        };
        // Now we have "binary_op = x1 + x2 + other_expr".

        // Extract variable and constant from binary operation, handling both orderings:
        // (X1 + C1) -> (X1, C1) if X2 is a constant
        // (C1 + X2) -> (X2, C1) if X1 is a constant
        let (x, c1_val) = if let AlgebraicExpression::Number(val) = x1.as_ref() {
            (x2.as_mut(), val)
        } else if let AlgebraicExpression::Number(val) = x2.as_ref() {
            (x1.as_mut(), val)
        } else {
            return;
        };
        // Now we have "binary_op = x + c1_val + other"

        let x = std::mem::replace(x, AlgebraicExpression::Number(0.into()));
        match other_expr {
            // Case 1: Combining with a constant
            // X + c1_val + Other -> X + (c1_val + Other)
            AlgebraicExpression::Number(c2) => {
                *e = x + AlgebraicExpression::Number(*c1_val + *c2);
            }

            // Case 2: Combining with any non-numeric expression
            // (X + c1_val) + Y -> (X + Y) + c1_val
            y => {
                let y = std::mem::replace(y, AlgebraicExpression::Number(0.into()));
                *e = x + y + AlgebraicExpression::Number(*c1_val);
            }
        }
    }
}
