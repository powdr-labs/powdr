use crate::parsed::visitor::VisitOrder;

use super::*;

impl<T> ExpressionVisitable<AlgebraicExpression<T>> for AlgebraicExpression<T> {
    fn visit_expressions_mut<F, B>(&mut self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&mut AlgebraicExpression<T>) -> ControlFlow<B>,
    {
        if o == VisitOrder::Pre {
            f(self)?;
        }
        match self {
            AlgebraicExpression::Reference(_)
            | AlgebraicExpression::PublicReference(_)
            | AlgebraicExpression::Challenge(_)
            | AlgebraicExpression::Number(_) => {}
            AlgebraicExpression::BinaryOperation(left, _, right) => {
                left.visit_expressions_mut(f, o)?;
                right.visit_expressions_mut(f, o)?;
            }
            AlgebraicExpression::UnaryOperation(_, e) => e.visit_expressions_mut(f, o)?,
        };
        if o == VisitOrder::Post {
            f(self)?;
        }
        ControlFlow::Continue(())
    }

    fn visit_expressions<F, B>(&self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&AlgebraicExpression<T>) -> ControlFlow<B>,
    {
        if o == VisitOrder::Pre {
            f(self)?;
        }
        match self {
            AlgebraicExpression::Reference(_)
            | AlgebraicExpression::PublicReference(_)
            | AlgebraicExpression::Challenge(_)
            | AlgebraicExpression::Number(_) => {}
            AlgebraicExpression::BinaryOperation(left, _, right) => {
                left.visit_expressions(f, o)?;
                right.visit_expressions(f, o)?;
            }
            AlgebraicExpression::UnaryOperation(_, e) => e.visit_expressions(f, o)?,
        };
        if o == VisitOrder::Post {
            f(self)?;
        }
        ControlFlow::Continue(())
    }
}
