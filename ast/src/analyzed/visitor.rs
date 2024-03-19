use crate::parsed::visitor::VisitOrder;

use self::parsed::visitor::Children;

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

impl<Expr> Children<Expr> for Identity<Expr> {
    fn children_mut(&mut self) -> Box<dyn Iterator<Item = &mut Expr> + '_> {
        Box::new(
            self.left
                .selector
                .as_mut()
                .into_iter()
                .chain(self.left.expressions.iter_mut())
                .chain(self.right.selector.as_mut())
                .chain(self.right.expressions.iter_mut()),
        )
    }

    fn children(&self) -> Box<dyn Iterator<Item = &Expr> + '_> {
        Box::new(
            self.left
                .selector
                .as_ref()
                .into_iter()
                .chain(self.left.expressions.iter())
                .chain(self.right.selector.iter())
                .chain(self.right.expressions.iter()),
        )
    }
}

impl ExpressionVisitable<Expression> for FunctionValueDefinition {
    fn visit_expressions_mut<F, B>(&mut self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&mut Expression) -> ControlFlow<B>,
    {
        match self {
            FunctionValueDefinition::Query(e)
            | FunctionValueDefinition::Expression(TypedExpression { e, type_scheme: _ }) => {
                e.visit_expressions_mut(f, o)
            }
            FunctionValueDefinition::Array(array) => array
                .iter_mut()
                .try_for_each(move |item| item.visit_expressions_mut(f, o)),
            FunctionValueDefinition::TypeDeclaration(enum_declaration) => enum_declaration
                .children_mut()
                .try_for_each(move |item| item.visit_expressions_mut(f, o)),
            FunctionValueDefinition::TypeConstructor(_, variant) => variant
                .children_mut()
                .try_for_each(move |item| item.visit_expressions_mut(f, o)),
        }
    }

    fn visit_expressions<F, B>(&self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&Expression) -> ControlFlow<B>,
    {
        match self {
            FunctionValueDefinition::Query(e)
            | FunctionValueDefinition::Expression(TypedExpression { e, type_scheme: _ }) => {
                e.visit_expressions(f, o)
            }
            FunctionValueDefinition::Array(array) => array
                .iter()
                .try_for_each(move |item| item.visit_expressions(f, o)),
            FunctionValueDefinition::TypeDeclaration(enum_declaration) => enum_declaration
                .children()
                .try_for_each(move |item| item.visit_expressions(f, o)),
            FunctionValueDefinition::TypeConstructor(_, variant) => variant
                .children()
                .try_for_each(move |item| item.visit_expressions(f, o)),
        }
    }
}

impl ExpressionVisitable<Expression> for RepeatedArray {
    fn visit_expressions_mut<F, B>(&mut self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&mut Expression) -> ControlFlow<B>,
    {
        self.pattern
            .iter_mut()
            .try_for_each(move |item| item.visit_expressions_mut(f, o))
    }

    fn visit_expressions<F, B>(&self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&Expression) -> ControlFlow<B>,
    {
        self.pattern
            .iter()
            .try_for_each(move |item| item.visit_expressions(f, o))
    }
}
