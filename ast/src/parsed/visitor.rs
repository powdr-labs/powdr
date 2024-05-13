use std::{iter::once, ops::ControlFlow};

use super::{
    ArrayExpression, ArrayLiteral, Expression, FunctionCall, FunctionDefinition, IfExpression,
    IndexAccess, LambdaExpression, MatchArm, MatchPattern, NamespacedPolynomialReference,
    PilStatement, SelectedExpressions,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum VisitOrder {
    Pre,
    Post,
}

/// A trait to be implemented by an AST node.
/// The idea is that it calls a callback function on each of the sub-nodes
/// that are expressions.
pub trait ExpressionVisitable<Expr> {
    /// Traverses the AST and calls `f` on each Expression in pre-order,
    /// potentially break early and return a value.
    fn pre_visit_expressions_return_mut<F, B>(&mut self, f: &mut F) -> ControlFlow<B>
    where
        F: FnMut(&mut Expr) -> ControlFlow<B>,
    {
        self.visit_expressions_mut(f, VisitOrder::Pre)
    }

    /// Traverses the AST and calls `f` on each Expression in pre-order.
    fn pre_visit_expressions_mut<F>(&mut self, f: &mut F)
    where
        F: FnMut(&mut Expr),
    {
        self.pre_visit_expressions_return_mut(&mut move |e| {
            f(e);
            ControlFlow::Continue::<()>(())
        });
    }

    /// Traverses the AST and calls `f` on each Expression in pre-order,
    /// potentially break early and return a value.
    fn pre_visit_expressions_return<F, B>(&self, f: &mut F) -> ControlFlow<B>
    where
        F: FnMut(&Expr) -> ControlFlow<B>,
    {
        self.visit_expressions(f, VisitOrder::Pre)
    }

    /// Traverses the AST and calls `f` on each Expression in pre-order.
    fn pre_visit_expressions<F>(&self, f: &mut F)
    where
        F: FnMut(&Expr),
    {
        self.pre_visit_expressions_return(&mut move |e| {
            f(e);
            ControlFlow::Continue::<()>(())
        });
    }

    /// Traverses the AST and calls `f` on each Expression in post-order,
    /// potentially break early and return a value.
    fn post_visit_expressions_return_mut<F, B>(&mut self, f: &mut F) -> ControlFlow<B>
    where
        F: FnMut(&mut Expr) -> ControlFlow<B>,
    {
        self.visit_expressions_mut(f, VisitOrder::Post)
    }

    /// Traverses the AST and calls `f` on each Expression in post-order.
    fn post_visit_expressions_mut<F>(&mut self, f: &mut F)
    where
        F: FnMut(&mut Expr),
    {
        self.post_visit_expressions_return_mut(&mut move |e| {
            f(e);
            ControlFlow::Continue::<()>(())
        });
    }

    /// Traverses the AST and calls `f` on each Expression in post-order,
    /// potentially break early and return a value.
    fn post_visit_expressions_return<F, B>(&self, f: &mut F) -> ControlFlow<B>
    where
        F: FnMut(&Expr) -> ControlFlow<B>,
    {
        self.visit_expressions(f, VisitOrder::Post)
    }

    /// Traverses the AST and calls `f` on each Expression in post-order.
    fn post_visit_expressions<F>(&self, f: &mut F)
    where
        F: FnMut(&Expr),
    {
        self.post_visit_expressions_return(&mut move |e| {
            f(e);
            ControlFlow::Continue::<()>(())
        });
    }

    fn visit_expressions<F, B>(&self, f: &mut F, order: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&Expr) -> ControlFlow<B>;

    fn visit_expressions_mut<F, B>(&mut self, f: &mut F, order: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&mut Expr) -> ControlFlow<B>;
}

impl<T, Ref> ExpressionVisitable<Expression<T, Ref>> for Expression<T, Ref> {
    fn visit_expressions_mut<F, B>(&mut self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&mut Expression<T, Ref>) -> ControlFlow<B>,
    {
        if o == VisitOrder::Pre {
            f(self)?;
        }
        match self {
            Expression::Reference(_)
            | Expression::PublicReference(_)
            | Expression::Number(_)
            | Expression::String(_) => {}
            Expression::BinaryOperation(left, _, right) => {
                left.visit_expressions_mut(f, o)?;
                right.visit_expressions_mut(f, o)?;
            }
            Expression::FreeInput(e) | Expression::UnaryOperation(_, e) => {
                e.visit_expressions_mut(f, o)?
            }
            Expression::LambdaExpression(lambda) => lambda.visit_expressions_mut(f, o)?,
            Expression::ArrayLiteral(array_literal) => array_literal.visit_expressions_mut(f, o)?,
            Expression::IndexAccess(index_access) => index_access.visit_expressions_mut(f, o)?,
            Expression::FunctionCall(function) => function.visit_expressions_mut(f, o)?,
            Expression::Tuple(items) => items
                .iter_mut()
                .try_for_each(|item| item.visit_expressions_mut(f, o))?,
            Expression::MatchExpression(scrutinee, arms) => {
                scrutinee.visit_expressions_mut(f, o)?;
                arms.iter_mut()
                    .try_for_each(|arm| arm.visit_expressions_mut(f, o))?;
            }
            Expression::IfExpression(if_expr) => if_expr.visit_expressions_mut(f, o)?,
        };
        if o == VisitOrder::Post {
            f(self)?;
        }
        ControlFlow::Continue(())
    }

    fn visit_expressions<F, B>(&self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&Expression<T, Ref>) -> ControlFlow<B>,
    {
        if o == VisitOrder::Pre {
            f(self)?;
        }
        match self {
            Expression::Reference(_)
            | Expression::PublicReference(_)
            | Expression::Number(_)
            | Expression::String(_) => {}
            Expression::BinaryOperation(left, _, right) => {
                left.visit_expressions(f, o)?;
                right.visit_expressions(f, o)?;
            }
            Expression::FreeInput(e) | Expression::UnaryOperation(_, e) => {
                e.visit_expressions(f, o)?
            }
            Expression::LambdaExpression(lambda) => lambda.visit_expressions(f, o)?,
            Expression::ArrayLiteral(array_literal) => array_literal.visit_expressions(f, o)?,
            Expression::IndexAccess(index_access) => index_access.visit_expressions(f, o)?,
            Expression::FunctionCall(function) => function.visit_expressions(f, o)?,
            Expression::Tuple(items) => items
                .iter()
                .try_for_each(|item| item.visit_expressions(f, o))?,
            Expression::MatchExpression(scrutinee, arms) => {
                scrutinee.visit_expressions(f, o)?;
                arms.iter()
                    .try_for_each(|arm| arm.visit_expressions(f, o))?;
            }
            Expression::IfExpression(if_expr) => if_expr.visit_expressions(f, o)?,
        };
        if o == VisitOrder::Post {
            f(self)?;
        }
        ControlFlow::Continue(())
    }
}

impl<T> ExpressionVisitable<Expression<T, NamespacedPolynomialReference>> for PilStatement<T> {
    fn visit_expressions_mut<F, B>(&mut self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&mut Expression<T, NamespacedPolynomialReference>) -> ControlFlow<B>,
    {
        match self {
            PilStatement::Expression(_, e) => e.visit_expressions_mut(f, o),
            PilStatement::PlookupIdentity(_, _, left, right)
            | PilStatement::PermutationIdentity(
                _, //
                _, //
                left,
                right,
            ) => [left, right] //
                .into_iter()
                .try_for_each(|e| e.visit_expressions_mut(f, o)),
            PilStatement::ConnectIdentity(_start, left, right) => left
                .iter_mut()
                .chain(right.iter_mut())
                .try_for_each(|e| e.visit_expressions_mut(f, o)),

            PilStatement::Namespace(_, _, e)
            | PilStatement::PolynomialDefinition(_, _, e)
            | PilStatement::PolynomialIdentity(_, _, e)
            | PilStatement::PublicDeclaration(_, _, _, None, e)
            | PilStatement::ConstantDefinition(_, _, e)
            | PilStatement::LetStatement(_, _, Some(e)) => e.visit_expressions_mut(f, o),

            PilStatement::PublicDeclaration(_, _, _, Some(i), e) => [i, e]
                .into_iter()
                .try_for_each(|e| e.visit_expressions_mut(f, o)),

            PilStatement::PolynomialConstantDefinition(_, _, fundef)
            | PilStatement::PolynomialCommitDeclaration(_, _, Some(fundef), _) => {
                fundef.visit_expressions_mut(f, o)
            }
            PilStatement::PolynomialCommitDeclaration(_, _, None, _)
            | PilStatement::Include(_, _)
            | PilStatement::PolynomialConstantDeclaration(_, _)
            | PilStatement::LetStatement(_, _, None) => ControlFlow::Continue(()),
        }
    }

    fn visit_expressions<F, B>(&self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&Expression<T>) -> ControlFlow<B>,
    {
        match self {
            PilStatement::Expression(_, e) => e.visit_expressions(f, o),
            PilStatement::PlookupIdentity(_, _, left, right)
            | PilStatement::PermutationIdentity(
                _, //
                _, //
                left,
                right,
            ) => [left, right] //
                .into_iter()
                .try_for_each(|e| e.visit_expressions(f, o)),
            PilStatement::ConnectIdentity(_start, left, right) => left
                .iter()
                .chain(right.iter())
                .try_for_each(|e| e.visit_expressions(f, o)),

            PilStatement::Namespace(_, _, e)
            | PilStatement::PolynomialDefinition(_, _, e)
            | PilStatement::PolynomialIdentity(_, _, e)
            | PilStatement::PublicDeclaration(_, _, _, None, e)
            | PilStatement::ConstantDefinition(_, _, e)
            | PilStatement::LetStatement(_, _, Some(e)) => e.visit_expressions(f, o),

            PilStatement::PublicDeclaration(_, _, _, Some(i), e) => [i, e]
                .into_iter()
                .try_for_each(|e| e.visit_expressions(f, o)),

            PilStatement::PolynomialConstantDefinition(_, _, fundef)
            | PilStatement::PolynomialCommitDeclaration(_, _, Some(fundef), _) => {
                fundef.visit_expressions(f, o)
            }
            PilStatement::PolynomialCommitDeclaration(_, _, None, _)
            | PilStatement::Include(_, _)
            | PilStatement::PolynomialConstantDeclaration(_, _)
            | PilStatement::LetStatement(_, _, None) => ControlFlow::Continue(()),
        }
    }
}

impl<Expr: ExpressionVisitable<Expr>> ExpressionVisitable<Expr> for SelectedExpressions<Expr> {
    fn visit_expressions_mut<F, B>(&mut self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&mut Expr) -> ControlFlow<B>,
    {
        self.selector
            .as_mut()
            .into_iter()
            .chain(self.expressions.iter_mut())
            .try_for_each(move |item| item.visit_expressions_mut(f, o))
    }

    fn visit_expressions<F, B>(&self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&Expr) -> ControlFlow<B>,
    {
        self.selector
            .as_ref()
            .into_iter()
            .chain(self.expressions.iter())
            .try_for_each(move |item| item.visit_expressions(f, o))
    }
}

impl<T> ExpressionVisitable<Expression<T>> for FunctionDefinition<T> {
    fn visit_expressions_mut<F, B>(&mut self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&mut Expression<T>) -> ControlFlow<B>,
    {
        match self {
            FunctionDefinition::Query(_, e) => e.visit_expressions_mut(f, o),
            FunctionDefinition::Array(ae) => ae.visit_expressions_mut(f, o),
            FunctionDefinition::Expression(e) => e.visit_expressions_mut(f, o),
        }
    }

    fn visit_expressions<F, B>(&self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&Expression<T>) -> ControlFlow<B>,
    {
        match self {
            FunctionDefinition::Query(_, e) => e.visit_expressions(f, o),
            FunctionDefinition::Array(ae) => ae.visit_expressions(f, o),
            FunctionDefinition::Expression(e) => e.visit_expressions(f, o),
        }
    }
}

impl<T> ExpressionVisitable<Expression<T>> for ArrayExpression<T> {
    fn visit_expressions_mut<F, B>(&mut self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&mut Expression<T>) -> ControlFlow<B>,
    {
        match self {
            ArrayExpression::Value(expressions) | ArrayExpression::RepeatedValue(expressions) => {
                expressions
                    .iter_mut()
                    .try_for_each(|e| e.visit_expressions_mut(f, o))
            }
            ArrayExpression::Concat(a1, a2) => [a1, a2]
                .iter_mut()
                .try_for_each(|e| e.visit_expressions_mut(f, o)),
        }
    }

    fn visit_expressions<F, B>(&self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&Expression<T>) -> ControlFlow<B>,
    {
        match self {
            ArrayExpression::Value(expressions) | ArrayExpression::RepeatedValue(expressions) => {
                expressions
                    .iter()
                    .try_for_each(|e| e.visit_expressions(f, o))
            }
            ArrayExpression::Concat(a1, a2) => {
                [a1, a2].iter().try_for_each(|e| e.visit_expressions(f, o))
            }
        }
    }
}

impl<T, Ref> ExpressionVisitable<Expression<T, Ref>> for LambdaExpression<T, Ref> {
    fn visit_expressions_mut<F, B>(&mut self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&mut Expression<T, Ref>) -> ControlFlow<B>,
    {
        self.body.visit_expressions_mut(f, o)
    }

    fn visit_expressions<F, B>(&self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&Expression<T, Ref>) -> ControlFlow<B>,
    {
        self.body.visit_expressions(f, o)
    }
}

impl<T, Ref> ExpressionVisitable<Expression<T, Ref>> for ArrayLiteral<T, Ref> {
    fn visit_expressions_mut<F, B>(&mut self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&mut Expression<T, Ref>) -> ControlFlow<B>,
    {
        self.items
            .iter_mut()
            .try_for_each(|item| item.visit_expressions_mut(f, o))
    }

    fn visit_expressions<F, B>(&self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&Expression<T, Ref>) -> ControlFlow<B>,
    {
        self.items
            .iter()
            .try_for_each(|item| item.visit_expressions(f, o))
    }
}

impl<T, Ref> ExpressionVisitable<Expression<T, Ref>> for IndexAccess<T, Ref> {
    fn visit_expressions_mut<F, B>(&mut self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&mut Expression<T, Ref>) -> ControlFlow<B>,
    {
        self.array.visit_expressions_mut(f, o)?;
        self.index.visit_expressions_mut(f, o)
    }

    fn visit_expressions<F, B>(&self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&Expression<T, Ref>) -> ControlFlow<B>,
    {
        self.array.visit_expressions(f, o)?;
        self.index.visit_expressions(f, o)
    }
}

impl<T, Ref> ExpressionVisitable<Expression<T, Ref>> for FunctionCall<T, Ref> {
    fn visit_expressions_mut<F, B>(&mut self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&mut Expression<T, Ref>) -> ControlFlow<B>,
    {
        once(self.function.as_mut())
            .chain(&mut self.arguments)
            .try_for_each(|item| item.visit_expressions_mut(f, o))
    }

    fn visit_expressions<F, B>(&self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&Expression<T, Ref>) -> ControlFlow<B>,
    {
        once(self.function.as_ref())
            .chain(&self.arguments)
            .try_for_each(|item| item.visit_expressions(f, o))
    }
}

impl<T, Ref> ExpressionVisitable<Expression<T, Ref>> for MatchArm<T, Ref> {
    fn visit_expressions_mut<F, B>(&mut self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&mut Expression<T, Ref>) -> ControlFlow<B>,
    {
        self.pattern.visit_expressions_mut(f, o)?;
        self.value.visit_expressions_mut(f, o)
    }

    fn visit_expressions<F, B>(&self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&Expression<T, Ref>) -> ControlFlow<B>,
    {
        self.pattern.visit_expressions(f, o)?;
        self.value.visit_expressions(f, o)
    }
}

impl<T, Ref> ExpressionVisitable<Expression<T, Ref>> for MatchPattern<T, Ref> {
    fn visit_expressions_mut<F, B>(&mut self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&mut Expression<T, Ref>) -> ControlFlow<B>,
    {
        match self {
            MatchPattern::CatchAll => ControlFlow::Continue(()),
            MatchPattern::Pattern(e) => e.visit_expressions_mut(f, o),
        }
    }

    fn visit_expressions<F, B>(&self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&Expression<T, Ref>) -> ControlFlow<B>,
    {
        match self {
            MatchPattern::CatchAll => ControlFlow::Continue(()),
            MatchPattern::Pattern(e) => e.visit_expressions(f, o),
        }
    }
}

impl<T, Ref> ExpressionVisitable<Expression<T, Ref>> for IfExpression<T, Ref> {
    fn visit_expressions_mut<F, B>(&mut self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&mut Expression<T, Ref>) -> ControlFlow<B>,
    {
        [&mut self.condition, &mut self.body, &mut self.else_body]
            .into_iter()
            .try_for_each(|e| e.visit_expressions_mut(f, o))
    }

    fn visit_expressions<F, B>(&self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&Expression<T, Ref>) -> ControlFlow<B>,
    {
        [&self.condition, &self.body, &self.else_body]
            .into_iter()
            .try_for_each(|e| e.visit_expressions(f, o))
    }
}
