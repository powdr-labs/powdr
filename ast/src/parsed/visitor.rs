use std::{iter::once, ops::ControlFlow};

use super::{
    types::{ArrayType, FunctionType, TupleType, Type},
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

impl<Ref> ExpressionVisitable<Expression<Ref>> for Expression<Ref> {
    fn visit_expressions_mut<F, B>(&mut self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&mut Expression<Ref>) -> ControlFlow<B>,
    {
        if o == VisitOrder::Pre {
            f(self)?;
        }
        match self {
            Expression::Reference(_)
            | Expression::PublicReference(_)
            | Expression::Number(_, _)
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
        F: FnMut(&Expression<Ref>) -> ControlFlow<B>,
    {
        if o == VisitOrder::Pre {
            f(self)?;
        }
        match self {
            Expression::Reference(_)
            | Expression::PublicReference(_)
            | Expression::Number(_, _)
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

impl ExpressionVisitable<Expression<NamespacedPolynomialReference>> for PilStatement {
    fn visit_expressions_mut<F, B>(&mut self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&mut Expression<NamespacedPolynomialReference>) -> ControlFlow<B>,
    {
        match self {
            PilStatement::Expression(_, e) => e.visit_expressions_mut(f, o),
            PilStatement::PlookupIdentity(_, left, right)
            | PilStatement::PermutationIdentity(_, left, right) => [left, right]
                .into_iter()
                .try_for_each(|e| e.visit_expressions_mut(f, o)),
            PilStatement::ConnectIdentity(_start, left, right) => left
                .iter_mut()
                .chain(right.iter_mut())
                .try_for_each(|e| e.visit_expressions_mut(f, o)),

            PilStatement::Namespace(_, _, e)
            | PilStatement::PolynomialDefinition(_, _, e)
            | PilStatement::PublicDeclaration(_, _, _, None, e)
            | PilStatement::ConstantDefinition(_, _, e) => e.visit_expressions_mut(f, o),

            PilStatement::LetStatement(_, _, type_scheme, value) => {
                if let Some(t) = type_scheme {
                    t.ty.visit_expressions_mut(f, o)?;
                };
                if let Some(v) = value {
                    v.visit_expressions_mut(f, o)?;
                };
                ControlFlow::Continue(())
            }

            PilStatement::PublicDeclaration(_, _, _, Some(i), e) => [i, e]
                .into_iter()
                .try_for_each(|e| e.visit_expressions_mut(f, o)),

            PilStatement::PolynomialConstantDefinition(_, _, fundef)
            | PilStatement::PolynomialCommitDeclaration(_, _, Some(fundef)) => {
                fundef.visit_expressions_mut(f, o)
            }
            PilStatement::PolynomialCommitDeclaration(_, _, None)
            | PilStatement::Include(_, _)
            | PilStatement::PolynomialConstantDeclaration(_, _) => ControlFlow::Continue(()),
        }
    }

    fn visit_expressions<F, B>(&self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&Expression) -> ControlFlow<B>,
    {
        match self {
            PilStatement::Expression(_, e) => e.visit_expressions(f, o),
            PilStatement::PlookupIdentity(_, left, right)
            | PilStatement::PermutationIdentity(_, left, right) => [left, right]
                .into_iter()
                .try_for_each(|e| e.visit_expressions(f, o)),
            PilStatement::ConnectIdentity(_start, left, right) => left
                .iter()
                .chain(right.iter())
                .try_for_each(|e| e.visit_expressions(f, o)),

            PilStatement::Namespace(_, _, e)
            | PilStatement::PolynomialDefinition(_, _, e)
            | PilStatement::PublicDeclaration(_, _, _, None, e)
            | PilStatement::ConstantDefinition(_, _, e) => e.visit_expressions(f, o),

            PilStatement::LetStatement(_, _, type_scheme, value) => {
                if let Some(t) = type_scheme {
                    t.ty.visit_expressions(f, o)?;
                };
                if let Some(v) = value {
                    v.visit_expressions(f, o)?;
                };
                ControlFlow::Continue(())
            }

            PilStatement::PublicDeclaration(_, _, _, Some(i), e) => [i, e]
                .into_iter()
                .try_for_each(|e| e.visit_expressions(f, o)),

            PilStatement::PolynomialConstantDefinition(_, _, fundef)
            | PilStatement::PolynomialCommitDeclaration(_, _, Some(fundef)) => {
                fundef.visit_expressions(f, o)
            }
            PilStatement::PolynomialCommitDeclaration(_, _, None)
            | PilStatement::Include(_, _)
            | PilStatement::PolynomialConstantDeclaration(_, _) => ControlFlow::Continue(()),
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

impl ExpressionVisitable<Expression> for FunctionDefinition {
    fn visit_expressions_mut<F, B>(&mut self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&mut Expression) -> ControlFlow<B>,
    {
        match self {
            FunctionDefinition::Query(e) | FunctionDefinition::Expression(e) => {
                e.visit_expressions_mut(f, o)
            }
            FunctionDefinition::Array(ae) => ae.visit_expressions_mut(f, o),
        }
    }

    fn visit_expressions<F, B>(&self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&Expression) -> ControlFlow<B>,
    {
        match self {
            FunctionDefinition::Query(e) | FunctionDefinition::Expression(e) => {
                e.visit_expressions(f, o)
            }
            FunctionDefinition::Array(ae) => ae.visit_expressions(f, o),
        }
    }
}

impl ExpressionVisitable<Expression> for ArrayExpression {
    fn visit_expressions_mut<F, B>(&mut self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&mut Expression) -> ControlFlow<B>,
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
        F: FnMut(&Expression) -> ControlFlow<B>,
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

impl<Ref> ExpressionVisitable<Expression<Ref>> for LambdaExpression<Ref> {
    fn visit_expressions_mut<F, B>(&mut self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&mut Expression<Ref>) -> ControlFlow<B>,
    {
        self.body.visit_expressions_mut(f, o)
    }

    fn visit_expressions<F, B>(&self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&Expression<Ref>) -> ControlFlow<B>,
    {
        self.body.visit_expressions(f, o)
    }
}

impl<Ref> ExpressionVisitable<Expression<Ref>> for ArrayLiteral<Ref> {
    fn visit_expressions_mut<F, B>(&mut self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&mut Expression<Ref>) -> ControlFlow<B>,
    {
        self.items
            .iter_mut()
            .try_for_each(|item| item.visit_expressions_mut(f, o))
    }

    fn visit_expressions<F, B>(&self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&Expression<Ref>) -> ControlFlow<B>,
    {
        self.items
            .iter()
            .try_for_each(|item| item.visit_expressions(f, o))
    }
}

impl<Ref> ExpressionVisitable<Expression<Ref>> for IndexAccess<Ref> {
    fn visit_expressions_mut<F, B>(&mut self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&mut Expression<Ref>) -> ControlFlow<B>,
    {
        self.array.visit_expressions_mut(f, o)?;
        self.index.visit_expressions_mut(f, o)
    }

    fn visit_expressions<F, B>(&self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&Expression<Ref>) -> ControlFlow<B>,
    {
        self.array.visit_expressions(f, o)?;
        self.index.visit_expressions(f, o)
    }
}

impl<Ref> ExpressionVisitable<Expression<Ref>> for FunctionCall<Ref> {
    fn visit_expressions_mut<F, B>(&mut self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&mut Expression<Ref>) -> ControlFlow<B>,
    {
        once(self.function.as_mut())
            .chain(&mut self.arguments)
            .try_for_each(|item| item.visit_expressions_mut(f, o))
    }

    fn visit_expressions<F, B>(&self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&Expression<Ref>) -> ControlFlow<B>,
    {
        once(self.function.as_ref())
            .chain(&self.arguments)
            .try_for_each(|item| item.visit_expressions(f, o))
    }
}

impl<Ref> ExpressionVisitable<Expression<Ref>> for MatchArm<Ref> {
    fn visit_expressions_mut<F, B>(&mut self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&mut Expression<Ref>) -> ControlFlow<B>,
    {
        self.pattern.visit_expressions_mut(f, o)?;
        self.value.visit_expressions_mut(f, o)
    }

    fn visit_expressions<F, B>(&self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&Expression<Ref>) -> ControlFlow<B>,
    {
        self.pattern.visit_expressions(f, o)?;
        self.value.visit_expressions(f, o)
    }
}

impl<Ref> ExpressionVisitable<Expression<Ref>> for MatchPattern<Ref> {
    fn visit_expressions_mut<F, B>(&mut self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&mut Expression<Ref>) -> ControlFlow<B>,
    {
        match self {
            MatchPattern::CatchAll => ControlFlow::Continue(()),
            MatchPattern::Pattern(e) => e.visit_expressions_mut(f, o),
        }
    }

    fn visit_expressions<F, B>(&self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&Expression<Ref>) -> ControlFlow<B>,
    {
        match self {
            MatchPattern::CatchAll => ControlFlow::Continue(()),
            MatchPattern::Pattern(e) => e.visit_expressions(f, o),
        }
    }
}

impl<Ref> ExpressionVisitable<Expression<Ref>> for IfExpression<Ref> {
    fn visit_expressions_mut<F, B>(&mut self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&mut Expression<Ref>) -> ControlFlow<B>,
    {
        [&mut self.condition, &mut self.body, &mut self.else_body]
            .into_iter()
            .try_for_each(|e| e.visit_expressions_mut(f, o))
    }

    fn visit_expressions<F, B>(&self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&Expression<Ref>) -> ControlFlow<B>,
    {
        [&self.condition, &self.body, &self.else_body]
            .into_iter()
            .try_for_each(|e| e.visit_expressions(f, o))
    }
}

impl<E: ExpressionVisitable<E>> ExpressionVisitable<E> for Type<E> {
    fn visit_expressions_mut<F, B>(&mut self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&mut E) -> ControlFlow<B>,
    {
        match self {
            _ if self.is_elementary() => ControlFlow::Continue(()),
            Type::TypeVar(_) => ControlFlow::Continue(()),
            Type::Array(a) => a.visit_expressions_mut(f, o),
            Type::Tuple(t) => t.visit_expressions_mut(f, o),
            Type::Function(fun) => fun.visit_expressions_mut(f, o),
            _ => unreachable!(),
        }
    }

    fn visit_expressions<F, B>(&self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&E) -> ControlFlow<B>,
    {
        match self {
            _ if self.is_elementary() => ControlFlow::Continue(()),
            Type::TypeVar(_) => ControlFlow::Continue(()),
            Type::Array(a) => a.visit_expressions(f, o),
            Type::Tuple(t) => t.visit_expressions(f, o),
            Type::Function(fun) => fun.visit_expressions(f, o),
            _ => unreachable!(),
        }
    }
}

impl<E: ExpressionVisitable<E>> ExpressionVisitable<E> for ArrayType<E> {
    fn visit_expressions_mut<F, B>(&mut self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&mut E) -> ControlFlow<B>,
    {
        self.base.visit_expressions_mut(f, o)?;
        self.length
            .iter_mut()
            .try_for_each(|e| e.visit_expressions_mut(f, o))
    }

    fn visit_expressions<F, B>(&self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&E) -> ControlFlow<B>,
    {
        self.base.visit_expressions(f, o)?;
        self.length
            .iter()
            .try_for_each(|e| e.visit_expressions(f, o))
    }
}

impl<E: ExpressionVisitable<E>> ExpressionVisitable<E> for TupleType<E> {
    fn visit_expressions_mut<F, B>(&mut self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&mut E) -> ControlFlow<B>,
    {
        self.items
            .iter_mut()
            .try_for_each(|i| i.visit_expressions_mut(f, o))
    }

    fn visit_expressions<F, B>(&self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&E) -> ControlFlow<B>,
    {
        self.items
            .iter()
            .try_for_each(|i| i.visit_expressions(f, o))
    }
}

impl<E: ExpressionVisitable<E>> ExpressionVisitable<E> for FunctionType<E> {
    fn visit_expressions_mut<F, B>(&mut self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&mut E) -> ControlFlow<B>,
    {
        self.params
            .iter_mut()
            .chain(once(self.value.as_mut()))
            .try_for_each(|i| i.visit_expressions_mut(f, o))
    }

    fn visit_expressions<F, B>(&self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&E) -> ControlFlow<B>,
    {
        self.params
            .iter()
            .chain(once(self.value.as_ref()))
            .try_for_each(|i| i.visit_expressions(f, o))
    }
}
