use std::ops::ControlFlow;

use super::{
    ArrayExpression, ArrayLiteral, Expression, FunctionCall, FunctionDefinition, LambdaExpression,
    MatchArm, PilStatement, ShiftedPolynomialReference,
};

/// A trait to be implemented by an AST node.
/// The idea is that it calls a callback function on each of the sub-nodes
/// that are expressions.
pub trait ExpressionVisitor<T, Ref> {
    /// Traverses the AST and calls `f` on each Expression in pre-order,
    /// potentially break early and return a value.
    fn pre_visit_expressions_return_mut<F, B>(&mut self, f: &mut F) -> ControlFlow<B>
    where
        F: FnMut(&mut Expression<T, Ref>) -> ControlFlow<B>;

    /// Traverses the AST and calls `f` on each Expression in pre-order.
    fn pre_visit_expressions_mut<F>(&mut self, f: &mut F)
    where
        F: FnMut(&mut Expression<T, Ref>),
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
        F: FnMut(&Expression<T, Ref>) -> ControlFlow<B>;

    /// Traverses the AST and calls `f` on each Expression in pre-order.
    fn pre_visit_expression<F>(&self, f: &mut F)
    where
        F: FnMut(&Expression<T, Ref>),
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
        F: FnMut(&mut Expression<T, Ref>) -> ControlFlow<B>;

    /// Traverses the AST and calls `f` on each Expression in post-order.
    fn post_visit_expressions_mut<F>(&mut self, f: &mut F)
    where
        F: FnMut(&mut Expression<T, Ref>),
    {
        self.post_visit_expressions_return_mut(&mut move |e| {
            f(e);
            ControlFlow::Continue::<()>(())
        });
    }
}

impl<T, Ref> ExpressionVisitor<T, Ref> for Expression<T, Ref> {
    fn pre_visit_expressions_return_mut<F, B>(&mut self, f: &mut F) -> ControlFlow<B>
    where
        F: FnMut(&mut Expression<T, Ref>) -> ControlFlow<B>,
    {
        f(self)?;
        match self {
            Expression::Reference(_)
            | Expression::Constant(_)
            | Expression::PublicReference(_)
            | Expression::Number(_)
            | Expression::String(_) => {}
            Expression::BinaryOperation(left, _, right) => {
                left.pre_visit_expressions_return_mut(f)?;
                right.pre_visit_expressions_return_mut(f)?;
            }
            Expression::FreeInput(e) | Expression::UnaryOperation(_, e) => {
                e.pre_visit_expressions_return_mut(f)?
            }
            Expression::LambdaExpression(lambda) => lambda.pre_visit_expressions_return_mut(f)?,
            Expression::ArrayLiteral(array_literal) => {
                array_literal.pre_visit_expressions_return_mut(f)?
            }
            Expression::FunctionCall(function) => function.pre_visit_expressions_return_mut(f)?,
            Expression::Tuple(items) => items
                .iter_mut()
                .try_for_each(|item| item.pre_visit_expressions_return_mut(f))?,
            Expression::MatchExpression(scrutinee, arms) => {
                std::iter::once(scrutinee.as_mut())
                    .chain(arms.iter_mut().map(|MatchArm { pattern: _, value }| value))
                    .try_for_each(move |item| item.pre_visit_expressions_return_mut(f))?;
            }
        };
        ControlFlow::Continue(())
    }

    fn pre_visit_expressions_return<F, B>(&self, f: &mut F) -> ControlFlow<B>
    where
        F: FnMut(&Expression<T, Ref>) -> ControlFlow<B>,
    {
        f(self)?;
        match self {
            Expression::Reference(_)
            | Expression::Constant(_)
            | Expression::PublicReference(_)
            | Expression::Number(_)
            | Expression::String(_) => {}
            Expression::BinaryOperation(left, _, right) => {
                left.pre_visit_expressions_return(f)?;
                right.pre_visit_expressions_return(f)?;
            }
            Expression::FreeInput(e) | Expression::UnaryOperation(_, e) => {
                e.pre_visit_expressions_return(f)?
            }
            Expression::LambdaExpression(lambda) => lambda.pre_visit_expressions_return(f)?,
            Expression::ArrayLiteral(array_literal) => {
                array_literal.pre_visit_expressions_return(f)?
            }
            Expression::FunctionCall(function) => function.pre_visit_expressions_return(f)?,
            Expression::Tuple(items) => items
                .iter()
                .try_for_each(|item| item.pre_visit_expressions_return(f))?,
            Expression::MatchExpression(scrutinee, arms) => {
                std::iter::once(scrutinee.as_ref())
                    .chain(arms.iter().map(|MatchArm { pattern: _, value }| value))
                    .try_for_each(move |item| item.pre_visit_expressions_return(f))?;
            }
        };
        ControlFlow::Continue(())
    }

    fn post_visit_expressions_return_mut<F, B>(&mut self, f: &mut F) -> ControlFlow<B>
    where
        F: FnMut(&mut Expression<T, Ref>) -> ControlFlow<B>,
    {
        match self {
            Expression::Reference(_)
            | Expression::Constant(_)
            | Expression::PublicReference(_)
            | Expression::Number(_)
            | Expression::String(_) => {}
            Expression::BinaryOperation(left, _, right) => {
                left.post_visit_expressions_return_mut(f)?;
                right.post_visit_expressions_return_mut(f)?;
            }
            Expression::FreeInput(e) | Expression::UnaryOperation(_, e) => {
                e.post_visit_expressions_return_mut(f)?
            }
            Expression::LambdaExpression(lambda) => lambda.post_visit_expressions_return_mut(f)?,
            Expression::ArrayLiteral(array_literal) => {
                array_literal.post_visit_expressions_return_mut(f)?
            }
            Expression::FunctionCall(function) => function.post_visit_expressions_return_mut(f)?,
            Expression::Tuple(items) => items
                .iter_mut()
                .try_for_each(|item| item.post_visit_expressions_return_mut(f))?,
            Expression::MatchExpression(scrutinee, arms) => {
                std::iter::once(scrutinee.as_mut())
                    .chain(arms.iter_mut().map(|MatchArm { pattern: _, value }| value))
                    .try_for_each(|item| item.post_visit_expressions_return_mut(f))?;
            }
        };
        f(self)
    }
}

impl<T> ExpressionVisitor<T, ShiftedPolynomialReference<T>> for PilStatement<T> {
    fn pre_visit_expressions_return_mut<F, B>(&mut self, f: &mut F) -> ControlFlow<B>
    where
        F: FnMut(&mut Expression<T>) -> ControlFlow<B>,
    {
        match self {
            PilStatement::FunctionCall(_, _, arguments) => arguments
                .iter_mut()
                .try_for_each(|e| e.pre_visit_expressions_return_mut(f)),
            PilStatement::PlookupIdentity(_, left, right)
            | PilStatement::PermutationIdentity(_, left, right) => left
                .selector
                .iter_mut()
                .chain(left.expressions.iter_mut())
                .chain(right.selector.iter_mut())
                .chain(right.expressions.iter_mut())
                .try_for_each(|e| e.pre_visit_expressions_return_mut(f)),
            PilStatement::ConnectIdentity(_start, left, right) => left
                .iter_mut()
                .chain(right.iter_mut())
                .try_for_each(|e| e.pre_visit_expressions_return_mut(f)),

            PilStatement::Namespace(_, _, e)
            | PilStatement::PolynomialDefinition(_, _, e)
            | PilStatement::PolynomialIdentity(_, e)
            | PilStatement::PublicDeclaration(_, _, _, e)
            | PilStatement::ConstantDefinition(_, _, e)
            | PilStatement::LetStatement(_, _, Some(e)) => e.pre_visit_expressions_return_mut(f),

            PilStatement::PolynomialConstantDefinition(_, _, fundef)
            | PilStatement::PolynomialCommitDeclaration(_, _, Some(fundef)) => {
                fundef.pre_visit_expressions_return_mut(f)
            }
            PilStatement::PolynomialCommitDeclaration(_, _, None)
            | PilStatement::Include(_, _)
            | PilStatement::PolynomialConstantDeclaration(_, _)
            | PilStatement::MacroDefinition(_, _, _, _, _)
            | PilStatement::LetStatement(_, _, None) => ControlFlow::Continue(()),
        }
    }

    fn pre_visit_expressions_return<F, B>(&self, f: &mut F) -> ControlFlow<B>
    where
        F: FnMut(&Expression<T>) -> ControlFlow<B>,
    {
        match self {
            PilStatement::FunctionCall(_, _, arguments) => arguments
                .iter()
                .try_for_each(|e| e.pre_visit_expressions_return(f)),
            PilStatement::PlookupIdentity(_, left, right)
            | PilStatement::PermutationIdentity(_, left, right) => left
                .selector
                .iter()
                .chain(left.expressions.iter())
                .chain(right.selector.iter())
                .chain(right.expressions.iter())
                .try_for_each(|e| e.pre_visit_expressions_return(f)),
            PilStatement::ConnectIdentity(_start, left, right) => left
                .iter()
                .chain(right.iter())
                .try_for_each(|e| e.pre_visit_expressions_return(f)),

            PilStatement::Namespace(_, _, e)
            | PilStatement::PolynomialDefinition(_, _, e)
            | PilStatement::PolynomialIdentity(_, e)
            | PilStatement::PublicDeclaration(_, _, _, e)
            | PilStatement::ConstantDefinition(_, _, e)
            | PilStatement::LetStatement(_, _, Some(e)) => e.pre_visit_expressions_return(f),

            PilStatement::PolynomialConstantDefinition(_, _, fundef)
            | PilStatement::PolynomialCommitDeclaration(_, _, Some(fundef)) => {
                fundef.pre_visit_expressions_return(f)
            }
            PilStatement::PolynomialCommitDeclaration(_, _, None)
            | PilStatement::Include(_, _)
            | PilStatement::PolynomialConstantDeclaration(_, _)
            | PilStatement::MacroDefinition(_, _, _, _, _)
            | PilStatement::LetStatement(_, _, None) => ControlFlow::Continue(()),
        }
    }

    fn post_visit_expressions_return_mut<F, B>(&mut self, f: &mut F) -> ControlFlow<B>
    where
        F: FnMut(&mut Expression<T>) -> ControlFlow<B>,
    {
        match self {
            PilStatement::FunctionCall(_, _, arguments) => arguments
                .iter_mut()
                .try_for_each(|e| e.post_visit_expressions_return_mut(f)),
            PilStatement::PlookupIdentity(_, left, right)
            | PilStatement::PermutationIdentity(_, left, right) => left
                .selector
                .iter_mut()
                .chain(left.expressions.iter_mut())
                .chain(right.selector.iter_mut())
                .chain(right.expressions.iter_mut())
                .try_for_each(|e| e.post_visit_expressions_return_mut(f)),
            PilStatement::ConnectIdentity(_start, left, right) => left
                .iter_mut()
                .chain(right.iter_mut())
                .try_for_each(|e| e.post_visit_expressions_return_mut(f)),

            PilStatement::Namespace(_, _, e)
            | PilStatement::PolynomialDefinition(_, _, e)
            | PilStatement::PolynomialIdentity(_, e)
            | PilStatement::PublicDeclaration(_, _, _, e)
            | PilStatement::ConstantDefinition(_, _, e)
            | PilStatement::LetStatement(_, _, Some(e)) => e.post_visit_expressions_return_mut(f),

            PilStatement::PolynomialConstantDefinition(_, _, fundef)
            | PilStatement::PolynomialCommitDeclaration(_, _, Some(fundef)) => {
                fundef.post_visit_expressions_return_mut(f)
            }
            PilStatement::PolynomialCommitDeclaration(_, _, None)
            | PilStatement::Include(_, _)
            | PilStatement::PolynomialConstantDeclaration(_, _)
            | PilStatement::MacroDefinition(_, _, _, _, _)
            | PilStatement::LetStatement(_, _, None) => ControlFlow::Continue(()),
        }
    }
}

impl<T> ExpressionVisitor<T, ShiftedPolynomialReference<T>> for FunctionDefinition<T> {
    fn pre_visit_expressions_return_mut<F, B>(&mut self, f: &mut F) -> ControlFlow<B>
    where
        F: FnMut(&mut Expression<T>) -> ControlFlow<B>,
    {
        match self {
            FunctionDefinition::Query(_, e) | FunctionDefinition::Mapping(_, e) => {
                e.pre_visit_expressions_return_mut(f)
            }
            FunctionDefinition::Array(ae) => ae.pre_visit_expressions_return_mut(f),
            FunctionDefinition::Expression(e) => e.pre_visit_expressions_return_mut(f),
        }
    }

    fn pre_visit_expressions_return<F, B>(&self, f: &mut F) -> ControlFlow<B>
    where
        F: FnMut(&Expression<T>) -> ControlFlow<B>,
    {
        match self {
            FunctionDefinition::Query(_, e) | FunctionDefinition::Mapping(_, e) => {
                e.pre_visit_expressions_return(f)
            }
            FunctionDefinition::Array(ae) => ae.pre_visit_expressions_return(f),
            FunctionDefinition::Expression(e) => e.pre_visit_expressions_return(f),
        }
    }

    fn post_visit_expressions_return_mut<F, B>(&mut self, f: &mut F) -> ControlFlow<B>
    where
        F: FnMut(&mut Expression<T>) -> ControlFlow<B>,
    {
        match self {
            FunctionDefinition::Query(_, e) | FunctionDefinition::Mapping(_, e) => {
                e.post_visit_expressions_return_mut(f)
            }
            FunctionDefinition::Array(ae) => ae.post_visit_expressions_return_mut(f),
            FunctionDefinition::Expression(e) => e.post_visit_expressions_return_mut(f),
        }
    }
}

impl<T> ExpressionVisitor<T, ShiftedPolynomialReference<T>> for ArrayExpression<T> {
    fn pre_visit_expressions_return_mut<F, B>(&mut self, f: &mut F) -> ControlFlow<B>
    where
        F: FnMut(&mut Expression<T>) -> ControlFlow<B>,
    {
        match self {
            ArrayExpression::Value(expressions) | ArrayExpression::RepeatedValue(expressions) => {
                expressions
                    .iter_mut()
                    .try_for_each(|e| e.pre_visit_expressions_return_mut(f))
            }
            ArrayExpression::Concat(a1, a2) => [a1, a2]
                .iter_mut()
                .try_for_each(|e| e.pre_visit_expressions_return_mut(f)),
        }
    }

    fn pre_visit_expressions_return<F, B>(&self, f: &mut F) -> ControlFlow<B>
    where
        F: FnMut(&Expression<T>) -> ControlFlow<B>,
    {
        match self {
            ArrayExpression::Value(expressions) | ArrayExpression::RepeatedValue(expressions) => {
                expressions
                    .iter()
                    .try_for_each(|e| e.pre_visit_expressions_return(f))
            }
            ArrayExpression::Concat(a1, a2) => [a1, a2]
                .iter()
                .try_for_each(|e| e.pre_visit_expressions_return(f)),
        }
    }

    fn post_visit_expressions_return_mut<F, B>(&mut self, f: &mut F) -> ControlFlow<B>
    where
        F: FnMut(&mut Expression<T>) -> ControlFlow<B>,
    {
        match self {
            ArrayExpression::Value(expressions) | ArrayExpression::RepeatedValue(expressions) => {
                expressions
                    .iter_mut()
                    .try_for_each(|e| e.post_visit_expressions_return_mut(f))
            }
            ArrayExpression::Concat(a1, a2) => [a1, a2]
                .iter_mut()
                .try_for_each(|e| e.post_visit_expressions_return_mut(f)),
        }
    }
}

impl<T, Ref> ExpressionVisitor<T, Ref> for LambdaExpression<T, Ref> {
    fn pre_visit_expressions_return_mut<F, B>(&mut self, f: &mut F) -> ControlFlow<B>
    where
        F: FnMut(&mut Expression<T, Ref>) -> ControlFlow<B>,
    {
        self.body.pre_visit_expressions_return_mut(f)
    }

    fn pre_visit_expressions_return<F, B>(&self, f: &mut F) -> ControlFlow<B>
    where
        F: FnMut(&Expression<T, Ref>) -> ControlFlow<B>,
    {
        self.body.pre_visit_expressions_return(f)
    }

    fn post_visit_expressions_return_mut<F, B>(&mut self, f: &mut F) -> ControlFlow<B>
    where
        F: FnMut(&mut Expression<T, Ref>) -> ControlFlow<B>,
    {
        self.body.post_visit_expressions_return_mut(f)
    }
}

impl<T, Ref> ExpressionVisitor<T, Ref> for ArrayLiteral<T, Ref> {
    fn pre_visit_expressions_return_mut<F, B>(&mut self, f: &mut F) -> ControlFlow<B>
    where
        F: FnMut(&mut Expression<T, Ref>) -> ControlFlow<B>,
    {
        self.items
            .iter_mut()
            .try_for_each(|item| item.pre_visit_expressions_return_mut(f))
    }

    fn pre_visit_expressions_return<F, B>(&self, f: &mut F) -> ControlFlow<B>
    where
        F: FnMut(&Expression<T, Ref>) -> ControlFlow<B>,
    {
        self.items
            .iter()
            .try_for_each(|item| item.pre_visit_expressions_return(f))
    }

    fn post_visit_expressions_return_mut<F, B>(&mut self, f: &mut F) -> ControlFlow<B>
    where
        F: FnMut(&mut Expression<T, Ref>) -> ControlFlow<B>,
    {
        self.items
            .iter_mut()
            .try_for_each(|item| item.post_visit_expressions_return_mut(f))
    }
}

impl<T, Ref> ExpressionVisitor<T, Ref> for FunctionCall<T, Ref> {
    fn pre_visit_expressions_return_mut<F, B>(&mut self, f: &mut F) -> ControlFlow<B>
    where
        F: FnMut(&mut Expression<T, Ref>) -> ControlFlow<B>,
    {
        self.arguments
            .iter_mut()
            .try_for_each(|item| item.pre_visit_expressions_return_mut(f))
    }

    fn pre_visit_expressions_return<F, B>(&self, f: &mut F) -> ControlFlow<B>
    where
        F: FnMut(&Expression<T, Ref>) -> ControlFlow<B>,
    {
        self.arguments
            .iter()
            .try_for_each(|item| item.pre_visit_expressions_return(f))
    }

    fn post_visit_expressions_return_mut<F, B>(&mut self, f: &mut F) -> ControlFlow<B>
    where
        F: FnMut(&mut Expression<T, Ref>) -> ControlFlow<B>,
    {
        self.arguments
            .iter_mut()
            .try_for_each(|item| item.post_visit_expressions_return_mut(f))
    }
}
