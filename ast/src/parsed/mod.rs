pub mod asm;
pub mod build;
pub mod display;
use std::{iter::once, ops::ControlFlow};

use number::{DegreeType, FieldElement};

use self::asm::FunctionCall;

#[derive(Debug, PartialEq, Eq)]
pub struct PILFile<T>(pub Vec<PilStatement<T>>);

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum PilStatement<T> {
    /// File name
    Include(usize, String),
    /// Name of namespace and polynomial degree (constant)
    Namespace(usize, String, Expression<T>),
    PolynomialDefinition(usize, String, Expression<T>),
    PublicDeclaration(usize, String, PolynomialReference<T>, Expression<T>),
    PolynomialConstantDeclaration(usize, Vec<PolynomialName<T>>),
    PolynomialConstantDefinition(usize, String, FunctionDefinition<T>),
    PolynomialCommitDeclaration(usize, Vec<PolynomialName<T>>, Option<FunctionDefinition<T>>),
    PolynomialIdentity(usize, Expression<T>),
    PlookupIdentity(usize, SelectedExpressions<T>, SelectedExpressions<T>),
    PermutationIdentity(usize, SelectedExpressions<T>, SelectedExpressions<T>),
    ConnectIdentity(usize, Vec<Expression<T>>, Vec<Expression<T>>),
    ConstantDefinition(usize, String, Expression<T>),
    MacroDefinition(
        usize,
        String,
        Vec<String>,
        Vec<PilStatement<T>>,
        Option<Expression<T>>,
    ),
    FunctionCall(usize, String, Vec<Expression<T>>),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct SelectedExpressions<T> {
    pub selector: Option<Expression<T>>,
    pub expressions: Vec<Expression<T>>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum Expression<T> {
    /// Reference to a constant, "%ConstantName"
    Constant(String),
    PolynomialReference(PolynomialReference<T>),
    PublicReference(String),
    Number(T),
    String(String),
    Tuple(Vec<Expression<T>>),
    BinaryOperation(Box<Expression<T>>, BinaryOperator, Box<Expression<T>>),
    UnaryOperation(UnaryOperator, Box<Expression<T>>),
    FunctionCall(FunctionCall<T>),
    FreeInput(Box<Expression<T>>),
    MatchExpression(
        Box<Expression<T>>,
        Vec<(Option<Expression<T>>, Expression<T>)>,
    ),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Default, Clone)]
pub struct PolynomialName<T> {
    pub name: String,
    pub array_size: Option<Expression<T>>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Default, Clone)]
pub struct PolynomialReference<T> {
    pub namespace: Option<String>,
    pub name: String,
    pub index: Option<Box<Expression<T>>>,
    pub next: bool,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum UnaryOperator {
    Plus,
    Minus,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    BinaryAnd,
    BinaryXor,
    BinaryOr,
    ShiftLeft,
    ShiftRight,
}

/// The definition of a function (excluding its name):
/// Either a param-value mapping or an array expression.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum FunctionDefinition<T> {
    /// Parameter-value-mapping.
    Mapping(Vec<String>, Expression<T>),
    /// Array expression.
    Array(ArrayExpression<T>),
    /// Prover query.
    Query(Vec<String>, Expression<T>),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum ArrayExpression<T> {
    Value(Vec<Expression<T>>),
    RepeatedValue(Vec<Expression<T>>),
    Concat(Box<ArrayExpression<T>>, Box<ArrayExpression<T>>),
}

impl<T: FieldElement> ArrayExpression<T> {
    pub fn value(v: Vec<Expression<T>>) -> Self {
        Self::Value(v)
    }

    pub fn repeated_value(v: Vec<Expression<T>>) -> Self {
        Self::RepeatedValue(v)
    }

    pub fn concat(self, other: Self) -> Self {
        Self::Concat(Box::new(self), Box::new(other))
    }

    fn pad_with(self, pad: Expression<T>) -> Self {
        Self::concat(self, Self::repeated_value(vec![pad]))
    }

    pub fn pad_with_zeroes(self) -> Self {
        self.pad_with(Expression::Number(0.into()))
    }

    fn last(&self) -> Option<&Expression<T>> {
        match self {
            ArrayExpression::Value(v) => v.last(),
            ArrayExpression::RepeatedValue(v) => v.last(),
            ArrayExpression::Concat(_, right) => right.last(),
        }
    }

    // return None if `self` is empty
    pub fn pad_with_last(self) -> Option<Self> {
        self.last().cloned().map(|last| self.pad_with(last))
    }
}

impl<T> ArrayExpression<T> {
    /// solve for `*`
    pub fn solve(&self, degree: DegreeType) -> DegreeType {
        assert!(
            self.number_of_repetitions() <= 1,
            "`*` can be used only once in rhs of array definition"
        );
        let len = self.constant_length();
        assert!(
            len <= degree,
            "Array literal is too large ({len}) for degree ({degree})."
        );
        // Fill up the remaining space with the repeated array
        degree - len
    }

    /// The number of times the `*` operator is used
    fn number_of_repetitions(&self) -> usize {
        match self {
            ArrayExpression::RepeatedValue(_) => 1,
            ArrayExpression::Value(_) => 0,
            ArrayExpression::Concat(left, right) => {
                left.number_of_repetitions() + right.number_of_repetitions()
            }
        }
    }

    /// The combined length of the constant-size parts of the array expression.
    fn constant_length(&self) -> DegreeType {
        match self {
            ArrayExpression::RepeatedValue(_) => 0,
            ArrayExpression::Value(e) => e.len() as DegreeType,
            ArrayExpression::Concat(left, right) => {
                left.constant_length() + right.constant_length()
            }
        }
    }
}

/// Traverses the expression tree and calls `f` in post-order.
pub fn postvisit_expression_mut<T, F, B>(e: &mut Expression<T>, f: &mut F) -> ControlFlow<B>
where
    F: FnMut(&mut Expression<T>) -> ControlFlow<B>,
{
    match e {
        Expression::PolynomialReference(_)
        | Expression::Constant(_)
        | Expression::PublicReference(_)
        | Expression::Number(_)
        | Expression::String(_) => {}
        Expression::BinaryOperation(left, _, right) => {
            postvisit_expression_mut(left, f)?;
            postvisit_expression_mut(right, f)?;
        }
        Expression::UnaryOperation(_, e) => postvisit_expression_mut(e.as_mut(), f)?,
        Expression::Tuple(items)
        | Expression::FunctionCall(FunctionCall {
            arguments: items, ..
        }) => items
            .iter_mut()
            .try_for_each(|item| postvisit_expression_mut(item, f))?,
        Expression::FreeInput(query) => postvisit_expression_mut(query.as_mut(), f)?,
        Expression::MatchExpression(scrutinee, arms) => {
            once(scrutinee.as_mut())
                .chain(arms.iter_mut().map(|(_n, e)| e))
                .try_for_each(|item| postvisit_expression_mut(item, f))?;
        }
    };
    f(e)
}

/// Traverses the expression trees of the statement and calls `f` in post-order.
/// Does not enter macro definitions.
pub fn postvisit_expression_in_statement_mut<T, F, B>(
    statement: &mut PilStatement<T>,
    f: &mut F,
) -> ControlFlow<B>
where
    F: FnMut(&mut Expression<T>) -> ControlFlow<B>,
{
    match statement {
        PilStatement::FunctionCall(_, _, arguments) => arguments
            .iter_mut()
            .try_for_each(|e| postvisit_expression_mut(e, f)),
        PilStatement::PlookupIdentity(_, left, right)
        | PilStatement::PermutationIdentity(_, left, right) => left
            .selector
            .iter_mut()
            .chain(left.expressions.iter_mut())
            .chain(right.selector.iter_mut())
            .chain(right.expressions.iter_mut())
            .try_for_each(|e| postvisit_expression_mut(e, f)),
        PilStatement::ConnectIdentity(_start, left, right) => left
            .iter_mut()
            .chain(right.iter_mut())
            .try_for_each(|e| postvisit_expression_mut(e, f)),

        PilStatement::Namespace(_, _, e)
        | PilStatement::PolynomialDefinition(_, _, e)
        | PilStatement::PolynomialIdentity(_, e)
        | PilStatement::PublicDeclaration(_, _, _, e)
        | PilStatement::ConstantDefinition(_, _, e) => postvisit_expression_mut(e, f),

        PilStatement::PolynomialConstantDefinition(_, _, fundef)
        | PilStatement::PolynomialCommitDeclaration(_, _, Some(fundef)) => match fundef {
            FunctionDefinition::Query(_, e) | FunctionDefinition::Mapping(_, e) => {
                postvisit_expression_mut(e, f)
            }
            FunctionDefinition::Array(ae) => postvisit_expression_in_array_expression_mut(ae, f),
        },
        PilStatement::PolynomialCommitDeclaration(_, _, None)
        | PilStatement::Include(_, _)
        | PilStatement::PolynomialConstantDeclaration(_, _)
        | PilStatement::MacroDefinition(_, _, _, _, _) => ControlFlow::Continue(()),
    }
}

fn postvisit_expression_in_array_expression_mut<T, F, B>(
    ae: &mut ArrayExpression<T>,
    f: &mut F,
) -> ControlFlow<B>
where
    F: FnMut(&mut Expression<T>) -> ControlFlow<B>,
{
    match ae {
        ArrayExpression::Value(expressions) | ArrayExpression::RepeatedValue(expressions) => {
            expressions
                .iter_mut()
                .try_for_each(|e| postvisit_expression_mut(e, f))
        }
        ArrayExpression::Concat(a1, a2) => [a1, a2]
            .iter_mut()
            .try_for_each(|e| postvisit_expression_in_array_expression_mut(e, f)),
    }
}
