pub mod asm;
pub mod build;
pub mod display;
pub mod folder;
pub mod utils;
pub mod visitor;

use std::ops;

use number::{DegreeType, FieldElement};

use self::asm::{Part, SymbolPath};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct PILFile<T>(pub Vec<PilStatement<T>>);

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum PilStatement<T> {
    /// File name
    Include(usize, String),
    /// Name of namespace and polynomial degree (constant)
    Namespace(usize, SymbolPath, Expression<T>),
    LetStatement(usize, String, Option<Expression<T>>),
    PolynomialDefinition(usize, String, Expression<T>),
    PublicDeclaration(
        usize,
        /// The name of the public value.
        String,
        /// The polynomial/column that contains the public value.
        NamespacedPolynomialReference,
        /// If the polynomial is an array, this is the array element index.
        Option<Expression<T>>,
        /// The row number of the public value.
        Expression<T>,
    ),
    PolynomialConstantDeclaration(usize, Vec<PolynomialName<T>>),
    PolynomialConstantDefinition(usize, String, FunctionDefinition<T>),
    PolynomialCommitDeclaration(usize, Vec<PolynomialName<T>>, Option<FunctionDefinition<T>>),
    PolynomialIdentity(usize, Expression<T>),
    PlookupIdentity(
        usize,
        SelectedExpressions<Expression<T>>,
        SelectedExpressions<Expression<T>>,
    ),
    PermutationIdentity(
        usize,
        SelectedExpressions<Expression<T>>,
        SelectedExpressions<Expression<T>>,
    ),
    ConnectIdentity(usize, Vec<Expression<T>>, Vec<Expression<T>>),
    ConstantDefinition(usize, String, Expression<T>),
    Expression(usize, Expression<T>),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct SelectedExpressions<Expr> {
    pub selector: Option<Expr>,
    pub expressions: Vec<Expr>,
}

impl<Expr> Default for SelectedExpressions<Expr> {
    fn default() -> Self {
        Self {
            selector: Default::default(),
            expressions: Default::default(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum Expression<T, Ref = NamespacedPolynomialReference> {
    Reference(Ref),
    PublicReference(String),
    Number(T),
    String(String),
    Tuple(Vec<Expression<T, Ref>>),
    LambdaExpression(LambdaExpression<T, Ref>),
    ArrayLiteral(ArrayLiteral<T, Ref>),
    BinaryOperation(
        Box<Expression<T, Ref>>,
        BinaryOperator,
        Box<Expression<T, Ref>>,
    ),
    UnaryOperation(UnaryOperator, Box<Expression<T, Ref>>),
    IndexAccess(IndexAccess<T, Ref>),
    FunctionCall(FunctionCall<T, Ref>),
    FreeInput(Box<Expression<T, Ref>>),
    MatchExpression(Box<Expression<T, Ref>>, Vec<MatchArm<T, Ref>>),
    IfExpression(IfExpression<T, Ref>),
}

impl<T, Ref> Expression<T, Ref> {
    pub fn new_binary(left: Self, op: BinaryOperator, right: Self) -> Self {
        Expression::BinaryOperation(Box::new(left), op, Box::new(right))
    }

    /// Visits this expression and all of its sub-expressions and returns true
    /// if `f` returns true on any of them.
    pub fn any(&self, mut f: impl FnMut(&Self) -> bool) -> bool {
        use std::ops::ControlFlow;
        use visitor::ExpressionVisitable;
        self.pre_visit_expressions_return(&mut |e| {
            if f(e) {
                ControlFlow::Break(())
            } else {
                ControlFlow::Continue(())
            }
        })
        .is_break()
    }
}

impl<T, Ref> ops::Add for Expression<T, Ref> {
    type Output = Expression<T, Ref>;

    fn add(self, rhs: Self) -> Self::Output {
        Self::new_binary(self, BinaryOperator::Add, rhs)
    }
}

impl<T, Ref> ops::Sub for Expression<T, Ref> {
    type Output = Expression<T, Ref>;

    fn sub(self, rhs: Self) -> Self::Output {
        Self::new_binary(self, BinaryOperator::Sub, rhs)
    }
}
impl<T, Ref> ops::Mul for Expression<T, Ref> {
    type Output = Expression<T, Ref>;

    fn mul(self, rhs: Self) -> Self::Output {
        Self::new_binary(self, BinaryOperator::Mul, rhs)
    }
}

impl<T: FieldElement, Ref> std::iter::Sum for Expression<T, Ref> {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.reduce(|a, b| a + b)
            .unwrap_or_else(|| T::zero().into())
    }
}

impl<T: FieldElement, Ref> From<T> for Expression<T, Ref> {
    fn from(value: T) -> Self {
        Expression::Number(value)
    }
}

impl<T> From<NamespacedPolynomialReference> for Expression<T> {
    fn from(value: NamespacedPolynomialReference) -> Self {
        Self::Reference(value)
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Default, Clone)]
pub struct PolynomialName<T> {
    pub name: String,
    pub array_size: Option<Expression<T>>,
}

#[derive(Debug, PartialEq, Eq, Default, Clone, PartialOrd, Ord)]
/// A polynomial with an optional namespace
/// This is different from SymbolPath mainly due to different formatting.
pub struct NamespacedPolynomialReference {
    pub path: SymbolPath,
}

impl From<SymbolPath> for NamespacedPolynomialReference {
    fn from(value: SymbolPath) -> Self {
        Self { path: value }
    }
}

impl NamespacedPolynomialReference {
    pub fn from_identifier(name: String) -> Self {
        SymbolPath::from_parts(vec![Part::Named(name)]).into()
    }

    pub fn try_to_identifier(&self) -> Option<&String> {
        self.path.try_to_identifier()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct LambdaExpression<T, Ref = NamespacedPolynomialReference> {
    pub params: Vec<String>,
    pub body: Box<Expression<T, Ref>>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ArrayLiteral<T, Ref = NamespacedPolynomialReference> {
    pub items: Vec<Expression<T, Ref>>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum UnaryOperator {
    Plus,
    Minus,
    LogicalNot,
    Next,
}

impl UnaryOperator {
    /// Returns true if the operator is a prefix-operator and false if it is a postfix operator.
    pub fn is_prefix(&self) -> bool {
        match self {
            UnaryOperator::Plus | UnaryOperator::Minus | UnaryOperator::LogicalNot => true,
            UnaryOperator::Next => false,
        }
    }
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
    LogicalOr,
    LogicalAnd,
    Less,
    LessEqual,
    Equal,
    NotEqual,
    GreaterEqual,
    Greater,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct IndexAccess<T, Ref = NamespacedPolynomialReference> {
    pub array: Box<Expression<T, Ref>>,
    pub index: Box<Expression<T, Ref>>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct FunctionCall<T, Ref = NamespacedPolynomialReference> {
    pub function: Box<Expression<T, Ref>>,
    pub arguments: Vec<Expression<T, Ref>>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct MatchArm<T, Ref = NamespacedPolynomialReference> {
    pub pattern: MatchPattern<T, Ref>,
    pub value: Expression<T, Ref>,
}

/// A pattern for a match arm. We could extend this in the future.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum MatchPattern<T, Ref = NamespacedPolynomialReference> {
    CatchAll,
    Pattern(Expression<T, Ref>),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct IfExpression<T, Ref = NamespacedPolynomialReference> {
    pub condition: Box<Expression<T, Ref>>,
    pub body: Box<Expression<T, Ref>>,
    pub else_body: Box<Expression<T, Ref>>,
}

/// The definition of a function (excluding its name):
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum FunctionDefinition<T> {
    /// Array expression.
    Array(ArrayExpression<T>),
    /// Prover query. The Expression usually is a LambdaExpression.
    Query(Expression<T>),
    /// Generic expression
    Expression(Expression<T>),
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
