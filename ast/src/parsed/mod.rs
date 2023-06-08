pub mod asm;
pub mod display;
pub mod utils;

use number::{DegreeType, FieldElement};

use asm::ASMStatement;

#[derive(Debug, PartialEq, Eq)]
pub struct PILFile<T>(pub Vec<Statement<T>>);

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Statement<T> {
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
        Vec<Statement<T>>,
        Option<Expression<T>>,
    ),
    FunctionCall(usize, String, Vec<Expression<T>>),
    ASMBlock(usize, Vec<ASMStatement<T>>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct SelectedExpressions<T> {
    pub selector: Option<Expression<T>>,
    pub expressions: Vec<Expression<T>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
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
    FunctionCall(String, Vec<Expression<T>>),
    FreeInput(Box<Expression<T>>),
    MatchExpression(
        Box<Expression<T>>,
        Vec<(Option<Expression<T>>, Expression<T>)>,
    ),
}

#[derive(Debug, PartialEq, Eq, Default, Clone)]
pub struct PolynomialName<T> {
    pub name: String,
    pub array_size: Option<Expression<T>>,
}

#[derive(Debug, PartialEq, Eq, Default, Clone)]
pub struct PolynomialReference<T> {
    pub namespace: Option<String>,
    pub name: String,
    pub index: Option<Box<Expression<T>>>,
    pub next: bool,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum UnaryOperator {
    Plus,
    Minus,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum FunctionDefinition<T> {
    /// Parameter-value-mapping.
    Mapping(Vec<String>, Expression<T>),
    /// Array expression.
    Array(ArrayExpression<T>),
    /// Prover query.
    Query(Vec<String>, Expression<T>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
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
    pub fn solve(&self, degree: DegreeType) -> Option<DegreeType> {
        assert!(degree > 0, "Degree cannot be zero.");
        // the length of this expression is `a + b*x`
        let (a, b) = self.len();
        // it must match `degree`, and we solve for `x`
        if b == 0 {
            None
        } else {
            assert!(
                a <= degree,
                "Array literal is too large ({a}) for degree ({degree})."
            );
            assert_eq!((degree - a) % b, 0, "Cannot find a suitable value for `*`");
            Some((degree - a) / b)
        }
    }

    /// find the total length of an array expression as an affine expression: `a + b*x`
    fn len(&self) -> (DegreeType, DegreeType) {
        match self {
            ArrayExpression::RepeatedValue(e) => (0, e.len() as DegreeType),
            ArrayExpression::Value(e) => (e.len() as DegreeType, 0),
            ArrayExpression::Concat(left, right) => {
                let (a0, b0) = left.len();
                let (a1, b1) = right.len();

                assert!(
                    b0 == 0 || b1 == 0,
                    "`*` can be used only once in rhs of array definition"
                );

                (a0 + a1, b0 + b1)
            }
        }
    }
}
