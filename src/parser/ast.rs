use crate::number::{AbstractNumberType, DegreeType};

#[derive(Debug, PartialEq, Eq)]
pub struct PILFile(pub Vec<Statement>);

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Statement {
    /// File name
    Include(usize, String),
    /// Name of namespace and polynomial degree (constant)
    Namespace(usize, String, Expression),
    PolynomialDefinition(usize, String, Expression),
    PublicDeclaration(usize, String, PolynomialReference, Expression),
    PolynomialConstantDeclaration(usize, Vec<PolynomialName>),
    PolynomialConstantDefinition(usize, String, FunctionDefinition),
    PolynomialCommitDeclaration(usize, Vec<PolynomialName>, Option<FunctionDefinition>),
    PolynomialIdentity(usize, Expression),
    PlookupIdentity(usize, SelectedExpressions, SelectedExpressions),
    PermutationIdentity(usize, SelectedExpressions, SelectedExpressions),
    ConnectIdentity(usize, Vec<Expression>, Vec<Expression>),
    ConstantDefinition(usize, String, Expression),
    MacroDefinition(
        usize,
        String,
        Vec<String>,
        Vec<Statement>,
        Option<Expression>,
    ),
    FunctionCall(usize, String, Vec<Expression>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct SelectedExpressions {
    pub selector: Option<Expression>,
    pub expressions: Vec<Expression>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expression {
    /// Reference to a constant, "%ConstantName"
    Constant(String),
    PolynomialReference(PolynomialReference),
    PublicReference(String),
    Number(AbstractNumberType),
    String(String),
    Tuple(Vec<Expression>),
    BinaryOperation(Box<Expression>, BinaryOperator, Box<Expression>),
    UnaryOperation(UnaryOperator, Box<Expression>),
    FunctionCall(String, Vec<Expression>),
    FreeInput(Box<Expression>),
    MatchExpression(
        Box<Expression>,
        Vec<(Option<AbstractNumberType>, Expression)>,
    ),
}

#[derive(Debug, PartialEq, Eq, Default, Clone)]
pub struct PolynomialName {
    pub name: String,
    pub array_size: Option<Expression>,
}

#[derive(Debug, PartialEq, Eq, Default, Clone)]
pub struct PolynomialReference {
    pub namespace: Option<String>,
    pub name: String,
    pub index: Option<Box<Expression>>,
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
pub enum FunctionDefinition {
    /// Parameter-value-mapping.
    Mapping(Vec<String>, Expression),
    /// Array expression.
    Array(ArrayExpression),
    /// Prover query.
    Query(Vec<String>, Expression),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ArrayExpression {
    Value(Vec<Expression>),
    RepeatedValue(Vec<Expression>),
    Concat(Box<ArrayExpression>, Box<ArrayExpression>),
}

impl ArrayExpression {
    pub fn value(v: Vec<Expression>) -> Self {
        Self::Value(v)
    }

    pub fn repeated_value(v: Vec<Expression>) -> Self {
        Self::RepeatedValue(v)
    }

    pub fn concat(self, other: Self) -> Self {
        Self::Concat(Box::new(self), Box::new(other))
    }

    pub fn pad_with_zeroes(self) -> Self {
        Self::concat(
            self,
            Self::repeated_value(vec![Expression::Number(0.into())]),
        )
    }
}

impl ArrayExpression {
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
            ArrayExpression::RepeatedValue(e) => (0, e.len() as u64),
            ArrayExpression::Value(e) => (e.len() as u64, 0),
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
