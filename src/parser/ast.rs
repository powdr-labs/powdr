#[derive(Debug, PartialEq, Eq)]
pub struct PILFile(pub Vec<Statement>);

#[derive(Debug, PartialEq, Eq)]
pub enum Statement {
    /// File name
    Include(String),
    /// Name of namespace and polynomial degree (constant)
    Namespace(String, Expression),
    PolynomialDefinition(String, Expression),
    PolynomialConstantDeclaration(Vec<PolynomialName>),
    PolynomialCommitDeclaration(Vec<PolynomialName>),
    PolynomialIdentity(Expression),
    PlookupIdentity(SelectedExpressions, SelectedExpressions),
    ConstantDefinition(String, Expression),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct SelectedExpressions {
    pub selector: Option<Expression>,
    pub expressions: Vec<Expression>,
}

// TODO use bignum or something
pub type ConstantNumberType = i128;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expression {
    /// Reference to a constant, "%ConstantName"
    Constant(String),
    PolynomialReference(PolynomialReference),
    Number(ConstantNumberType),
    BinaryOperation(Box<Expression>, BinaryOperator, Box<Expression>),
    UnaryOperation(UnaryOperator, Box<Expression>),
}

#[derive(Debug, PartialEq, Eq, Default)]
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
    Pow,
}
