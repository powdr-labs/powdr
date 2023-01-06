#[derive(Debug, PartialEq, Eq)]
pub struct PILFile(pub Vec<Statement>);

#[derive(Debug, PartialEq, Eq)]
pub enum Statement {
    /// File name
    Include(String),
    /// Name of namespace and polynomial degree (constant)
    Namespace(String, Expression),
    PolynomialDefinition(String, Expression),
    PolynomialConstantDeclaration(String),
    PolynomialCommitDeclaration(String),
    PolynomialIdentity(Expression),
    PlookupIdentity(SelectedExpressions, SelectedExpressions),
    ConstantDefinition(String, Expression),
}

#[derive(Debug, PartialEq, Eq)]
pub struct SelectedExpressions {
    pub selector: Option<Expression>,
    pub expressions: Vec<Expression>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expression {
    /// Reference to a constant, "%ConstantName"
    Constant(String),
    PolynomialReference(PolynomialReference),
    // TODO use bignum or something
    Number(u64),
    BinaryOperation(Box<Expression>, BinaryOperator, Box<Expression>),
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct PolynomialReference {
    pub namespace: Option<String>,
    pub name: String,
    pub index: Option<Box<Expression>>,
    pub next: bool,
}

#[derive(Debug, PartialEq, Eq)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
}
