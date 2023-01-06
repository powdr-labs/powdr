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
    ConstantDefinition(String, Expression),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expression {
    /// Reference to a constant, "%ConstantName"
    Constant(String),
    // TODO use bignum or something
    Number(u64),
    BinaryOperation(Box<Expression>, BinaryOperator, Box<Expression>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
}
