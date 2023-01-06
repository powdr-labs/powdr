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
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expression {
    /// Reference to a constant, "%ConstantName"
    Constant(String),
}
