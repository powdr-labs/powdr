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

// TODO use bignum or something
pub type ConstantNumberType = i128;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expression {
    /// Reference to a constant, "%ConstantName"
    Constant(String),
    PolynomialReference(PolynomialReference),
    PublicReference(String),
    Number(ConstantNumberType),
    String(String),
    Tuple(Vec<Expression>),
    BinaryOperation(Box<Expression>, BinaryOperator, Box<Expression>),
    UnaryOperation(UnaryOperator, Box<Expression>),
    FunctionCall(String, Vec<Expression>),
    FreeInput(Box<Expression>),
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
    BinaryOr,
    ShiftLeft,
    ShiftRight,
}

/// The definition of a function (excluding its name):
/// Either a param-value mapping or an array of values.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum FunctionDefinition {
    /// Parameter-value-mapping.
    Mapping(Vec<String>, Expression),
    /// Array of values.
    Array(Vec<Expression>),
    /// Prover query.
    Query(Vec<String>, Expression),
}
