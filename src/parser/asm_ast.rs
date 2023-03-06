use super::ast::{Expression, SelectedExpressions, Statement};

#[derive(Debug, PartialEq, Eq)]
pub struct ASMFile(pub Vec<ASMStatement>);

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ASMStatement {
    RegisterDeclaration(usize, String, Option<RegisterFlag>),
    InstructionDeclaration(
        usize,
        String,
        Vec<InstructionParam>,
        Vec<InstructionBodyElement>,
    ),
    InlinePil(usize, Vec<Statement>),
    Assignment(usize, Vec<String>, Option<String>, Box<Expression>),
    Instruction(usize, String, Vec<Expression>),
    Label(usize, String),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum RegisterFlag {
    IsPC,
    IsDefaultAssignment,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct InstructionParam {
    pub name: String,
    pub param_type: Option<String>,
    /// Which register this parameter is passed in (first) and out (second).
    /// It is a double option, because the arrow can be optional and the
    /// assign register inside the arrow is optional as well.
    pub assignment_reg: (Option<Option<String>>, Option<Option<String>>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum InstructionBodyElement {
    Expression(Expression),
    PlookupIdentity(SelectedExpressions, SelectedExpressions),
}
