use number::AbstractNumberType;

use super::ast::{Expression, SelectedExpressions, Statement};

#[derive(Debug, PartialEq, Eq)]
pub struct ASMFile(pub Vec<ASMStatement>);

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct InstructionParamList {
    pub params: Vec<InstructionParam>,
}

impl InstructionParamList {
    pub fn new(params: Vec<InstructionParam>) -> Self {
        Self { params }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct InstructionParams {
    pub inputs: InstructionParamList,
    pub outputs: Option<InstructionParamList>,
}

impl InstructionParams {
    pub fn new(inputs: InstructionParamList, outputs: Option<InstructionParamList>) -> Self {
        Self { inputs, outputs }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ASMStatement {
    Degree(usize, AbstractNumberType),
    RegisterDeclaration(usize, String, Option<RegisterFlag>),
    InstructionDeclaration(
        usize,
        String,
        InstructionParams,
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
    IsAssignment,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct InstructionParam {
    pub name: String,
    pub ty: Option<String>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum InstructionBodyElement {
    Expression(Expression),
    PlookupIdentity(SelectedExpressions, PlookupOperator, SelectedExpressions),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum PlookupOperator {
    In,
    Is,
}
