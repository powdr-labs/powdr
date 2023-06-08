use number::AbstractNumberType;

use super::{Expression, SelectedExpressions, Statement};

#[derive(Debug, PartialEq, Eq)]
pub struct ASMFile<T>(pub Vec<ASMStatement<T>>);

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
pub enum ASMStatement<T> {
    Degree(usize, AbstractNumberType),
    RegisterDeclaration(usize, String, Option<RegisterFlag>),
    InstructionDeclaration(
        usize,
        String,
        InstructionParams,
        Vec<InstructionBodyElement<T>>,
    ),
    InlinePil(usize, Vec<Statement<T>>),
    Assignment(usize, Vec<String>, Option<String>, Box<Expression<T>>),
    Instruction(usize, String, Vec<Expression<T>>),
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
pub enum InstructionBodyElement<T> {
    Expression(Expression<T>),
    PlookupIdentity(
        SelectedExpressions<T>,
        PlookupOperator,
        SelectedExpressions<T>,
    ),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum PlookupOperator {
    In,
    Is,
}
