use number::AbstractNumberType;

use super::{Expression, PilStatement, SelectedExpressions};

#[derive(Debug, PartialEq, Eq)]
pub struct ASMFile<T> {
    pub machines: Vec<Machine<T>>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Machine<T> {
    pub start: usize,
    pub name: String,
    pub statements: Vec<MachineStatement<T>>,
}

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
pub enum MachineStatement<T> {
    Degree(usize, AbstractNumberType),
    Submachine(usize, String, String),
    RegisterDeclaration(usize, String, Option<RegisterFlag>),
    InstructionDeclaration(
        usize,
        Latch<T>,
        String,
        InstructionParams,
        InstructionBody<T>,
    ),
    InlinePil(usize, Vec<PilStatement<T>>),
    Program(usize, Vec<ProgramStatement<T>>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Latch<T> {
    All,
    When(Expression<T>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum InstructionBody<T> {
    Local(Vec<InstructionBodyElement<T>>),
    External(String, String),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ProgramStatement<T> {
    Assignment(usize, Vec<String>, Option<String>, Box<Expression<T>>),
    Instruction(usize, String, Vec<Expression<T>>),
    Label(usize, String),
    DebugDirective(usize, DebugDirective),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum DebugDirective {
    File(usize, String, String),
    Loc(usize, usize, usize),
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
    FunctionCall(FunctionCall<T>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FunctionCall<T> {
    pub id: String,
    pub arguments: Vec<Expression<T>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum PlookupOperator {
    In,
    Is,
}
