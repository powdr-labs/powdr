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
    pub arguments: MachineArguments,
    pub statements: Vec<MachineStatement<T>>,
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct MachineArguments {
    pub latch: Option<String>,
    pub function_id: Option<String>,
}

#[derive(Debug, PartialEq, Eq, Clone, Default)]
pub struct ParamList {
    pub params: Vec<Param>,
}

impl ParamList {
    pub fn new(params: Vec<Param>) -> Self {
        Self { params }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Default)]
pub struct Params {
    pub inputs: ParamList,
    pub outputs: Option<ParamList>,
}

impl Params {
    pub fn new(inputs: ParamList, outputs: Option<ParamList>) -> Self {
        Self { inputs, outputs }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
/// the function id necessary to call this function from the outside
pub struct FunctionId<T> {
    pub id: T,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum MachineStatement<T> {
    Degree(usize, AbstractNumberType),
    Submachine(usize, String, String),
    RegisterDeclaration(usize, String, Option<RegisterFlag>),
    InstructionDeclaration(usize, String, Params, InstructionBody<T>),
    InlinePil(usize, Vec<PilStatement<T>>),
    FunctionDeclaration(
        usize,
        String,
        // function id, explicit for static machines
        Option<FunctionId<T>>,
        Params,
        Vec<FunctionStatement<T>>,
    ),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum InstructionBody<T> {
    Local(Vec<InstructionBodyElement<T>>),
    External(String, String),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum FunctionStatement<T> {
    Assignment(usize, Vec<String>, Option<String>, Box<Expression<T>>),
    Instruction(usize, String, Vec<Expression<T>>),
    Label(usize, String),
    DebugDirective(usize, DebugDirective),
    Return(usize, Vec<Expression<T>>),
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
    IsReadOnly,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Param {
    pub name: String,
    pub ty: Option<String>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum InstructionBodyElement<T> {
    PolynomialIdentity(Expression<T>, Expression<T>),
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
