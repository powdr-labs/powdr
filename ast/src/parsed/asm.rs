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

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct MachineArguments {
    pub latch: Option<String>,
    pub operation_id: Option<String>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Default)]
pub struct ParamList {
    pub params: Vec<Param>,
}

impl ParamList {
    pub fn new(params: Vec<Param>) -> Self {
        Self { params }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Default)]
pub struct Params {
    pub inputs: ParamList,
    pub outputs: Option<ParamList>,
}

impl Params {
    pub fn new(inputs: ParamList, outputs: Option<ParamList>) -> Self {
        Self { inputs, outputs }
    }

    fn is_empty(&self) -> bool {
        self.inputs.params.is_empty()
            && self
                .outputs
                .as_ref()
                .map(|outputs| outputs.params.is_empty())
                .unwrap_or(true)
    }

    pub fn prepend_space_if_non_empty(&self) -> String {
        let mut params_str = self.to_string();
        if !self.is_empty() {
            params_str = format!(" {params_str}");
        }
        params_str
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
/// the operation id necessary to call this function from the outside
pub struct OperationId<T> {
    pub id: T,
}

#[derive(Debug, PartialEq, Eq, Clone, PartialOrd, Ord)]
pub struct Instruction<T> {
    pub params: Params,
    pub body: InstructionBody<T>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum MachineStatement<T> {
    Degree(usize, AbstractNumberType),
    Submachine(usize, String, String),
    RegisterDeclaration(usize, String, Option<RegisterFlag>),
    InstructionDeclaration(usize, String, Instruction<T>),
    LinkDeclaration(LinkDeclaration<T>),
    InlinePil(usize, Vec<PilStatement<T>>),
    FunctionDeclaration(usize, String, Params, Vec<FunctionStatement<T>>),
    OperationDeclaration(usize, String, OperationId<T>, Params),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct LinkDeclaration<T> {
    pub start: usize,
    pub flag: Expression<T>,
    pub params: Params,
    pub to: CallableRef,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct CallableRef {
    pub instance: String,
    pub callable: String,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum InstructionBody<T> {
    Local(Vec<InstructionBodyElement<T>>),
    CallableRef(CallableRef),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum FunctionStatement<T> {
    Assignment(usize, Vec<String>, Option<String>, Box<Expression<T>>),
    Instruction(usize, String, Vec<Expression<T>>),
    Label(usize, String),
    DebugDirective(usize, DebugDirective),
    Return(usize, Vec<Expression<T>>),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum DebugDirective {
    File(usize, String, String),
    Loc(usize, usize, usize),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum RegisterFlag {
    IsPC,
    IsAssignment,
    IsReadOnly,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct Param {
    pub name: String,
    pub ty: Option<String>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum InstructionBodyElement<T> {
    PolynomialIdentity(Expression<T>, Expression<T>),
    PlookupIdentity(
        SelectedExpressions<T>,
        PlookupOperator,
        SelectedExpressions<T>,
    ),
    FunctionCall(FunctionCall<T>),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct FunctionCall<T> {
    pub id: String,
    pub arguments: Vec<Expression<T>>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum PlookupOperator {
    In,
    Is,
}
