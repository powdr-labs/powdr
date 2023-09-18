use std::collections::VecDeque;

use number::AbstractNumberType;

use derive_more::From;

use super::{Expression, PilStatement};

#[derive(Default, Debug, PartialEq, Eq)]
pub struct ASMProgram<T> {
    pub main: ASMModule<T>,
}

#[derive(Default, Debug, PartialEq, Eq)]
pub struct ASMModule<T> {
    pub statements: Vec<ModuleStatement<T>>,
}

impl<T> ASMModule<T> {
    pub fn symbol_definitions(&self) -> impl Iterator<Item = &SymbolDefinition<T>> {
        self.statements.iter().map(|s| match s {
            ModuleStatement::SymbolDefinition(d) => d,
        })
    }
}

#[derive(Debug, PartialEq, Eq, From)]
pub enum ModuleStatement<T> {
    SymbolDefinition(SymbolDefinition<T>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct SymbolDefinition<T> {
    pub name: String,
    pub value: SymbolValue<T>,
}

#[derive(Debug, PartialEq, Eq, From)]
pub enum SymbolValue<T> {
    /// A machine definition
    Machine(Machine<T>),
    /// An import of a symbol from another module
    Import(Import),
    /// A module definition
    Module(Module<T>),
}

impl<T> SymbolValue<T> {
    pub fn as_ref(&self) -> SymbolValueRef<T> {
        match self {
            SymbolValue::Machine(machine) => SymbolValueRef::Machine(machine),
            SymbolValue::Import(i) => SymbolValueRef::Import(i),
            SymbolValue::Module(m) => SymbolValueRef::Module(m.as_ref()),
        }
    }
}

#[derive(Debug, PartialEq, Eq, From)]
pub enum SymbolValueRef<'a, T> {
    /// A machine definition
    Machine(&'a Machine<T>),
    /// An import of a symbol from another module
    Import(&'a Import),
    /// A module definition
    Module(ModuleRef<'a, T>),
}

#[derive(Debug, PartialEq, Eq, From)]
pub enum Module<T> {
    External(String),
    Local(ASMModule<T>),
}

impl<T> Module<T> {
    fn as_ref(&self) -> ModuleRef<T> {
        match self {
            Module::External(n) => ModuleRef::External(n),
            Module::Local(m) => ModuleRef::Local(m),
        }
    }
}

#[derive(Debug, PartialEq, Eq, From)]
pub enum ModuleRef<'a, T> {
    External(&'a str),
    Local(&'a ASMModule<T>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Import {
    /// the path imported in the source
    pub path: SymbolPath,
}

#[derive(Default, Debug, PartialEq, Eq, Clone, PartialOrd, Ord)]
pub struct SymbolPath {
    pub parts: VecDeque<Part>,
}

#[derive(Default, Debug, PartialEq, Eq, Clone, PartialOrd, Ord)]
pub struct AbsoluteSymbolPath {
    pub parts: VecDeque<String>,
}

impl<S: Into<String>> From<S> for AbsoluteSymbolPath {
    fn from(name: S) -> Self {
        Self {
            parts: [name.into()].into(),
        }
    }
}

/// parses a path like `path::to::symbol`
pub fn parse_absolute_path(s: &str) -> AbsoluteSymbolPath {
    s.split("::")
        .fold(AbsoluteSymbolPath::default(), |path, part| path.join(part))
}

impl AbsoluteSymbolPath {
    pub fn pop_front(&mut self) -> Option<String> {
        self.parts.pop_front()
    }

    pub fn pop_back(&mut self) -> Option<String> {
        self.parts.pop_back()
    }
}

impl AbsoluteSymbolPath {
    pub fn join<P: Into<SymbolPath>>(self, other: P) -> Self {
        other
            .into()
            .parts
            .into_iter()
            .fold(self, |mut acc, part| match part {
                Part::Super => {
                    acc.pop_back().unwrap();
                    acc
                }
                Part::Named(name) => {
                    acc.parts.push_back(name);
                    acc
                }
            })
    }
}

impl From<AbsoluteSymbolPath> for SymbolPath {
    fn from(value: AbsoluteSymbolPath) -> Self {
        Self {
            parts: value.parts.into_iter().map(Part::Named).collect(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, PartialOrd, Ord)]
pub enum Part {
    Super,
    Named(String),
}

impl TryInto<String> for Part {
    type Error = ();

    fn try_into(self) -> Result<String, Self::Error> {
        if let Part::Named(name) = self {
            Ok(name)
        } else {
            Err(())
        }
    }
}

impl SymbolPath {
    pub fn pop_front(&mut self) -> Option<Part> {
        self.parts.pop_front()
    }

    pub fn pop_back(&mut self) -> Option<Part> {
        self.parts.pop_back()
    }
}

impl<S: Into<String>> From<S> for SymbolPath {
    fn from(name: S) -> Self {
        Self {
            parts: [Part::Named(name.into())].into(),
        }
    }
}

impl SymbolPath {
    pub fn join<P: Into<Self>>(mut self, other: P) -> Self {
        self.parts.extend(other.into().parts);
        self
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Machine<T> {
    pub arguments: MachineArguments,
    pub statements: Vec<MachineStatement<T>>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Default, Clone)]
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
    Submachine(usize, SymbolPath, String),
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
    Local(Vec<PilStatement<T>>),
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
