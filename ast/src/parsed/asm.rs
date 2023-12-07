use std::{fmt::Display, iter::once};

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

/// A symbol path is a sequence of strings separated by ``::`.
/// It can contain the special word `super`, which goes up a level.
/// If it does not start with `::`, it is relative.
#[derive(Default, Debug, PartialEq, Eq, Clone, PartialOrd, Ord)]
pub struct SymbolPath {
    /// The parts between each `::`.
    pub parts: Vec<Part>,
}

impl SymbolPath {
    pub fn join<P: Into<Self>>(mut self, other: P) -> Self {
        self.parts.extend(other.into().parts);
        self
    }
}

/// An absolute symbol path is a resolved SymbolPath,
/// which means it has to start with `::` and it cannot contain
/// the word `super`.
#[derive(Default, Debug, PartialEq, Eq, Clone, PartialOrd, Ord)]
pub struct AbsoluteSymbolPath {
    /// Contains the parts after the initial `::`.
    pub parts: Vec<String>,
}

/// Parses a path like `::path::to::symbol`.
/// Panics if the path does not start with '::'.
pub fn parse_absolute_path(s: &str) -> AbsoluteSymbolPath {
    match s.strip_prefix("::") {
        Some(s) => s
            .split("::")
            .fold(AbsoluteSymbolPath::default(), |path, part| {
                path.with_part(part)
            }),
        None => panic!("Absolute symbol path does not start with '::': {s}"),
    }
}

impl AbsoluteSymbolPath {
    pub fn pop(&mut self) -> Option<String> {
        self.parts.pop()
    }
}

impl AbsoluteSymbolPath {
    /// Resolves a relative path in the context of this absolute path.
    pub fn join<P: Into<SymbolPath> + Display>(mut self, other: P) -> Self {
        for part in other.into().parts {
            match part {
                Part::Super => {
                    self.pop().unwrap();
                }
                Part::Named(name) => {
                    if name.is_empty() {
                        self.parts.clear();
                    } else {
                        self.parts.push(name);
                    }
                }
            }
        }
        self
    }

    /// Appends a part to the end of the path and returns a new copy.
    pub fn with_part(&self, part: &str) -> Self {
        assert!(!part.is_empty());
        let mut parts = self.parts.clone();
        parts.push(part.to_string());
        Self { parts }
    }
}

impl From<AbsoluteSymbolPath> for SymbolPath {
    fn from(value: AbsoluteSymbolPath) -> Self {
        Self {
            parts: once(String::new())
                .chain(value.parts)
                .map(Part::Named)
                .collect(),
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
pub struct ParamList<T> {
    pub params: Vec<Param<T>>,
}

impl<T> ParamList<T> {
    pub fn new(params: Vec<Param<T>>) -> Self {
        Self { params }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Default)]
pub struct Params<T> {
    pub inputs: ParamList<T>,
    pub outputs: Option<ParamList<T>>,
}

impl<T: Display> Params<T> {
    pub fn new(inputs: ParamList<T>, outputs: Option<ParamList<T>>) -> Self {
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
    pub id: Option<T>,
}

#[derive(Debug, PartialEq, Eq, Clone, PartialOrd, Ord)]
pub struct Instruction<T> {
    pub params: Params<T>,
    pub body: InstructionBody<T>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum MachineStatement<T> {
    Degree(usize, AbstractNumberType),
    Pil(usize, PilStatement<T>),
    Submachine(usize, SymbolPath, String),
    RegisterDeclaration(usize, String, Option<RegisterFlag>),
    InstructionDeclaration(usize, String, Instruction<T>),
    LinkDeclaration(LinkDeclaration<T>),
    FunctionDeclaration(usize, String, Params<T>, Vec<FunctionStatement<T>>),
    OperationDeclaration(usize, String, OperationId<T>, Params<T>),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct LinkDeclaration<T> {
    pub start: usize,
    pub flag: Expression<T>,
    pub params: Params<T>,
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

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum AssignmentRegister {
    Register(String),
    Wildcard,
}

impl AssignmentRegister {
    pub fn unwrap(self) -> String {
        match self {
            AssignmentRegister::Register(r) => r,
            AssignmentRegister::Wildcard => panic!("cannot unwrap wildcard"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum FunctionStatement<T> {
    Assignment(
        usize,
        Vec<String>,
        Option<Vec<AssignmentRegister>>,
        Box<Expression<T>>,
    ),
    Instruction(usize, String, Vec<Expression<T>>),
    Label(usize, String),
    DebugDirective(usize, DebugDirective),
    Return(usize, Vec<Expression<T>>),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum DebugDirective {
    File(usize, String, String),
    Loc(usize, usize, usize),
    OriginalInstruction(String),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum RegisterFlag {
    IsPC,
    IsAssignment,
    IsReadOnly,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct Param<T> {
    pub name: String,
    pub index: Option<T>,
    pub ty: Option<String>,
}
