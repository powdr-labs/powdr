use std::{
    fmt::{Display, Formatter, Result},
    iter::{empty, once, repeat},
};

use itertools::Itertools;
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
    /// A generic symbol / function.
    Expression(Expression<T>),
}

impl<T> SymbolValue<T> {
    pub fn as_ref(&self) -> SymbolValueRef<T> {
        match self {
            SymbolValue::Machine(machine) => SymbolValueRef::Machine(machine),
            SymbolValue::Import(i) => SymbolValueRef::Import(i),
            SymbolValue::Module(m) => SymbolValueRef::Module(m.as_ref()),
            SymbolValue::Expression(e) => SymbolValueRef::Expression(e),
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
    /// A generic symbol / function.
    Expression(&'a Expression<T>),
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
    parts: Vec<Part>,
}

impl SymbolPath {
    pub fn from_identifier(name: String) -> Self {
        Self {
            parts: vec![Part::Named(name)],
        }
    }

    pub fn from_parts<P: IntoIterator<Item = Part>>(parts: P) -> Self {
        Self {
            parts: parts.into_iter().collect(),
        }
    }

    pub fn join<P: Into<Self>>(mut self, other: P) -> Self {
        self.parts.extend(other.into().parts);
        self
    }

    /// Formats the path and uses `.` as separator if
    /// there are at most two components.
    pub fn to_dotted_string(&self) -> String {
        let separator = if self.parts.len() <= 2 { "." } else { "::" };
        self.parts.iter().format(separator).to_string()
    }

    pub fn try_to_identifier(&self) -> Option<&String> {
        match &self.parts[..] {
            [Part::Named(name)] => Some(name),
            _ => None,
        }
    }

    pub fn try_last_part_mut(&mut self) -> Option<&mut String> {
        self.parts.last_mut().and_then(|p| match p {
            Part::Super => None,
            Part::Named(n) => Some(n),
        })
    }

    pub fn try_last_part(&self) -> Option<&String> {
        self.parts.last().and_then(|p| match p {
            Part::Super => None,
            Part::Named(n) => Some(n),
        })
    }

    /// Returns the last part of the path. Panics if it is "super" or if the path is empty.
    pub fn name(&self) -> &String {
        self.try_last_part().unwrap()
    }

    pub fn parts(&self) -> impl DoubleEndedIterator + ExactSizeIterator<Item = &Part> {
        self.parts.iter()
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

impl Display for SymbolPath {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.parts.iter().format("::"))
    }
}

/// An absolute symbol path is a resolved SymbolPath,
/// which means it has to start with `::` and it cannot contain
/// the word `super`.
#[derive(Default, Debug, PartialEq, Eq, Clone, PartialOrd, Ord)]
pub struct AbsoluteSymbolPath {
    /// Contains the parts after the initial `::`.
    parts: Vec<String>,
}

/// Parses a path like `::path::to::symbol`.
/// Panics if the path does not start with '::'.
pub fn parse_absolute_path(s: &str) -> AbsoluteSymbolPath {
    match s.strip_prefix("::") {
        Some("") => AbsoluteSymbolPath::default(),
        Some(s) => s
            .split("::")
            .fold(AbsoluteSymbolPath::default(), |path, part| {
                path.with_part(part)
            }),
        None => panic!("Absolute symbol path does not start with '::': {s}"),
    }
}

impl AbsoluteSymbolPath {
    /// Removes and returns the last path component (unless empty).
    pub fn pop(&mut self) -> Option<String> {
        self.parts.pop()
    }

    /// Returns the path one level higher.
    pub fn parent(mut self) -> AbsoluteSymbolPath {
        self.pop().unwrap();
        self
    }

    pub fn len(&self) -> usize {
        self.parts.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn parts(&self) -> impl DoubleEndedIterator + ExactSizeIterator<Item = &str> {
        self.parts.iter().map(|p| p.as_str())
    }

    /// Returns an iterator over all paths (not parts!) from self to the root.
    pub fn iter_to_root(&self) -> impl Iterator<Item = AbsoluteSymbolPath> + '_ {
        (0..=self.parts.len()).rev().map(|i| AbsoluteSymbolPath {
            parts: self.parts[..i].to_vec(),
        })
    }

    /// Appends a part to the end of the path.
    pub fn push(&mut self, part: String) {
        self.parts.push(part);
    }

    /// Returns the relative path from base to self.
    /// In other words, base.join(self.relative_to(base)) == self.
    pub fn relative_to(&self, base: &AbsoluteSymbolPath) -> SymbolPath {
        let common_prefix_len = self.common_prefix(base).parts.len();
        // Start with max(0, base.parts.len() - common_root.parts.len())
        // repetitions of "super".
        let parts = repeat(Part::Super)
            .take(base.parts.len().saturating_sub(common_prefix_len))
            // append the parts of self after the common root.
            .chain(
                self.parts
                    .iter()
                    .skip(common_prefix_len)
                    .cloned()
                    .map(Part::Named),
            )
            .collect();
        SymbolPath { parts }
    }

    /// Returns the common prefix of two paths.
    pub fn common_prefix(&self, other: &AbsoluteSymbolPath) -> AbsoluteSymbolPath {
        let parts = self
            .parts
            .iter()
            .zip(other.parts.iter())
            .map_while(|(a, b)| if a == b { Some(a.clone()) } else { None })
            .collect();

        AbsoluteSymbolPath { parts }
    }

    /// Resolves a relative path in the context of this absolute path.
    pub fn join<P: Into<SymbolPath>>(mut self, other: P) -> Self {
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

    /// Formats the path without leading `::` and uses `.` as separator if
    /// there are at most two components.
    pub fn to_dotted_string(&self) -> String {
        let separator = if self.parts.len() <= 2 { "." } else { "::" };
        self.parts.join(separator)
    }
}

impl Display for AbsoluteSymbolPath {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "::{}", self.parts.iter().format("::"))
    }
}

#[derive(Debug, PartialEq, Eq, Clone, PartialOrd, Ord)]
pub enum Part {
    Super,
    Named(String),
}

impl TryInto<String> for Part {
    type Error = ();

    fn try_into(self) -> std::result::Result<String, Self::Error> {
        if let Part::Named(name) = self {
            Ok(name)
        } else {
            Err(())
        }
    }
}

impl Display for Part {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Part::Super => write!(f, "super"),
            Part::Named(name) => write!(f, "{name}"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Machine<T> {
    pub arguments: MachineArguments,
    pub statements: Vec<MachineStatement<T>>,
}

impl<T: Clone> Machine<T> {
    /// Returns a vector of all local variables / names defined in the machine.
    pub fn local_names(&self) -> Box<dyn Iterator<Item = &String> + '_> {
        Box::new(self.statements.iter().flat_map(|s| match s {
            MachineStatement::RegisterDeclaration(_, name, _) => Box::new(once(name)),
            MachineStatement::Pil(_, statement) => statement.symbol_definition_names(),
            MachineStatement::Degree(_, _)
            | MachineStatement::Submachine(_, _, _)
            | MachineStatement::InstructionDeclaration(_, _, _)
            | MachineStatement::LinkDeclaration(_)
            | MachineStatement::FunctionDeclaration(_, _, _, _)
            | MachineStatement::OperationDeclaration(_, _, _, _) => Box::new(empty()),
        }))
    }
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn common_prefix() {
        assert_eq!(
            parse_absolute_path("::a::b").common_prefix(&parse_absolute_path("::a::c")),
            parse_absolute_path("::a")
        );
        assert_eq!(
            parse_absolute_path("::a::b").common_prefix(&parse_absolute_path("::a")),
            parse_absolute_path("::a")
        );
        assert_eq!(
            parse_absolute_path("::a").common_prefix(&parse_absolute_path("::a::c")),
            parse_absolute_path("::a")
        );
        assert_eq!(
            parse_absolute_path("::x").common_prefix(&parse_absolute_path("::y::t")),
            parse_absolute_path("::")
        );
        assert_eq!(
            parse_absolute_path("::x::r::v").common_prefix(&parse_absolute_path("::x::r::t")),
            parse_absolute_path("::x::r")
        );
    }

    #[test]
    fn relative_to() {
        assert_eq!(
            parse_absolute_path("::a::b")
                .relative_to(&parse_absolute_path("::a::c"))
                .to_string(),
            "super::b".to_string()
        );
        assert_eq!(
            parse_absolute_path("::a::b")
                .relative_to(&parse_absolute_path("::a"))
                .to_string(),
            "b".to_string()
        );
        assert_eq!(
            parse_absolute_path("::x")
                .relative_to(&parse_absolute_path("::y::t"))
                .to_string(),
            "super::super::x".to_string()
        );
        assert_eq!(
            parse_absolute_path("::x::r::v")
                .relative_to(&parse_absolute_path("::x::r"))
                .to_string(),
            "v".to_string()
        );
        assert_eq!(
            parse_absolute_path("::x")
                .relative_to(&parse_absolute_path("::x::t::k"))
                .to_string(),
            "super::super".to_string()
        );
        assert_eq!(
            parse_absolute_path("::x")
                .relative_to(&parse_absolute_path("::x"))
                .to_string(),
            "".to_string()
        );
    }

    #[test]
    fn relative_to_join() {
        let v = parse_absolute_path("::x::r::v");
        let base = parse_absolute_path("::x::t");
        let rel = v.relative_to(&base);
        assert_eq!(base.join(rel), v);
    }
}
