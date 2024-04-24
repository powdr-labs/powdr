mod display;

use std::{
    collections::{
        btree_map::{IntoIter, Iter, IterMut},
        BTreeMap, BTreeSet,
    },
    iter::{once, repeat},
    ops::ControlFlow,
};

use itertools::Either;

use crate::parsed::{
    asm::{
        AbsoluteSymbolPath, AssignmentRegister, CallableRef, FunctionParams, InstructionBody,
        InstructionParams, OperationId, OperationParams,
    },
    visitor::{ExpressionVisitable, VisitOrder},
    EnumDeclaration, NamespacedPolynomialReference, PilStatement, TypedExpression,
};
use crate::SourceRef;

pub use crate::parsed::Expression;

#[derive(Clone, Debug)]
pub struct RegisterDeclarationStatement {
    pub source: SourceRef,
    pub name: String,
    pub ty: RegisterTy,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum RegisterTy {
    Pc,
    Assignment,
    Write,
    ReadOnly,
}

impl RegisterTy {
    pub fn is_write(&self) -> bool {
        self == &Self::Write
    }

    pub fn is_assignment(&self) -> bool {
        self == &Self::Assignment
    }

    pub fn is_read_only(&self) -> bool {
        self == &Self::ReadOnly
    }

    pub fn is_pc(&self) -> bool {
        self == &Self::Pc
    }
}

#[derive(Clone, Debug)]
pub struct InstructionDefinitionStatement {
    pub source: SourceRef,
    pub name: String,
    pub instruction: Instruction,
}

#[derive(Clone, Debug)]
pub struct Instruction {
    pub params: InstructionParams,
    pub body: InstructionBody,
}

#[derive(Clone, Debug)]
pub struct LinkDefinitionStatement {
    pub source: SourceRef,
    /// the flag which activates this link. Should be boolean.
    pub flag: Expression,
    /// the callable to invoke when the flag is on. TODO: check this during type checking
    pub to: CallableRef,
    /// true if this is a permutation link
    pub is_permutation: bool,
}

#[derive(Clone, Debug, Default)]
pub struct FunctionStatements {
    inner: Vec<FunctionStatement>,
    batches: Option<Vec<BatchMetadata>>,
}

pub struct BatchRef<'a> {
    pub statements: &'a [FunctionStatement],
    reason: &'a Option<IncompatibleSet>,
}

pub struct Batch {
    pub statements: Vec<FunctionStatement>,
    reason: Option<IncompatibleSet>,
}

impl From<Vec<FunctionStatement>> for Batch {
    fn from(statements: Vec<FunctionStatement>) -> Self {
        Self {
            statements,
            reason: None,
        }
    }
}

impl Batch {
    pub fn set_reason(&mut self, reason: IncompatibleSet) {
        self.reason = Some(reason);
    }

    pub fn reason(mut self, reason: IncompatibleSet) -> Self {
        self.reason = Some(reason);
        self
    }
}

impl FunctionStatements {
    /// create with no batch information
    pub fn new(inner: Vec<FunctionStatement>) -> Self {
        Self {
            inner,
            batches: None,
        }
    }

    /// turn into the underlying statements, forgetting batch information
    pub fn into_inner(self) -> Vec<FunctionStatement> {
        self.inner
    }

    /// set the batch information
    pub fn set_batches(&mut self, batches: Vec<BatchMetadata>) {
        self.batches = Some(batches);
    }

    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    /// iterate over the statements by reference
    pub fn iter(&self) -> impl Iterator<Item = &FunctionStatement> {
        self.inner.iter()
    }

    /// iterate over the statements by mutable reference
    /// Warning: mutation should be checked not to invalidate batch information
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut FunctionStatement> {
        self.inner.iter_mut()
    }

    /// iterate over the batches by reference
    pub fn iter_batches(&self) -> impl Iterator<Item = BatchRef> {
        match &self.batches {
            Some(batches) => Either::Left(batches.iter()),
            None => Either::Right(
                repeat(&BatchMetadata {
                    size: 1,
                    reason: None,
                })
                .take(self.inner.len()),
            ),
        }
        .scan(0, move |start, batch| {
            let res = BatchRef {
                reason: &batch.reason,
                statements: &self.inner[*start..*start + batch.size],
            };
            *start += batch.size;
            Some(res)
        })
    }
}

impl FunctionStatements {
    /// iterate over the batches by reference
    pub fn into_iter_batches(self) -> impl Iterator<Item = Batch> {
        let len = self.inner.len();
        let mut inner = self.inner.into_iter();

        match self.batches {
            Some(batches) => Either::Left(batches.into_iter()),
            None => Either::Right(
                repeat(BatchMetadata {
                    size: 1,
                    reason: None,
                })
                .take(len),
            ),
        }
        .map(move |batch| Batch {
            reason: batch.reason,
            statements: (&mut inner).take(batch.size).collect(),
        })
    }
}

impl FromIterator<Batch> for FunctionStatements {
    fn from_iter<I: IntoIterator<Item = Batch>>(iter: I) -> Self {
        let mut inner = vec![];
        let mut batches = vec![];

        for batch in iter {
            batches.push(BatchMetadata {
                size: batch.statements.len(),
                reason: batch.reason,
            });
            inner.extend(batch.statements);
        }

        FunctionStatements {
            inner,
            batches: Some(batches),
        }
    }
}

#[derive(Clone, Debug)]
pub struct FunctionBody {
    pub statements: FunctionStatements,
}

#[derive(Debug)]
pub struct CallableSymbolDefinitionRef<'a> {
    /// the name of this symbol
    pub name: &'a str,
    /// a reference to the symbol
    pub symbol: &'a CallableSymbol,
}

#[derive(Debug)]
pub struct CallableSymbolDefinitionMut<'a> {
    /// the name of this symbol
    pub name: &'a str,
    /// a mutable reference to the symbol
    pub symbol: &'a mut CallableSymbol,
}

#[derive(Debug)]
pub struct CallableSymbolDefinition {
    /// the name of this symbol
    pub name: String,
    /// the symbol
    pub symbol: CallableSymbol,
}

#[derive(Clone, Debug, Default)]
pub struct CallableSymbolDefinitions(pub BTreeMap<String, CallableSymbol>);

impl IntoIterator for CallableSymbolDefinitions {
    type Item = CallableSymbolDefinition;

    type IntoIter = std::iter::Map<
        IntoIter<String, CallableSymbol>,
        fn((String, CallableSymbol)) -> CallableSymbolDefinition,
    >;

    fn into_iter(self) -> Self::IntoIter {
        self.0
            .into_iter()
            .map(|(name, symbol)| CallableSymbolDefinition { name, symbol })
    }
}

impl<'a> IntoIterator for &'a CallableSymbolDefinitions {
    type Item = CallableSymbolDefinitionRef<'a>;

    type IntoIter = std::iter::Map<
        Iter<'a, String, CallableSymbol>,
        fn((&'a String, &'a CallableSymbol)) -> CallableSymbolDefinitionRef<'a>,
    >;

    fn into_iter(self) -> Self::IntoIter {
        self.0
            .iter()
            .map(|(name, symbol)| CallableSymbolDefinitionRef { name, symbol })
    }
}

impl<'a> IntoIterator for &'a mut CallableSymbolDefinitions {
    type Item = CallableSymbolDefinitionMut<'a>;

    type IntoIter = std::iter::Map<
        IterMut<'a, String, CallableSymbol>,
        fn((&'a String, &'a mut CallableSymbol)) -> CallableSymbolDefinitionMut<'a>,
    >;

    fn into_iter(self) -> Self::IntoIter {
        self.0
            .iter_mut()
            .map(|(name, symbol)| CallableSymbolDefinitionMut { name, symbol })
    }
}

impl FromIterator<CallableSymbolDefinition> for CallableSymbolDefinitions {
    fn from_iter<I: IntoIterator<Item = CallableSymbolDefinition>>(iter: I) -> Self {
        Self(iter.into_iter().map(|d| (d.name, d.symbol)).collect())
    }
}

impl CallableSymbolDefinitions {
    /// Returns whether all definitions define operations
    pub fn is_only_operations(&self) -> bool {
        self.iter()
            .all(|d| matches!(d.symbol, CallableSymbol::Operation(_)))
    }

    /// Returns whether all definitions define functions
    pub fn is_only_functions(&self) -> bool {
        self.iter()
            .all(|d| matches!(d.symbol, CallableSymbol::Function(_)))
    }

    /// Returns an iterator over references to definitions
    pub fn iter(&self) -> impl Iterator<Item = CallableSymbolDefinitionRef> {
        self.into_iter()
    }

    /// Returns an iterator over mutable references to definitions
    pub fn iter_mut(&mut self) -> impl Iterator<Item = CallableSymbolDefinitionMut> {
        self.into_iter()
    }

    /// Returns an iterator over references to operation definitions
    pub fn operation_definitions(&self) -> impl Iterator<Item = OperationDefinitionRef> {
        self.0.iter().filter_map(|(name, symbol)| {
            <&OperationSymbol>::try_from(symbol)
                .map(|operation| OperationDefinitionRef { name, operation })
                .ok()
        })
    }

    /// Returns an iterator over references to function definitions
    pub fn function_definitions(&self) -> impl Iterator<Item = FunctionDefinitionRef> {
        self.0.iter().filter_map(|(name, symbol)| {
            <&FunctionSymbol>::try_from(symbol)
                .map(|function| FunctionDefinitionRef { name, function })
                .ok()
        })
    }

    /// Returns an iterator over mutable references to operation definitions
    pub fn operation_definitions_mut(&mut self) -> impl Iterator<Item = OperationDefinitionMut> {
        self.0.iter_mut().filter_map(|(name, symbol)| {
            <&mut OperationSymbol>::try_from(symbol)
                .map(|operation| OperationDefinitionMut { name, operation })
                .ok()
        })
    }

    /// Returns an iterator over mutable references to function definitions
    pub fn function_definitions_mut(&mut self) -> impl Iterator<Item = FunctionDefinitionMut> {
        self.0.iter_mut().filter_map(|(name, symbol)| {
            <&mut FunctionSymbol>::try_from(symbol)
                .map(|function| FunctionDefinitionMut { name, function })
                .ok()
        })
    }

    /// Returns an iterator over references to operations
    pub fn operations(&self) -> impl Iterator<Item = &OperationSymbol> {
        self.0
            .iter()
            .filter_map(|(_, symbol)| symbol.try_into().ok())
    }

    /// Returns an iterator over references to functions
    pub fn functions(&self) -> impl Iterator<Item = &FunctionSymbol> {
        self.0
            .iter()
            .filter_map(|(_, symbol)| symbol.try_into().ok())
    }

    /// Returns an iterator over references to operations
    pub fn operations_mut(&mut self) -> impl Iterator<Item = &mut OperationSymbol> {
        self.0
            .iter_mut()
            .filter_map(|(_, symbol)| symbol.try_into().ok())
    }

    /// Returns an iterator over references to functions
    pub fn functions_mut(&mut self) -> impl Iterator<Item = &mut FunctionSymbol> {
        self.0
            .iter_mut()
            .filter_map(|(_, symbol)| symbol.try_into().ok())
    }

    /// insert a symbol with a given name
    pub fn insert<S: Into<CallableSymbol>>(
        &mut self,
        name: String,
        s: S,
    ) -> Option<CallableSymbol> {
        self.0.insert(name, s.into())
    }
}

pub struct OperationDefinitionRef<'a> {
    /// the name of the operation
    pub name: &'a str,
    /// a reference to the operation
    pub operation: &'a OperationSymbol,
}

pub struct OperationDefinitionMut<'a> {
    /// the name of the operation
    pub name: &'a str,
    /// a mutable reference to the operation
    pub operation: &'a mut OperationSymbol,
}

pub struct FunctionDefinitionRef<'a> {
    /// the name of the function
    pub name: &'a str,
    /// a reference to the function
    pub function: &'a FunctionSymbol,
}

pub struct FunctionDefinitionMut<'a> {
    /// the name of the function
    pub name: &'a str,
    /// a mutable reference to the function
    pub function: &'a mut FunctionSymbol,
}

#[derive(Clone, Debug)]
pub enum CallableSymbol {
    Function(FunctionSymbol),
    Operation(OperationSymbol),
}

impl From<FunctionSymbol> for CallableSymbol {
    fn from(value: FunctionSymbol) -> Self {
        Self::Function(value)
    }
}

impl From<OperationSymbol> for CallableSymbol {
    fn from(value: OperationSymbol) -> Self {
        Self::Operation(value)
    }
}

impl TryFrom<CallableSymbol> for FunctionSymbol {
    type Error = ();

    fn try_from(value: CallableSymbol) -> Result<Self, Self::Error> {
        match value {
            CallableSymbol::Function(s) => Ok(s),
            _ => Err(()),
        }
    }
}

impl TryFrom<CallableSymbol> for OperationSymbol {
    type Error = ();

    fn try_from(value: CallableSymbol) -> Result<Self, Self::Error> {
        match value {
            CallableSymbol::Operation(s) => Ok(s),
            _ => Err(()),
        }
    }
}

impl<'a> TryFrom<&'a CallableSymbol> for &'a FunctionSymbol {
    type Error = ();

    fn try_from(value: &'a CallableSymbol) -> Result<Self, Self::Error> {
        match value {
            CallableSymbol::Function(s) => Ok(s),
            _ => Err(()),
        }
    }
}

impl<'a> TryFrom<&'a CallableSymbol> for &'a OperationSymbol {
    type Error = ();

    fn try_from(value: &'a CallableSymbol) -> Result<Self, Self::Error> {
        match value {
            CallableSymbol::Operation(s) => Ok(s),
            _ => Err(()),
        }
    }
}

impl<'a> TryFrom<&'a mut CallableSymbol> for &'a mut FunctionSymbol {
    type Error = ();

    fn try_from(value: &'a mut CallableSymbol) -> Result<Self, Self::Error> {
        match value {
            CallableSymbol::Function(s) => Ok(s),
            _ => Err(()),
        }
    }
}

impl<'a> TryFrom<&'a mut CallableSymbol> for &'a mut OperationSymbol {
    type Error = ();

    fn try_from(value: &'a mut CallableSymbol) -> Result<Self, Self::Error> {
        match value {
            CallableSymbol::Operation(s) => Ok(s),
            _ => Err(()),
        }
    }
}

#[derive(Clone, Debug)]
pub struct FunctionSymbol {
    pub source: SourceRef,
    /// the parameters of this function, in the form of values
    pub params: FunctionParams,
    /// the body of the function
    pub body: FunctionBody,
}

#[derive(Clone, Debug)]
pub struct OperationSymbol {
    pub source: SourceRef,
    /// the id of this operation. This machine's operation id must be set to this value in order for this operation to be active.
    pub id: OperationId,
    /// the parameters of this operation, in the form of columns defined in some constraints block of this machine
    pub params: OperationParams,
}

#[derive(Clone, Debug)]
pub enum FunctionStatement {
    Assignment(AssignmentStatement),
    Instruction(InstructionStatement),
    Label(LabelStatement),
    DebugDirective(DebugDirective),
    Return(Return),
}

impl ExpressionVisitable<Expression<NamespacedPolynomialReference>> for FunctionStatement {
    fn visit_expressions_mut<F, B>(&mut self, f: &mut F, o: VisitOrder) -> std::ops::ControlFlow<B>
    where
        F: FnMut(&mut Expression<NamespacedPolynomialReference>) -> std::ops::ControlFlow<B>,
    {
        match self {
            FunctionStatement::Assignment(assignment) => {
                assignment.rhs.as_mut().visit_expressions_mut(f, o)
            }
            FunctionStatement::Instruction(instruction) => instruction
                .inputs
                .iter_mut()
                .try_for_each(move |i| i.visit_expressions_mut(f, o)),
            FunctionStatement::Label(_) | FunctionStatement::DebugDirective(..) => {
                ControlFlow::Continue(())
            }
            FunctionStatement::Return(ret) => ret
                .values
                .iter_mut()
                .try_for_each(move |e| e.visit_expressions_mut(f, o)),
        }
    }

    fn visit_expressions<F, B>(&self, f: &mut F, o: VisitOrder) -> std::ops::ControlFlow<B>
    where
        F: FnMut(&Expression<NamespacedPolynomialReference>) -> std::ops::ControlFlow<B>,
    {
        match self {
            FunctionStatement::Assignment(assignment) => {
                assignment.rhs.as_ref().visit_expressions(f, o)
            }
            FunctionStatement::Instruction(instruction) => instruction
                .inputs
                .iter()
                .try_for_each(move |i| i.visit_expressions(f, o)),
            FunctionStatement::Label(_) | FunctionStatement::DebugDirective(..) => {
                ControlFlow::Continue(())
            }
            FunctionStatement::Return(ret) => ret
                .values
                .iter()
                .try_for_each(move |e| e.visit_expressions(f, o)),
        }
    }
}

impl From<AssignmentStatement> for FunctionStatement {
    fn from(value: AssignmentStatement) -> Self {
        Self::Assignment(value)
    }
}

impl From<InstructionStatement> for FunctionStatement {
    fn from(value: InstructionStatement) -> Self {
        Self::Instruction(value)
    }
}

impl From<LabelStatement> for FunctionStatement {
    fn from(value: LabelStatement) -> Self {
        Self::Label(value)
    }
}

impl From<DebugDirective> for FunctionStatement {
    fn from(value: DebugDirective) -> Self {
        Self::DebugDirective(value)
    }
}

impl From<Return> for FunctionStatement {
    fn from(value: Return) -> Self {
        Self::Return(value)
    }
}

#[derive(Clone, Debug)]
pub struct AssignmentStatement {
    pub source: SourceRef,
    pub lhs_with_reg: Vec<(String, AssignmentRegister)>,
    pub rhs: Box<Expression>,
}

impl AssignmentStatement {
    fn lhs(&self) -> impl Iterator<Item = &String> {
        self.lhs_with_reg.iter().map(|(lhs, _)| lhs)
    }

    fn assignment_registers(&self) -> impl Iterator<Item = &AssignmentRegister> {
        self.lhs_with_reg.iter().map(|(_, reg)| reg)
    }
}

#[derive(Clone, Debug)]
pub struct InstructionStatement {
    pub source: SourceRef,
    pub instruction: String,
    pub inputs: Vec<Expression>,
}

#[derive(Clone, Debug)]
pub struct LabelStatement {
    pub source: SourceRef,
    pub name: String,
}

#[derive(Clone, Debug)]
pub struct DebugDirective {
    pub source: SourceRef,
    pub directive: crate::parsed::asm::DebugDirective,
}

#[derive(Clone, Debug)]
pub struct Return {
    pub source: SourceRef,
    pub values: Vec<Expression>,
}

#[derive(Clone, Debug)]
pub struct SubmachineDeclaration {
    /// the name of this instance
    pub name: String,
    /// the type of the submachine
    pub ty: AbsoluteSymbolPath,
}

/// An item that is part of the module tree after all modules,
/// imports and references have been resolved.
#[derive(Clone, Debug)]
pub enum Item {
    Machine(Machine),
    Expression(TypedExpression),
    TypeDeclaration(EnumDeclaration<Expression>),
}

impl Item {
    pub fn try_to_machine(&self) -> Option<&Machine> {
        match self {
            Item::Machine(m) => Some(m),
            Item::Expression(_) | Item::TypeDeclaration(_) => None,
        }
    }
}

#[derive(Clone, Default, Debug)]
pub struct Machine {
    /// The degree if any, i.e. the number of rows in instances of this machine type
    pub degree: Option<Expression>,
    /// The latch, i.e. the boolean column whose values must be 1 in order for this machine to be accessed. Must be defined in one of the constraint blocks of this machine.
    pub latch: Option<String>,
    /// The operation id, i.e. the column whose values determine which operation is being invoked in the current block. Must be defined in one of the constraint blocks of this machine.
    pub operation_id: Option<String>,
    /// call selector array
    pub call_selectors: Option<String>,
    /// The set of registers for this machine
    pub registers: Vec<RegisterDeclarationStatement>,
    /// The index of the program counter in the registers, if any
    pub pc: Option<usize>,
    /// The set of pil statements
    pub pil: Vec<PilStatement>,
    /// The set of instructions which can be invoked in functions
    pub instructions: Vec<InstructionDefinitionStatement>,
    /// The set of low level links to other machines
    pub links: Vec<LinkDefinitionStatement>,
    /// The set of functions and operations in the same namespace
    pub callable: CallableSymbolDefinitions,
    /// The set of submachines
    pub submachines: Vec<SubmachineDeclaration>,
}

impl Machine {
    /// Returns whether this machine type features a program counter. This is how we differentiate virtual machines from constrained machines.
    pub fn has_pc(&self) -> bool {
        self.pc.is_some()
    }

    /// Returns the name of the program counter, if any.
    pub fn pc(&self) -> Option<String> {
        self.pc.map(|index| self.registers[index].name.clone())
    }

    /// Returns an iterator over references to the names of register declarations
    pub fn write_register_names(&self) -> impl Iterator<Item = &str> {
        self.registers
            .iter()
            .filter(|r| r.ty.is_write())
            .map(|r| r.name.as_ref())
    }

    /// Returns an iterator over references to the names of the assignment registers
    pub fn assignment_register_names(&self) -> impl Iterator<Item = &str> {
        self.registers
            .iter()
            .filter(|r| r.ty.is_assignment())
            .map(|r| r.name.as_ref())
    }

    /// Returns an iterator over references to the names of the read-only registers
    pub fn read_only_register_names(&self) -> impl Iterator<Item = &str> {
        self.registers
            .iter()
            .filter(|r| r.ty.is_read_only())
            .map(|r| r.name.as_ref())
    }

    /// Returns an iterator over references to the operation definitions    
    pub fn operation_definitions(&self) -> impl Iterator<Item = OperationDefinitionRef> {
        self.callable.operation_definitions()
    }

    /// Returns an iterator over references to the function definitions
    pub fn function_definitions(&self) -> impl Iterator<Item = FunctionDefinitionRef> {
        self.callable.function_definitions()
    }

    /// Returns an iterator over mutable references to the operation definitions
    pub fn operation_definitions_mut(&mut self) -> impl Iterator<Item = OperationDefinitionMut> {
        self.callable.operation_definitions_mut()
    }

    /// Returns an iterator over mutable references to the function definitions
    pub fn function_definitions_mut(&mut self) -> impl Iterator<Item = FunctionDefinitionMut> {
        self.callable.function_definitions_mut()
    }

    /// Returns an iterator over references to the operations    
    pub fn operations(&self) -> impl Iterator<Item = &OperationSymbol> {
        self.callable.operations()
    }

    /// Returns an iterator over references to the functions
    pub fn functions(&self) -> impl Iterator<Item = &FunctionSymbol> {
        self.callable.functions()
    }

    /// Returns an iterator over mutable references to the operations    
    pub fn operations_mut(&mut self) -> impl Iterator<Item = &mut OperationSymbol> {
        self.callable.operations_mut()
    }

    /// Returns an iterator over mutable references to the functions
    pub fn functions_mut(&mut self) -> impl Iterator<Item = &mut FunctionSymbol> {
        self.callable.functions_mut()
    }
}

#[derive(Clone, Default, Debug)]
pub struct Rom {
    pub statements: FunctionStatements,
}

#[derive(Default, Clone, Debug)]
pub struct AnalysisASMFile {
    pub items: BTreeMap<AbsoluteSymbolPath, Item>,
}

impl AnalysisASMFile {
    pub fn machines(&self) -> impl Iterator<Item = (&AbsoluteSymbolPath, &Machine)> {
        self.items.iter().filter_map(|(n, m)| match m {
            Item::Machine(m) => Some((n, m)),
            Item::Expression(_) | Item::TypeDeclaration(_) => None,
        })
    }
    pub fn machines_mut(&mut self) -> impl Iterator<Item = (&AbsoluteSymbolPath, &mut Machine)> {
        self.items.iter_mut().filter_map(|(n, m)| match m {
            Item::Machine(m) => Some((n, m)),
            Item::Expression(_) | Item::TypeDeclaration(_) => None,
        })
    }
}

#[derive(Default, Debug, Clone)]
pub struct BatchMetadata {
    // the set of compatible statements
    pub size: usize,
    // the reason why this batch ended (for debugging purposes), None if we ran out of statements to batch
    pub reason: Option<IncompatibleSet>,
}

impl BatchMetadata {
    pub fn size(&self) -> usize {
        self.size
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum Incompatible {
    Label,
    Unimplemented,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Default, Clone)]
pub struct IncompatibleSet(pub BTreeSet<Incompatible>);

impl From<Incompatible> for IncompatibleSet {
    fn from(value: Incompatible) -> Self {
        Self(once(value).collect())
    }
}
