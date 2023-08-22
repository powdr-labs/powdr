mod display;
pub mod utils;

use std::{
    collections::{
        btree_map::{IntoIter, Iter, IterMut},
        BTreeMap, BTreeSet,
    },
    iter::{once, repeat},
};

use itertools::Either;
use num_bigint::BigUint;
use number::FieldElement;

use crate::parsed::{
    asm::{CallableRef, InstructionBody, OperationId, Params},
    PilStatement,
};

pub use crate::parsed::Expression;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct RegisterDeclarationStatement {
    pub start: usize,
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

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct InstructionDefinitionStatement<T> {
    pub start: usize,
    pub name: String,
    pub instruction: Instruction<T>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Instruction<T> {
    pub params: Params,
    pub body: InstructionBody<T>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct LinkDefinitionStatement<T> {
    pub start: usize,
    /// the flag which activates this link. Should be boolean.
    pub flag: Expression<T>,
    /// the parameters to pass to the callable
    pub params: Params,
    /// the callable to invoke when the flag is on
    pub to: CallableRef,
}

#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionStatements<T> {
    inner: Vec<FunctionStatement<T>>,
    batches: Option<Vec<BatchMetadata>>,
}

pub struct BatchRef<'a, T> {
    statements: &'a [FunctionStatement<T>],
    reason: &'a Option<IncompatibleSet>,
}

pub struct Batch<T> {
    pub statements: Vec<FunctionStatement<T>>,
    reason: Option<IncompatibleSet>,
}

impl<T> From<Vec<FunctionStatement<T>>> for Batch<T> {
    fn from(statements: Vec<FunctionStatement<T>>) -> Self {
        Self {
            statements,
            reason: None,
        }
    }
}

impl<T> Batch<T> {
    pub fn set_reason(&mut self, reason: IncompatibleSet) {
        self.reason = Some(reason);
    }

    pub fn reason(mut self, reason: IncompatibleSet) -> Self {
        self.reason = Some(reason);
        self
    }
}

impl<T> FunctionStatements<T> {
    /// create with no batch information
    pub fn new(inner: Vec<FunctionStatement<T>>) -> Self {
        Self {
            inner,
            batches: None,
        }
    }

    /// turn into the underlying statements, forgetting batch information
    pub fn into_inner(self) -> Vec<FunctionStatement<T>> {
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
    pub fn iter(&self) -> impl Iterator<Item = &FunctionStatement<T>> {
        self.inner.iter()
    }

    /// iterate over the statements by mutable reference
    /// Warning: mutation should be checked not to invalidate batch information
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut FunctionStatement<T>> {
        self.inner.iter_mut()
    }

    /// iterate over the batches by reference
    fn iter_batches(&self) -> impl Iterator<Item = BatchRef<T>> {
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

impl<T: FieldElement> FunctionStatements<T> {
    /// iterate over the batches by reference
    pub fn into_iter_batches(self) -> impl Iterator<Item = Batch<T>> {
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

impl<T> FromIterator<Batch<T>> for FunctionStatements<T> {
    fn from_iter<I: IntoIterator<Item = Batch<T>>>(iter: I) -> Self {
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

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionBody<T> {
    pub statements: FunctionStatements<T>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct CallableSymbolDefinitionRef<'a, T> {
    /// the name of this symbol
    pub name: &'a str,
    /// a reference to the symbol
    pub symbol: &'a CallableSymbol<T>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct CallableSymbolDefinitionMut<'a, T> {
    /// the name of this symbol
    pub name: &'a str,
    /// a mutable reference to the symbol
    pub symbol: &'a mut CallableSymbol<T>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct CallableSymbolDefinition<T> {
    /// the name of this symbol
    pub name: String,
    /// the symbol
    pub symbol: CallableSymbol<T>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct CallableSymbolDefinitions<T>(BTreeMap<String, CallableSymbol<T>>);

impl<T> IntoIterator for CallableSymbolDefinitions<T> {
    type Item = CallableSymbolDefinition<T>;

    type IntoIter = std::iter::Map<
        IntoIter<String, CallableSymbol<T>>,
        fn((String, CallableSymbol<T>)) -> CallableSymbolDefinition<T>,
    >;

    fn into_iter(self) -> Self::IntoIter {
        self.0
            .into_iter()
            .map(|(name, symbol)| CallableSymbolDefinition { name, symbol })
    }
}

impl<'a, T> IntoIterator for &'a CallableSymbolDefinitions<T> {
    type Item = CallableSymbolDefinitionRef<'a, T>;

    type IntoIter = std::iter::Map<
        Iter<'a, String, CallableSymbol<T>>,
        fn((&'a String, &'a CallableSymbol<T>)) -> CallableSymbolDefinitionRef<'a, T>,
    >;

    fn into_iter(self) -> Self::IntoIter {
        self.0
            .iter()
            .map(|(name, symbol)| CallableSymbolDefinitionRef { name, symbol })
    }
}

impl<'a, T> IntoIterator for &'a mut CallableSymbolDefinitions<T> {
    type Item = CallableSymbolDefinitionMut<'a, T>;

    type IntoIter = std::iter::Map<
        IterMut<'a, String, CallableSymbol<T>>,
        fn((&'a String, &'a mut CallableSymbol<T>)) -> CallableSymbolDefinitionMut<'a, T>,
    >;

    fn into_iter(self) -> Self::IntoIter {
        self.0
            .iter_mut()
            .map(|(name, symbol)| CallableSymbolDefinitionMut { name, symbol })
    }
}

impl<T> FromIterator<CallableSymbolDefinition<T>> for CallableSymbolDefinitions<T> {
    fn from_iter<I: IntoIterator<Item = CallableSymbolDefinition<T>>>(iter: I) -> Self {
        Self(iter.into_iter().map(|d| (d.name, d.symbol)).collect())
    }
}

impl<T> CallableSymbolDefinitions<T> {
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
    pub fn iter(&self) -> impl Iterator<Item = CallableSymbolDefinitionRef<T>> {
        self.into_iter()
    }

    /// Returns an iterator over mutable references to definitions
    pub fn iter_mut(&mut self) -> impl Iterator<Item = CallableSymbolDefinitionMut<T>> {
        self.into_iter()
    }

    /// Returns an iterator over references to operation definitions
    pub fn operation_definitions(&self) -> impl Iterator<Item = OperationDefinitionRef<T>> {
        self.0.iter().filter_map(|(name, symbol)| {
            <&OperationSymbol<_>>::try_from(symbol)
                .map(|operation| OperationDefinitionRef { name, operation })
                .ok()
        })
    }

    /// Returns an iterator over references to function definitions
    pub fn function_definitions(&self) -> impl Iterator<Item = FunctionDefinitionRef<T>> {
        self.0.iter().filter_map(|(name, symbol)| {
            <&FunctionSymbol<_>>::try_from(symbol)
                .map(|function| FunctionDefinitionRef { name, function })
                .ok()
        })
    }

    /// Returns an iterator over mutable references to operation definitions
    pub fn operation_definitions_mut(&mut self) -> impl Iterator<Item = OperationDefinitionMut<T>> {
        self.0.iter_mut().filter_map(|(name, symbol)| {
            <&mut OperationSymbol<_>>::try_from(symbol)
                .map(|operation| OperationDefinitionMut { name, operation })
                .ok()
        })
    }

    /// Returns an iterator over mutable references to function definitions
    pub fn function_definitions_mut(&mut self) -> impl Iterator<Item = FunctionDefinitionMut<T>> {
        self.0.iter_mut().filter_map(|(name, symbol)| {
            <&mut FunctionSymbol<_>>::try_from(symbol)
                .map(|function| FunctionDefinitionMut { name, function })
                .ok()
        })
    }

    /// Returns an iterator over references to operations
    pub fn operations(&self) -> impl Iterator<Item = &OperationSymbol<T>> {
        self.0
            .iter()
            .filter_map(|(_, symbol)| symbol.try_into().ok())
    }

    /// Returns an iterator over references to functions
    pub fn functions(&self) -> impl Iterator<Item = &FunctionSymbol<T>> {
        self.0
            .iter()
            .filter_map(|(_, symbol)| symbol.try_into().ok())
    }

    /// Returns an iterator over references to operations
    pub fn operations_mut(&mut self) -> impl Iterator<Item = &mut OperationSymbol<T>> {
        self.0
            .iter_mut()
            .filter_map(|(_, symbol)| symbol.try_into().ok())
    }

    /// Returns an iterator over references to functions
    pub fn functions_mut(&mut self) -> impl Iterator<Item = &mut FunctionSymbol<T>> {
        self.0
            .iter_mut()
            .filter_map(|(_, symbol)| symbol.try_into().ok())
    }

    /// insert a symbol with a given name
    pub fn insert<S: Into<CallableSymbol<T>>>(
        &mut self,
        name: String,
        s: S,
    ) -> Option<CallableSymbol<T>> {
        self.0.insert(name, s.into())
    }
}

pub struct OperationDefinitionRef<'a, T> {
    /// the name of the operation
    pub name: &'a str,
    /// a reference to the operation
    pub operation: &'a OperationSymbol<T>,
}

pub struct OperationDefinitionMut<'a, T> {
    /// the name of the operation
    pub name: &'a str,
    /// a mutable reference to the operation
    pub operation: &'a mut OperationSymbol<T>,
}

pub struct FunctionDefinitionRef<'a, T> {
    /// the name of the function
    pub name: &'a str,
    /// a reference to the function
    pub function: &'a FunctionSymbol<T>,
}

pub struct FunctionDefinitionMut<'a, T> {
    /// the name of the function
    pub name: &'a str,
    /// a mutable reference to the function
    pub function: &'a mut FunctionSymbol<T>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum CallableSymbol<T> {
    Function(FunctionSymbol<T>),
    Operation(OperationSymbol<T>),
}

impl<T> From<FunctionSymbol<T>> for CallableSymbol<T> {
    fn from(value: FunctionSymbol<T>) -> Self {
        Self::Function(value)
    }
}

impl<T> From<OperationSymbol<T>> for CallableSymbol<T> {
    fn from(value: OperationSymbol<T>) -> Self {
        Self::Operation(value)
    }
}

impl<T> TryFrom<CallableSymbol<T>> for FunctionSymbol<T> {
    type Error = ();

    fn try_from(value: CallableSymbol<T>) -> Result<Self, Self::Error> {
        match value {
            CallableSymbol::Function(s) => Ok(s),
            _ => Err(()),
        }
    }
}

impl<T> TryFrom<CallableSymbol<T>> for OperationSymbol<T> {
    type Error = ();

    fn try_from(value: CallableSymbol<T>) -> Result<Self, Self::Error> {
        match value {
            CallableSymbol::Operation(s) => Ok(s),
            _ => Err(()),
        }
    }
}

impl<'a, T> TryFrom<&'a CallableSymbol<T>> for &'a FunctionSymbol<T> {
    type Error = ();

    fn try_from(value: &'a CallableSymbol<T>) -> Result<Self, Self::Error> {
        match value {
            CallableSymbol::Function(s) => Ok(s),
            _ => Err(()),
        }
    }
}

impl<'a, T> TryFrom<&'a CallableSymbol<T>> for &'a OperationSymbol<T> {
    type Error = ();

    fn try_from(value: &'a CallableSymbol<T>) -> Result<Self, Self::Error> {
        match value {
            CallableSymbol::Operation(s) => Ok(s),
            _ => Err(()),
        }
    }
}

impl<'a, T> TryFrom<&'a mut CallableSymbol<T>> for &'a mut FunctionSymbol<T> {
    type Error = ();

    fn try_from(value: &'a mut CallableSymbol<T>) -> Result<Self, Self::Error> {
        match value {
            CallableSymbol::Function(s) => Ok(s),
            _ => Err(()),
        }
    }
}

impl<'a, T> TryFrom<&'a mut CallableSymbol<T>> for &'a mut OperationSymbol<T> {
    type Error = ();

    fn try_from(value: &'a mut CallableSymbol<T>) -> Result<Self, Self::Error> {
        match value {
            CallableSymbol::Operation(s) => Ok(s),
            _ => Err(()),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionSymbol<T> {
    pub start: usize,
    /// the parameters of this function, in the form of values
    pub params: Params,
    /// the body of the function
    pub body: FunctionBody<T>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct OperationSymbol<T> {
    pub start: usize,
    /// the id of this operation. This machine's operation id must be set to this value in order for this operation to be active.
    pub id: OperationId<T>,
    /// the parameters of this operation, in the form of columns defined in some constraints block of this machine
    pub params: Params,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct DegreeStatement {
    pub degree: BigUint,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum FunctionStatement<T> {
    Assignment(AssignmentStatement<T>),
    Instruction(InstructionStatement<T>),
    Label(LabelStatement),
    DebugDirective(DebugDirective),
    Return(Return<T>),
}

impl<T> From<AssignmentStatement<T>> for FunctionStatement<T> {
    fn from(value: AssignmentStatement<T>) -> Self {
        Self::Assignment(value)
    }
}

impl<T> From<InstructionStatement<T>> for FunctionStatement<T> {
    fn from(value: InstructionStatement<T>) -> Self {
        Self::Instruction(value)
    }
}

impl<T> From<LabelStatement> for FunctionStatement<T> {
    fn from(value: LabelStatement) -> Self {
        Self::Label(value)
    }
}

impl<T> From<DebugDirective> for FunctionStatement<T> {
    fn from(value: DebugDirective) -> Self {
        Self::DebugDirective(value)
    }
}

impl<T> From<Return<T>> for FunctionStatement<T> {
    fn from(value: Return<T>) -> Self {
        Self::Return(value)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct AssignmentStatement<T> {
    pub start: usize,
    pub lhs: Vec<String>,
    pub using_reg: Option<String>,
    pub rhs: Box<Expression<T>>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct InstructionStatement<T> {
    pub start: usize,
    pub instruction: String,
    pub inputs: Vec<Expression<T>>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct LabelStatement {
    pub start: usize,
    pub name: String,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct DebugDirective {
    pub start: usize,
    pub directive: crate::parsed::asm::DebugDirective,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Return<T> {
    pub start: usize,
    pub values: Vec<Expression<T>>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct PilBlock<T> {
    pub start: usize,
    pub statements: Vec<PilStatement<T>>,
}

#[derive(Clone, Default, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct SubmachineDeclaration {
    /// the name of this instance
    pub name: String,
    /// the type of the submachine
    pub ty: String,
}

#[derive(Clone, Default, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Machine<T> {
    /// The degree if any, i.e. the number of rows in instances of this machine type
    pub degree: Option<DegreeStatement>,
    /// The latch, i.e. the boolean column whose values must be 1 in order for this machine to be accessed. Must be defined in one of the constraint blocks of this machine.
    pub latch: Option<String>,
    /// The operation id, i.e. the column whose values determine which operation is being invoked in the current block. Must be defined in one of the constraint blocks of this machine.
    pub operation_id: Option<String>,
    /// The set of registers for this machine
    pub registers: Vec<RegisterDeclarationStatement>,
    /// The index of the program counter in the registers, if any
    pub pc: Option<usize>,
    /// The set of contraint blocks
    pub constraints: Vec<PilBlock<T>>,
    /// The set of instructions which can be invoked in functions
    pub instructions: Vec<InstructionDefinitionStatement<T>>,
    /// The set of low level links to other machines
    pub links: Vec<LinkDefinitionStatement<T>>,
    /// The set of functions and operations in the same namespace
    pub callable: CallableSymbolDefinitions<T>,
    /// The set of submachines
    pub submachines: Vec<SubmachineDeclaration>,
}

impl<T> Machine<T> {
    /// Returns whether this machine type features a program counter. This is how we differenciate virtual machines from constrained machines.
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
            .filter_map(|r: &RegisterDeclarationStatement| r.ty.is_write().then(|| r.name.as_ref()))
    }

    /// Returns an iterator over references to the names of the assignment registers
    pub fn assignment_register_names(&self) -> impl Iterator<Item = &str> {
        self.registers
            .iter()
            .filter_map(|r: &RegisterDeclarationStatement| {
                r.ty.is_assignment().then(|| r.name.as_ref())
            })
    }

    /// Returns an iterator over references to the names of the read-only registers
    pub fn read_only_register_names(&self) -> impl Iterator<Item = &str> {
        self.registers
            .iter()
            .filter_map(|r: &RegisterDeclarationStatement| {
                r.ty.is_read_only().then(|| r.name.as_ref())
            })
    }

    /// Returns an iterator over references to the operation definitions    
    pub fn operation_definitions(&self) -> impl Iterator<Item = OperationDefinitionRef<T>> {
        self.callable.operation_definitions()
    }

    /// Returns an iterator over references to the function definitions
    pub fn function_definitions(&self) -> impl Iterator<Item = FunctionDefinitionRef<T>> {
        self.callable.function_definitions()
    }

    /// Returns an iterator over mutable references to the operation definitions
    pub fn operation_definitions_mut(&mut self) -> impl Iterator<Item = OperationDefinitionMut<T>> {
        self.callable.operation_definitions_mut()
    }

    /// Returns an iterator over mutable references to the function definitions
    pub fn function_definitions_mut(&mut self) -> impl Iterator<Item = FunctionDefinitionMut<T>> {
        self.callable.function_definitions_mut()
    }

    /// Returns an iterator over references to the operations    
    pub fn operations(&self) -> impl Iterator<Item = &OperationSymbol<T>> {
        self.callable.operations()
    }

    /// Returns an iterator over references to the functions
    pub fn functions(&self) -> impl Iterator<Item = &FunctionSymbol<T>> {
        self.callable.functions()
    }

    /// Returns an iterator over mutable references to the operations    
    pub fn operations_mut(&mut self) -> impl Iterator<Item = &mut OperationSymbol<T>> {
        self.callable.operations_mut()
    }

    /// Returns an iterator over mutable references to the functions
    pub fn functions_mut(&mut self) -> impl Iterator<Item = &mut FunctionSymbol<T>> {
        self.callable.functions_mut()
    }
}

#[derive(Clone, Default, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Rom<T> {
    pub statements: FunctionStatements<T>,
}

#[derive(Default, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct AnalysisASMFile<T> {
    pub machines: BTreeMap<String, Machine<T>>,
}

#[derive(Default, Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
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
