mod display;
pub mod utils;

use std::{
    collections::{BTreeMap, BTreeSet},
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
    pub params: Params,
    pub body: InstructionBody<T>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct LinkDefinitionStatement {
    pub start: usize,
    pub flag: String,
    pub params: Params,
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
pub struct CallableSymbolDeclarationRef<'a, T> {
    pub name: &'a str,
    pub symbol: &'a CallableSymbol<T>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct CallableSymbolDeclarations<T>(pub BTreeMap<String, CallableSymbol<T>>);

impl<T> CallableSymbolDeclarations<T> {
    pub fn iter(&self) -> impl Iterator<Item = CallableSymbolDeclarationRef<T>> {
        self.0
            .iter()
            .map(|(name, symbol)| CallableSymbolDeclarationRef { name, symbol })
    }

    pub fn operation_definitions(&self) -> impl Iterator<Item = OperationDefinition<T>> {
        self.0.iter().filter_map(|(name, symbol)| {
            <&OperationSymbol<_>>::try_from(symbol)
                .map(|ref operation| OperationDefinition { name, operation })
                .ok()
        })
    }

    pub fn function_definitions(&self) -> impl Iterator<Item = FunctionDefinition<T>> {
        self.0.iter().filter_map(|(name, symbol)| {
            <&FunctionSymbol<_>>::try_from(symbol)
                .map(|ref function| FunctionDefinition { name, function })
                .ok()
        })
    }

    pub fn operation_definitions_mut(&mut self) -> impl Iterator<Item = OperationDefinitionMut<T>> {
        self.0.iter_mut().filter_map(|(name, symbol)| {
            <&mut OperationSymbol<_>>::try_from(symbol)
                .map(|operation| OperationDefinitionMut { name, operation })
                .ok()
        })
    }

    pub fn function_definitions_mut(&mut self) -> impl Iterator<Item = FunctionDefinitionMut<T>> {
        self.0.iter_mut().filter_map(|(name, symbol)| {
            <&mut FunctionSymbol<_>>::try_from(symbol)
                .map(|function| FunctionDefinitionMut { name, function })
                .ok()
        })
    }

    pub fn operations(&self) -> impl Iterator<Item = &OperationSymbol<T>> {
        self.0
            .iter()
            .filter_map(|(_, symbol)| symbol.try_into().ok())
    }

    pub fn functions(&self) -> impl Iterator<Item = &FunctionSymbol<T>> {
        self.0
            .iter()
            .filter_map(|(_, symbol)| symbol.try_into().ok())
    }

    pub fn operations_mut(&mut self) -> impl Iterator<Item = &mut OperationSymbol<T>> {
        self.0
            .iter_mut()
            .filter_map(|(_, symbol)| symbol.try_into().ok())
    }

    pub fn functions_mut(&mut self) -> impl Iterator<Item = &mut FunctionSymbol<T>> {
        self.0
            .iter_mut()
            .filter_map(|(_, symbol)| symbol.try_into().ok())
    }
}

pub struct OperationDefinition<'a, T> {
    pub name: &'a str,
    pub operation: &'a OperationSymbol<T>,
}

pub struct OperationDefinitionMut<'a, T> {
    pub name: &'a str,
    pub operation: &'a mut OperationSymbol<T>,
}

pub struct FunctionDefinition<'a, T> {
    pub name: &'a str,
    pub function: &'a FunctionSymbol<T>,
}

pub struct FunctionDefinitionMut<'a, T> {
    pub name: &'a str,
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
    pub params: Params,
    pub body: FunctionBody<T>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct OperationSymbol<T> {
    pub start: usize,
    pub id: OperationId<T>,
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
    pub degree: Option<DegreeStatement>,
    pub latch: Option<String>,
    pub operation_id: Option<String>,
    pub registers: Vec<RegisterDeclarationStatement>,
    // index of the pc in the registers, if any
    pub pc: Option<usize>,
    pub constraints: Vec<PilBlock<T>>,
    pub instructions: Vec<InstructionDefinitionStatement<T>>,
    pub links: Vec<LinkDefinitionStatement>,
    pub callable: CallableSymbolDeclarations<T>,
    pub submachines: Vec<SubmachineDeclaration>,
    /// the rom gets generated in romgen
    pub rom: Option<Rom<T>>,
}

impl<T> Machine<T> {
    pub fn has_pc(&self) -> bool {
        self.pc.is_some()
    }

    pub fn pc(&self) -> Option<String> {
        self.pc.map(|index| self.registers[index].name.clone())
    }

    pub fn write_registers(&self) -> impl Iterator<Item = &RegisterDeclarationStatement> {
        self.registers.iter().filter(|r| r.ty.is_write())
    }

    pub fn assignment_registers(&self) -> impl Iterator<Item = &RegisterDeclarationStatement> {
        self.registers.iter().filter(|r| r.ty.is_assignment())
    }

    pub fn read_only_registers(&self) -> impl Iterator<Item = &RegisterDeclarationStatement> {
        self.registers.iter().filter(|r| r.ty.is_read_only())
    }

    pub fn operation_definitions(&self) -> impl Iterator<Item = OperationDefinition<T>> {
        self.callable.operation_definitions()
    }

    pub fn function_definitions(&self) -> impl Iterator<Item = FunctionDefinition<T>> {
        self.callable.function_definitions()
    }

    pub fn operation_definitions_mut(&mut self) -> impl Iterator<Item = OperationDefinitionMut<T>> {
        self.callable.operation_definitions_mut()
    }

    pub fn function_definitions_mut(&mut self) -> impl Iterator<Item = FunctionDefinitionMut<T>> {
        self.callable.function_definitions_mut()
    }

    pub fn operations(&self) -> impl Iterator<Item = &OperationSymbol<T>> {
        self.callable.operations()
    }

    pub fn functions(&self) -> impl Iterator<Item = &FunctionSymbol<T>> {
        self.callable.functions()
    }

    pub fn operations_mut(&mut self) -> impl Iterator<Item = &mut OperationSymbol<T>> {
        self.callable.operations_mut()
    }

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
