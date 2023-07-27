mod display;

use std::collections::{BTreeMap, BTreeSet};

use num_bigint::BigUint;

use crate::parsed::{
    asm::{InstructionBody, Params, RegisterFlag},
    PilStatement,
};

pub use crate::parsed::Expression;

#[derive(Clone, Debug)]
pub struct RegisterDeclarationStatement {
    pub start: usize,
    pub name: String,
    pub flag: Option<RegisterFlag>,
}

#[derive(Clone, Debug)]
pub struct InstructionDefinitionStatement<T> {
    pub start: usize,
    pub name: String,
    pub params: Params,
    pub body: InstructionBody<T>,
}

#[derive(Clone, Debug)]
pub struct FunctionBody<T> {
    pub statements: Vec<FunctionStatement<T>>,
}

#[derive(Clone, Debug)]
pub struct FunctionDefinitionStatement<T> {
    pub start: usize,
    pub name: String,
    pub params: Params,
    pub body: FunctionBody<T>,
}

#[derive(Clone, Debug)]
pub struct DegreeStatement {
    pub degree: BigUint,
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub struct AssignmentStatement<T> {
    pub start: usize,
    pub lhs: Vec<String>,
    pub using_reg: Option<String>,
    pub rhs: Box<Expression<T>>,
}

#[derive(Clone, Debug)]
pub struct InstructionStatement<T> {
    pub start: usize,
    pub instruction: String,
    pub inputs: Vec<Expression<T>>,
}

#[derive(Clone, Debug)]
pub struct LabelStatement {
    pub start: usize,
    pub name: String,
}

#[derive(Clone, Debug)]
pub struct DebugDirective {
    pub start: usize,
    pub directive: crate::parsed::asm::DebugDirective,
}

#[derive(Clone, Debug)]
pub struct PilBlock<T> {
    pub start: usize,
    pub statements: Vec<PilStatement<T>>,
}

#[derive(Clone, Default, Debug)]
pub struct Machine<T> {
    pub degree: Option<DegreeStatement>,
    pub registers: Vec<RegisterDeclarationStatement>,
    // index of the pc in the registers, if any
    pub pc: Option<usize>,
    pub constraints: Vec<PilBlock<T>>,
    pub instructions: Vec<InstructionDefinitionStatement<T>>,
    pub functions: Vec<FunctionDefinitionStatement<T>>,
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
}

#[derive(Clone, Default, Debug)]
pub struct Rom<T> {
    pub statements: Vec<FunctionStatement<T>>,
    pub batches: Option<Vec<BatchMetadata>>,
}

#[derive(Default, Debug)]
pub struct AnalysisASMFile<T> {
    pub machines: BTreeMap<String, Machine<T>>,
}

#[derive(Default, Debug, PartialEq, Eq, Clone)]
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

#[derive(Debug, PartialEq, Eq, Default, Clone)]
pub struct IncompatibleSet(pub BTreeSet<Incompatible>);
