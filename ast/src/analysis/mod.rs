mod display;

use std::collections::BTreeSet;

use num_bigint::BigUint;

use crate::parsed::{
    asm::{DebugDirective, InstructionBodyElement, InstructionParams, RegisterFlag},
    Expression, Statement,
};

pub struct RegisterDeclarationStatement {
    pub start: usize,
    pub name: String,
    pub flag: Option<RegisterFlag>,
}

pub struct InstructionDefinitionStatement<T> {
    pub start: usize,
    pub name: String,
    pub params: InstructionParams,
    pub body: Vec<InstructionBodyElement<T>>,
}

pub struct DegreeStatement {
    pub degree: BigUint,
}

pub enum ProgramStatement<T> {
    Assignment(AssignmentStatement<T>),
    Instruction(InstructionStatement<T>),
    Label(LabelStatement),
    DebugDirective(DebugDirective),
}

impl<T> From<AssignmentStatement<T>> for ProgramStatement<T> {
    fn from(value: AssignmentStatement<T>) -> Self {
        Self::Assignment(value)
    }
}

impl<T> From<InstructionStatement<T>> for ProgramStatement<T> {
    fn from(value: InstructionStatement<T>) -> Self {
        Self::Instruction(value)
    }
}

impl<T> From<LabelStatement> for ProgramStatement<T> {
    fn from(value: LabelStatement) -> Self {
        Self::Label(value)
    }
}

impl<T> From<DebugDirective> for ProgramStatement<T> {
    fn from(value: DebugDirective) -> Self {
        Self::DebugDirective(value)
    }
}

pub struct AssignmentStatement<T> {
    pub start: usize,
    pub lhs: Vec<String>,
    pub using_reg: Option<String>,
    pub rhs: Box<Expression<T>>,
}

pub struct InstructionStatement<T> {
    pub start: usize,
    pub instruction: String,
    pub inputs: Vec<Expression<T>>,
}

pub struct LabelStatement {
    pub start: usize,
    pub name: String,
}

pub struct PilBlock<T> {
    pub start: usize,
    pub statements: Vec<Statement<T>>,
}

pub struct AnalysisASMFile<T> {
    pub degree: Option<DegreeStatement>,
    pub registers: Vec<RegisterDeclarationStatement>,
    pub pil: Vec<PilBlock<T>>,
    pub instructions: Vec<InstructionDefinitionStatement<T>>,
    pub program: Vec<ProgramStatement<T>>,
    pub batches: Option<Vec<BatchMetadata>>,
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
