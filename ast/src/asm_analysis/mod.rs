mod display;

use std::collections::{BTreeSet, HashMap};

use num_bigint::BigUint;

use crate::parsed::{
    asm::{DebugDirective, InstructionBody, InstructionParams, RegisterFlag},
    Expression, PilStatement,
};

#[derive(Clone)]
pub struct RegisterDeclarationStatement {
    pub start: usize,
    pub name: String,
    pub flag: Option<RegisterFlag>,
}

#[derive(Clone)]
pub struct InstructionDefinitionStatement<T> {
    pub start: usize,
    pub name: String,
    pub params: InstructionParams,
    pub body: InstructionBody<T>,
}

#[derive(Clone)]
pub struct DegreeStatement {
    pub degree: BigUint,
}

#[derive(Clone)]
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

#[derive(Clone)]
pub struct AssignmentStatement<T> {
    pub start: usize,
    pub lhs: Vec<String>,
    pub using_reg: Option<String>,
    pub rhs: Box<Expression<T>>,
}

#[derive(Clone)]
pub struct InstructionStatement<T> {
    pub start: usize,
    pub instruction: String,
    pub inputs: Vec<Expression<T>>,
}

#[derive(Clone)]
pub struct LabelStatement {
    pub start: usize,
    pub name: String,
}

#[derive(Clone)]
pub struct PilBlock<T> {
    pub start: usize,
    pub statements: Vec<PilStatement<T>>,
}

#[derive(Clone, Default)]
pub struct Machine<T> {
    pub degree: Option<DegreeStatement>,
    pub submachines: Vec<(String, String)>,
    pub registers: Vec<RegisterDeclarationStatement>,
    pub constraints: Vec<PilBlock<T>>,
    pub instructions: Vec<InstructionDefinitionStatement<T>>,
    pub program: Program<T>,
}

#[derive(Clone, Default)]
pub struct Program<T> {
    pub statements: Vec<ProgramStatement<T>>,
    pub batches: Option<Vec<BatchMetadata>>,
}

pub struct AnalysisASMFile<T> {
    pub machines: HashMap<String, Machine<T>>,
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
