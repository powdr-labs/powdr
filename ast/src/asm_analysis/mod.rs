mod display;
pub mod utils;

use std::collections::{BTreeSet, HashMap};

use num_bigint::BigUint;

use crate::parsed::{
    asm::{DebugDirective, InstructionBody, Params, RegisterFlag},
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
    pub params: Params,
    pub body: InstructionBody<T>,
}

#[derive(Clone)]
pub struct OperationDefinitionStatement<T> {
    pub start: usize,
    pub name: String,
    pub params: Params,
    pub body: Vec<OperationStatement<T>>,
}

#[derive(Clone)]
pub struct DegreeStatement {
    pub degree: BigUint,
}

#[derive(Clone)]
pub enum OperationStatement<T> {
    Assignment(AssignmentStatement<T>),
    Instruction(InstructionStatement<T>),
    Label(LabelStatement),
    DebugDirective(DebugDirective),
}

impl<T> From<AssignmentStatement<T>> for OperationStatement<T> {
    fn from(value: AssignmentStatement<T>) -> Self {
        Self::Assignment(value)
    }
}

impl<T> From<InstructionStatement<T>> for OperationStatement<T> {
    fn from(value: InstructionStatement<T>) -> Self {
        Self::Instruction(value)
    }
}

impl<T> From<LabelStatement> for OperationStatement<T> {
    fn from(value: LabelStatement) -> Self {
        Self::Label(value)
    }
}

impl<T> From<DebugDirective> for OperationStatement<T> {
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
    // index of the pc in the registers, if any
    pub pc: Option<usize>,
    pub constraints: Vec<PilBlock<T>>,
    pub instructions: Vec<InstructionDefinitionStatement<T>>,
    pub operations: Vec<OperationDefinitionStatement<T>>,
    /// the program gets generated in romgen
    pub program: Option<Program<T>>,
}

impl<T> Machine<T> {
    pub fn has_pc(&self) -> bool {
        self.pc.is_some()
    }

    pub fn pc(&self) -> Option<String> {
        self.pc.map(|index| self.registers[index].name.clone())
    }

    pub fn write_registers(&self) -> impl Iterator<Item = &RegisterDeclarationStatement> {
        self.registers.iter().filter(|r| r.flag.is_none())
    }

    pub fn assignment_registers(&self) -> impl Iterator<Item = &RegisterDeclarationStatement> {
        self.registers
            .iter()
            .filter(|r| r.flag == Some(RegisterFlag::IsAssignment))
    }
}

#[derive(Clone, Default)]
pub struct Program<T> {
    pub statements: Vec<OperationStatement<T>>,
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
