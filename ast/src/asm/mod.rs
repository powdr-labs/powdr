mod display;

use std::collections::BTreeSet;

use num_bigint::BigUint;

use crate::parsed::{
    asm::{ASMFile, ASMStatement, InstructionBodyElement, InstructionParams, RegisterFlag},
    Statement,
};

impl<T> From<ASMFile<T>> for AnalysisASMFile<T> {
    fn from(file: ASMFile<T>) -> Self {
        let mut degree = None;
        let mut registers = vec![];
        let mut pil = vec![];
        let mut instructions = vec![];
        let mut statements = vec![];

        for s in file.0 {
            match s {
                ASMStatement::Degree(_, degree_value) => {
                    degree = Some(DegreeStatement {
                        degree: degree_value,
                    });
                }
                ASMStatement::RegisterDeclaration(start, name, flag) => {
                    registers.push(RegisterDeclarationStatement { start, name, flag });
                }
                ASMStatement::InstructionDeclaration(start, name, params, body) => {
                    instructions.push(InstructionDefinitionStatement {
                        start,
                        name,
                        params,
                        body,
                    });
                }
                ASMStatement::InlinePil(start, statements) => {
                    pil.push(PilBlock { start, statements });
                }
                s => statements.push(s),
            }
        }

        AnalysisASMFile {
            degree,
            registers,
            pil,
            instructions,
            statements,
            batches: None,
        }
    }
}

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

pub struct PilBlock<T> {
    pub start: usize,
    pub statements: Vec<Statement<T>>,
}

pub struct AnalysisASMFile<T> {
    pub degree: Option<DegreeStatement>,
    pub registers: Vec<RegisterDeclarationStatement>,
    pub pil: Vec<PilBlock<T>>,
    pub instructions: Vec<InstructionDefinitionStatement<T>>,
    pub statements: Vec<ASMStatement<T>>,
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
    pub fn get_size(&self) -> usize {
        self.size
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum Incompatible {
    BusyWriteRegister,
    BusyAssignmentRegister,
    Label,
    ReadAfterWrite,
    Jump,
    Unimplemented,
}

#[derive(Debug, PartialEq, Eq, Default, Clone)]
pub struct IncompatibleSet(pub BTreeSet<Incompatible>);
