use std::collections::{BTreeMap, BTreeSet};
use std::ops::ControlFlow;

use powdr_ast::asm_analysis::InstructionDefinitionStatement;
use powdr_ast::parsed::asm::{
    parse_absolute_path, AbsoluteSymbolPath, CallableRef, Instruction, InstructionBody,
    LinkDeclaration, MachineParams, OperationId, Param, Params, Part, SymbolPath,
};
use powdr_ast::{asm_analysis::AnalysisASMFile, parsed::asm::ASMProgram};
use powdr_ast::{
    asm_analysis::{
        CallableSymbol, CallableSymbolDefinitions, FunctionStatement, FunctionStatements,
        InstructionStatement, LabelStatement, LinkDefinition, Machine, MachineDegree, Module,
        OperationSymbol, RegisterTy, SubmachineDeclaration,
    },
    parsed::{
        visitor::{ExpressionVisitable, VisitOrder},
        BinaryOperator, FunctionCall, NamespacedPolynomialReference, Number, PilStatement,
        UnaryOperation, UnaryOperator,
    },
};
use powdr_number::BigUint;
use powdr_number::FieldElement;
use powdr_parser_util::SourceRef;

pub mod powdr;

type Expression = powdr_ast::asm_analysis::Expression<NamespacedPolynomialReference>;

const MAIN_MACHINE_STR: &str = "::Main";

pub struct SymbolicInstruction {
    pub name: String,
    pub args: Vec<String>,
}

pub struct SymbolicConstraint {
    pub expr: Expression,
}

pub struct SymbolicBusInteraction {
    pub kind: BusInteractionKind,
    pub id: u64,
    pub mult: Expression,
    pub args: Vec<Expression>,
}

pub enum BusInteractionKind {
    Send,
    Receive,
}

pub struct SymbolicMachine {
    pub cols: BTreeSet<String>,
    pub constraints: Vec<SymbolicConstraint>,
    pub bus_interactions: Vec<SymbolicBusInteraction>,
}

pub enum InstructionKind {
    Normal,
    ConditionalBranch,
    UnconditionalBranch,
    Terminal,
}

pub struct Autoprecompiles {
    pub program: Vec<SymbolicInstruction>,
    pub instruction_kind: BTreeMap<String, InstructionKind>,
    pub instruction_machines: BTreeMap<String, SymbolicMachine>,
}
