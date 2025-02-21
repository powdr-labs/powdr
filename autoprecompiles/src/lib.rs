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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SymbolicInstructionStatement {
    pub name: String,
    pub args: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct SymbolicInstructionDefinition {
    pub name: String,
    pub inputs: Vec<String>,
    pub outputs: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct SymbolicConstraint {
    pub expr: Expression,
}

impl From<Expression> for SymbolicConstraint {
    fn from(expr: Expression) -> Self {
        SymbolicConstraint { expr }
    }
}

#[derive(Debug, Clone)]
pub struct SymbolicBusInteraction {
    pub kind: BusInteractionKind,
    pub id: u64,
    pub mult: Expression,
    pub args: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub enum BusInteractionKind {
    Send,
    Receive,
}

#[derive(Debug, Clone)]
pub struct SymbolicMachine {
    pub cols: BTreeSet<String>,
    pub constraints: Vec<SymbolicConstraint>,
    pub bus_interactions: Vec<SymbolicBusInteraction>,
}

#[derive(Debug, Clone)]
pub enum InstructionKind {
    Normal,
    ConditionalBranch,
    UnconditionalBranch,
    Terminal,
}

#[derive(Debug, Clone)]
pub struct Autoprecompiles {
    pub program: Vec<SymbolicInstructionStatement>,
    pub instruction_kind: BTreeMap<String, InstructionKind>,
    pub instruction_machines: BTreeMap<String, (SymbolicInstructionDefinition, SymbolicMachine)>,
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub start_idx: u64,
    pub statements: Vec<SymbolicInstructionStatement>,
}

impl Autoprecompiles {
    pub fn run(
        &self,
    ) -> (
        Vec<SymbolicInstructionStatement>,
        Vec<(String, SymbolicMachine)>,
    ) {
        let blocks = self.collect_basic_blocks();
        let new_instr_name = "new_instr".to_string();
        let new_instr = SymbolicInstructionStatement {
            name: new_instr_name.clone(),
            args: Vec::new(),
        };
        let selected = [(0, new_instr)].into();
        let new_program = replace_autoprecompile_basic_blocks(&blocks, &selected);

        let machine = generate_precompile(
            &blocks[0].statements,
            &self.instruction_kind,
            &self.instruction_machines,
        );

        (new_program, vec![(new_instr_name, machine)])
    }

    pub fn collect_basic_blocks(&self) -> Vec<BasicBlock> {
        let mut blocks = Vec::new();
        let mut curr_block = BasicBlock {
            start_idx: 0,
            statements: Vec::new(),
        };
        for (i, instr) in self.program.iter().enumerate() {
            match self.instruction_kind.get(&instr.name).unwrap() {
                InstructionKind::Normal => {
                    curr_block.statements.push(instr.clone());
                }
                InstructionKind::ConditionalBranch
                | InstructionKind::UnconditionalBranch
                | InstructionKind::Terminal => {
                    curr_block.statements.push(instr.clone());
                    blocks.push(curr_block);
                    curr_block = BasicBlock {
                        start_idx: i as u64,
                        statements: Vec::new(),
                    };
                }
            }
        }

        if !curr_block.statements.is_empty() {
            blocks.push(curr_block);
        }

        blocks
    }
}

pub fn replace_autoprecompile_basic_blocks(
    blocks: &Vec<BasicBlock>,
    selected: &BTreeMap<u64, SymbolicInstructionStatement>,
) -> Vec<SymbolicInstructionStatement> {
    let mut new_program = Vec::new();
    for (i, block) in blocks.iter().enumerate() {
        if let Some(instr) = selected.get(&(i as u64)) {
            new_program.push(instr.clone());
        } else {
            new_program.extend(block.statements.clone());
        }
    }
    new_program
}

pub fn generate_precompile(
    statements: &Vec<SymbolicInstructionStatement>,
    instruction_kinds: &BTreeMap<String, InstructionKind>,
    instruction_machines: &BTreeMap<String, (SymbolicInstructionDefinition, SymbolicMachine)>,
) -> SymbolicMachine {
    let mut ssa_counter = 0;
    let mut cols: BTreeSet<String> = BTreeSet::new();
    let mut constraints: Vec<SymbolicConstraint> = Vec::new();
    let mut bus_interactions: Vec<SymbolicBusInteraction> = Vec::new();

    // let dest;
    cols.insert("dest".to_string());

    // let pc;
    // let pc_next;
    cols.insert("pc".to_string());
    cols.insert("pc_next".to_string());

    let dest_symbol = SymbolPath::from_identifier("dest".to_string());
    let dest_ref: NamespacedPolynomialReference = dest_symbol.into();
    let dest_ref = Expression::Reference(Default::default(), dest_ref);

    let one = Expression::Number(
        Default::default(),
        Number {
            value: BigUint::from(1u32),
            type_: None,
        },
    );

    let pc_next_symbol = SymbolPath::from_identifier("pc_next".to_string());
    let pc_next_ref: NamespacedPolynomialReference = pc_next_symbol.into();
    let pc_next_ref = Expression::Reference(Default::default(), pc_next_ref);

    let pc_symbol = SymbolPath::from_identifier("pc".to_string());
    let pc_ref: NamespacedPolynomialReference = pc_symbol.into();
    let pc_ref = Expression::Reference(Default::default(), pc_ref);

    for (i, instr) in statements.iter().enumerate() {
        match instruction_kinds.get(&instr.name).unwrap() {
            InstructionKind::Normal
            | InstructionKind::UnconditionalBranch
            | InstructionKind::ConditionalBranch => {
                let (instr_def, machine) = instruction_machines.get(&instr.name).unwrap();

                // Create initial substitution map
                let mut sub_map: BTreeMap<String, Expression> = instr_def
                    .inputs
                    .clone()
                    .into_iter()
                    .zip(instr.args.clone())
                    .collect();

                let local_cols = machine
                    .cols
                    .iter()
                    .map(|col| format!("{}_{ssa_counter}", col.clone()))
                    .collect::<Vec<_>>();

                // Constraints from main
                let local_identities = machine
                    .constraints
                    .iter()
                    .map(|expr| {
                        let mut expr = expr.expr.clone();
                        powdr::append_suffix_mut(&mut expr, &ssa_counter.to_string());
                        SymbolicConstraint { expr }
                    })
                    .collect::<Vec<_>>();

                cols.extend(local_cols);
                constraints.extend(local_identities);

                for bus_int in &machine.bus_interactions {
                    let mut link = bus_int.clone();
                    link.args
                        .iter_mut()
                        .chain(std::iter::once(&mut link.mult))
                        .for_each(|e| {
                            powdr::substitute(e, &sub_map);
                            powdr::append_suffix_mut(e, &ssa_counter.to_string());
                        });
                    bus_interactions.push(link);
                }

                ssa_counter += 1;
            }
            _ => {}
        }
    }

    let last_is_branch = matches!(
        instruction_kinds
            .get(&statements.last().unwrap().name)
            .unwrap(),
        InstructionKind::UnconditionalBranch | InstructionKind::ConditionalBranch
    );

    if !last_is_branch {
        let pc_plus_one = Expression::new_binary(pc_ref.clone(), BinaryOperator::Add, one);
        let pc_eq_pc_plus_one =
            Expression::new_binary(pc_next_ref.clone(), BinaryOperator::Identity, pc_plus_one);
        constraints.push(SymbolicConstraint {
            expr: pc_eq_pc_plus_one,
        });
    }

    SymbolicMachine {
        cols,
        constraints,
        bus_interactions,
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_replace_autoprecompile_basic_blocks() {
        let blocks = vec![
            BasicBlock {
                start_idx: 0,
                statements: vec![
                    SymbolicInstructionStatement {
                        name: "instr1".to_string(),
                        args: vec![],
                    },
                    SymbolicInstructionStatement {
                        name: "instr2".to_string(),
                        args: vec![],
                    },
                ],
            },
            BasicBlock {
                start_idx: 2,
                statements: vec![
                    SymbolicInstructionStatement {
                        name: "instr3".to_string(),
                        args: vec![],
                    },
                    SymbolicInstructionStatement {
                        name: "instr4".to_string(),
                        args: vec![],
                    },
                ],
            },
        ];
        let selected = [(
            0,
            SymbolicInstructionStatement {
                name: "new_instr".to_string(),
                args: vec![],
            },
        )]
        .into();
        let new_program = replace_autoprecompile_basic_blocks(&blocks, &selected);
        assert_eq!(new_program.len(), 3);
        assert_eq!(new_program[0].name, "new_instr");
        assert_eq!(new_program[1].name, "instr3");
        assert_eq!(new_program[2].name, "instr4");
    }

    #[test]
    fn test_generate_precompile() {
        let program = vec![
            SymbolicInstructionStatement {
                name: "instr1".to_string(),
                args: vec![],
            },
            SymbolicInstructionStatement {
                name: "instr2".to_string(),
                args: vec![],
            },
            SymbolicInstructionStatement {
                name: "instr3".to_string(),
                args: vec![],
            },
            SymbolicInstructionStatement {
                name: "instr4".to_string(),
                args: vec![],
            },
        ];
        let one = Expression::Number(
            Default::default(),
            Number {
                value: BigUint::from(1u32),
                type_: None,
            },
        );
        let a_symbol = SymbolPath::from_identifier("a".to_string());
        let a_ref: NamespacedPolynomialReference = a_symbol.into();
        let a_ref = Expression::Reference(Default::default(), a_ref);

        let b_symbol = SymbolPath::from_identifier("b".to_string());
        let b_ref: NamespacedPolynomialReference = b_symbol.into();
        let b_ref = Expression::Reference(Default::default(), b_ref);

        let a_plus_one = Expression::new_binary(a_ref.clone(), BinaryOperator::Add, one.clone());
        let b_eq_a_plus_one =
            Expression::new_binary(b_ref.clone(), BinaryOperator::Identity, a_plus_one);

        let c_symbol = SymbolPath::from_identifier("c".to_string());
        let c_ref: NamespacedPolynomialReference = c_symbol.into();
        let c_ref = Expression::Reference(Default::default(), c_ref);

        let d_symbol = SymbolPath::from_identifier("d".to_string());
        let d_ref: NamespacedPolynomialReference = d_symbol.into();
        let d_ref = Expression::Reference(Default::default(), d_ref);

        let bus_send = SymbolicBusInteraction {
            kind: BusInteractionKind::Send,
            id: 1,
            mult: one.clone(),
            args: vec![c_ref.into(), d_ref.into()],
        };

        let autoprecompiles = Autoprecompiles {
            program,
            instruction_kind: [
                ("instr1".to_string(), InstructionKind::Normal),
                ("instr2".to_string(), InstructionKind::Normal),
                ("instr3".to_string(), InstructionKind::UnconditionalBranch),
                ("instr4".to_string(), InstructionKind::Terminal),
            ]
            .into(),
            instruction_machines: [
                (
                    "instr1".to_string(),
                    (
                        SymbolicInstructionDefinition {
                            name: "instr1".to_string(),
                            inputs: vec!["a".to_string()],
                            outputs: vec!["b".to_string()],
                        },
                        SymbolicMachine {
                            cols: BTreeSet::new(),
                            constraints: vec![b_eq_a_plus_one.into()],
                            bus_interactions: vec![],
                        },
                    ),
                ),
                (
                    "instr2".to_string(),
                    (
                        SymbolicInstructionDefinition {
                            name: "instr2".to_string(),
                            inputs: vec!["c".to_string()],
                            outputs: vec!["d".to_string()],
                        },
                        SymbolicMachine {
                            cols: BTreeSet::new(),
                            constraints: vec![],
                            bus_interactions: vec![bus_send],
                        },
                    ),
                ),
                (
                    "instr3".to_string(),
                    (
                        SymbolicInstructionDefinition {
                            name: "instr3".to_string(),
                            inputs: vec![],
                            outputs: vec![],
                        },
                        SymbolicMachine {
                            cols: BTreeSet::new(),
                            constraints: vec![],
                            bus_interactions: vec![],
                        },
                    ),
                ),
                (
                    "instr4".to_string(),
                    (
                        SymbolicInstructionDefinition {
                            name: "instr4".to_string(),
                            inputs: vec![],
                            outputs: vec![],
                        },
                        SymbolicMachine {
                            cols: BTreeSet::new(),
                            constraints: vec![],
                            bus_interactions: vec![],
                        },
                    ),
                ),
            ]
            .into(),
        };
        let (new_program, new_machines) = autoprecompiles.run();
        let expected_program = vec![
            SymbolicInstructionStatement {
                name: "new_instr".to_string(),
                args: vec![],
            },
            SymbolicInstructionStatement {
                name: "instr4".to_string(),
                args: vec![],
            },
        ];
        assert_eq!(new_program, expected_program);
        println!("{:?}", new_machines);
    }
}
