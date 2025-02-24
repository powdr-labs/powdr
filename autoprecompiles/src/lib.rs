use std::collections::{BTreeMap, BTreeSet};

use powdr_ast::analyzed::{
    AlgebraicBinaryOperator, AlgebraicExpression, AlgebraicReference, PolyID, PolynomialType,
};

use powdr_number::FieldElement;
use serde::{Deserialize, Serialize};

pub mod powdr;

const MAIN_MACHINE_STR: &str = "::Main";

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SymbolicInstructionStatement<T> {
    pub name: String,
    pub args: Vec<AlgebraicExpression<T>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SymbolicInstructionDefinition {
    pub name: String,
    pub inputs: Vec<String>,
    pub outputs: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SymbolicConstraint<T> {
    pub expr: AlgebraicExpression<T>,
}

impl<T> From<AlgebraicExpression<T>> for SymbolicConstraint<T> {
    fn from(expr: AlgebraicExpression<T>) -> Self {
        SymbolicConstraint { expr }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SymbolicBusInteraction<T> {
    pub kind: BusInteractionKind,
    pub id: u64,
    pub mult: AlgebraicExpression<T>,
    pub args: Vec<AlgebraicExpression<T>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum BusInteractionKind {
    Send,
    Receive,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SymbolicMachine<T> {
    pub constraints: Vec<SymbolicConstraint<T>>,
    pub bus_interactions: Vec<SymbolicBusInteraction<T>>,
}

impl<T> SymbolicMachine<T> {
    pub fn algebraic_expressions(&self) -> impl Iterator<Item = &AlgebraicExpression<T>> {
        let constraints_exprs = self.constraints.iter().map(|constraint| &constraint.expr);

        let bus_mult_exprs = self
            .bus_interactions
            .iter()
            .map(|interaction| &interaction.mult);

        let bus_args_exprs = self
            .bus_interactions
            .iter()
            .flat_map(|interaction| interaction.args.iter());

        constraints_exprs
            .chain(bus_mult_exprs)
            .chain(bus_args_exprs)
    }
}

#[derive(Debug, Clone)]
pub enum InstructionKind {
    Normal,
    ConditionalBranch,
    UnconditionalBranch,
    Terminal,
}

#[derive(Debug, Clone)]
pub struct Autoprecompiles<T> {
    pub program: Vec<SymbolicInstructionStatement<T>>,
    pub instruction_kind: BTreeMap<String, InstructionKind>,
    pub instruction_machines: BTreeMap<String, (SymbolicInstructionDefinition, SymbolicMachine<T>)>,
}

#[derive(Debug, Clone)]
pub struct BasicBlock<T> {
    pub start_idx: u64,
    pub statements: Vec<SymbolicInstructionStatement<T>>,
}

impl<T: FieldElement + Clone> Autoprecompiles<T> {
    pub fn run(
        &self,
    ) -> (
        Vec<SymbolicInstructionStatement<T>>,
        Vec<(String, SymbolicMachine<T>)>,
        Vec<BTreeMap<String, String>>,
    ) {
        let blocks = self.collect_basic_blocks();
        let new_instr_name = "new_instr".to_string();
        let new_instr = SymbolicInstructionStatement {
            name: new_instr_name.clone(),
            args: Vec::new(),
        };
        let selected = [(0, new_instr)].into();
        let new_program = replace_autoprecompile_basic_blocks(&blocks, &selected);

        let (machine, col_subs) = generate_precompile(
            &blocks[0].statements,
            &self.instruction_kind,
            &self.instruction_machines,
        );

        (new_program, vec![(new_instr_name, machine)], col_subs)
    }

    pub fn collect_basic_blocks(&self) -> Vec<BasicBlock<T>> {
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

pub fn replace_autoprecompile_basic_blocks<T: Clone>(
    blocks: &Vec<BasicBlock<T>>,
    selected: &BTreeMap<u64, SymbolicInstructionStatement<T>>,
) -> Vec<SymbolicInstructionStatement<T>> {
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

pub fn generate_precompile<T: Clone + Ord + std::fmt::Debug>(
    statements: &Vec<SymbolicInstructionStatement<T>>,
    instruction_kinds: &BTreeMap<String, InstructionKind>,
    instruction_machines: &BTreeMap<String, (SymbolicInstructionDefinition, SymbolicMachine<T>)>,
) -> (SymbolicMachine<T>, Vec<BTreeMap<String, String>>) {
    let mut col_counter = 3;
    let mut constraints: Vec<SymbolicConstraint<T>> = Vec::new();
    let mut bus_interactions: Vec<SymbolicBusInteraction<T>> = Vec::new();
    let mut col_subs: Vec<BTreeMap<String, String>> = Vec::new();

    //let one = AlgebraicExpression::Number(T::from(1));

    let dest_ref = AlgebraicReference {
        name: "dest".to_string(),
        poly_id: PolyID {
            ptype: PolynomialType::Intermediate,
            id: 0,
        },
        next: false,
    };
    let dest_ref: AlgebraicExpression<T> = AlgebraicExpression::Reference(dest_ref);

    let pc_next_ref = AlgebraicReference {
        name: "pc_next".to_string(),
        poly_id: PolyID {
            ptype: PolynomialType::Committed,
            id: 1,
        },
        next: false,
    };
    let pc_next_ref: AlgebraicExpression<T> = AlgebraicExpression::Reference(pc_next_ref);

    let pc_ref = AlgebraicReference {
        name: "pc".to_string(),
        poly_id: PolyID {
            ptype: PolynomialType::Intermediate,
            id: 2,
        },
        next: false,
    };
    let pc_ref: AlgebraicExpression<T> = AlgebraicExpression::Reference(pc_ref);

    for instr in statements.iter() {
        match instruction_kinds.get(&instr.name).unwrap() {
            InstructionKind::Normal
            | InstructionKind::UnconditionalBranch
            | InstructionKind::ConditionalBranch => {
                let (instr_def, machine) = instruction_machines.get(&instr.name).unwrap();

                let mut local_cols: BTreeSet<AlgebraicExpression<T>> = Default::default();
                machine.algebraic_expressions().for_each(|e| {
                    let cols = powdr::collect_cols_algebraic(e);
                    local_cols.extend(cols);
                });

                let (local_col_subs, local_col_exprs): (
                    BTreeMap<String, String>,
                    BTreeMap<String, AlgebraicExpression<T>>,
                ) = local_cols
                    .iter()
                    .map(|e| {
                        if let AlgebraicExpression::Reference(expr_ref) = e {
                            col_counter += 1;
                            let col_name = format!("w_{col_counter}");
                            let new_expr_ref = AlgebraicReference {
                                name: col_name.clone(),
                                poly_id: PolyID {
                                    ptype: expr_ref.poly_id.ptype,
                                    id: col_counter,
                                },
                                ..expr_ref.clone()
                            };
                            let new_expr_ref: AlgebraicExpression<T> =
                                AlgebraicExpression::Reference(new_expr_ref);
                            (
                                (expr_ref.name.clone(), col_name.clone()),
                                (expr_ref.name.clone(), new_expr_ref),
                            )
                        } else {
                            panic!("Expected reference")
                        }
                    })
                    .collect();

                col_subs.push(local_col_subs);

                let sub_map: BTreeMap<String, AlgebraicExpression<T>> = instr_def
                    .inputs
                    .clone()
                    .into_iter()
                    .zip(instr.args.clone())
                    .collect();

                let local_cols = machine
                    .cols
                    .iter()
                    .map(|col| {
                        let name = format!("w{col_counter}");
                        let expr = AlgebraicExpression::Reference(AlgebraicReference {
                            name: name.clone(),
                            poly_id: PolyID {
                                ptype: PolynomialType::Committed,
                                id: col_counter,
                            },
                            next: false,
                        });
                        col_counter += 1;
                        (col.clone(), expr)
                    })
                    .collect::<BTreeMap<_, _>>();

                let local_identities = machine
                    .constraints
                    .iter()
                    .map(|expr| {
                        let mut expr = expr.expr.clone();
                        powdr::substitute_algebraic(&mut expr, &sub_map);
                        powdr::substitute_algebraic(&mut expr, &local_cols);
                        SymbolicConstraint { expr }
                    })
                    .collect::<Vec<_>>();

                cols.extend(local_cols.values().map(|e| match e {
                    AlgebraicExpression::Reference(ref r) => r.name.clone(),
                    _ => panic!("Expected reference"),
                }));
                constraints.extend(local_identities);

                for bus_int in &machine.bus_interactions {
                    let mut link = bus_int.clone();
                    link.args
                        .iter_mut()
                        .chain(std::iter::once(&mut link.mult))
                        .for_each(|e| {
                            powdr::substitute_algebraic(e, &sub_map);
                        });
                    bus_interactions.push(link);
                }
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

    /*
        if !last_is_branch {
            let pc_plus_one =
                AlgebraicExpression::new_binary(pc_ref.clone(), AlgebraicBinaryOperator::Add, one);
            let pc_eq_pc_plus_one = AlgebraicExpression::new_binary(
                pc_next_ref.clone(),
                AlgebraicBinaryOperator::Sub,
                pc_plus_one,
            );
            constraints.push(SymbolicConstraint {
                expr: pc_eq_pc_plus_one,
            });
        }
    */

    (
        SymbolicMachine {
            constraints,
            bus_interactions,
        },
        col_subs,
    )
}

#[cfg(test)]
mod test {
    use powdr_ast::analyzed::AlgebraicBinaryOperator;
    use powdr_number::BabyBearField;

    use super::*;

    #[test]
    fn test_replace_autoprecompile_basic_blocks() {
        let blocks: Vec<BasicBlock<AlgebraicExpression<BabyBearField>>> = vec![
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
        let one = AlgebraicExpression::Number(BabyBearField::from(1u32));

        let a_ref = AlgebraicReference {
            name: "a".to_string(),
            poly_id: PolyID {
                ptype: PolynomialType::Intermediate,
                id: 0,
            },
            next: false,
        };
        let a_ref = AlgebraicExpression::Reference(a_ref);

        let b_ref = AlgebraicReference {
            name: "b".to_string(),
            poly_id: PolyID {
                ptype: PolynomialType::Intermediate,
                id: 1,
            },
            next: false,
        };
        let b_ref = AlgebraicExpression::Reference(b_ref);

        let a_plus_one = AlgebraicExpression::new_binary(
            a_ref.clone(),
            AlgebraicBinaryOperator::Add,
            one.clone(),
        );
        let b_eq_a_plus_one = AlgebraicExpression::new_binary(
            b_ref.clone(),
            AlgebraicBinaryOperator::Sub,
            a_plus_one,
        );

        let c_ref: AlgebraicReference = AlgebraicReference {
            name: "c".to_string(),
            poly_id: PolyID {
                ptype: PolynomialType::Intermediate,
                id: 2,
            },
            next: false,
        };
        let c_ref = AlgebraicExpression::Reference(c_ref);

        let d_ref: AlgebraicReference = AlgebraicReference {
            name: "d".to_string(),
            poly_id: PolyID {
                ptype: PolynomialType::Intermediate,
                id: 3,
            },
            next: false,
        };
        let d_ref = AlgebraicExpression::Reference(d_ref);

        let bus_send = SymbolicBusInteraction {
            kind: BusInteractionKind::Send,
            id: 1,
            mult: one.clone(),
            args: vec![c_ref, d_ref],
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
        println!("{new_machines:?}");
    }
}
