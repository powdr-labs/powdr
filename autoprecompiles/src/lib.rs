use itertools::Itertools;
use powdr::collect_cols_algebraic;
use powdr_parser_util::SourceRef;
use powdr_pil_analyzer::analyze_ast;
use powdr_pilopt::optimize;
use std::{
    collections::{BTreeMap, BTreeSet, HashSet},
    str::FromStr,
};

use powdr_ast::{
    analyzed::{
        AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression, AlgebraicReference,
        AlgebraicUnaryOperator, Analyzed, BusInteractionIdentity, Identity, PolyID,
        PolynomialIdentity, PolynomialType,
    },
    parsed::{
        asm::SymbolPath, visitor::AllChildren, ArrayLiteral, BinaryOperation, BinaryOperator,
        Expression, FunctionCall, NamespacedPolynomialReference, Number, PILFile, PilStatement,
        UnaryOperation, UnaryOperator,
    },
};
use serde::{Deserialize, Serialize};
use std::convert::TryFrom;

use powdr_number::{BigUint, FieldElement, LargeInt};
use powdr_pilopt::simplify_expression;

pub mod powdr;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SymbolicInstructionStatement<T> {
    pub name: String,
    pub opcode: usize,
    pub args: Vec<T>,
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

impl<T: Clone + Ord + std::fmt::Display> SymbolicConstraint<T> {
    pub fn columns(&self) -> BTreeSet<String> {
        let mut cols = BTreeSet::new();
        cols.extend(powdr::collect_cols_names_algebraic(&self.expr));
        cols
    }

    pub fn column_ids(&self) -> BTreeSet<u64> {
        let mut cols = BTreeSet::new();
        cols.extend(powdr::collect_cols_ids_algebraic(&self.expr));
        cols
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub struct SymbolicBusInteraction<T> {
    pub kind: BusInteractionKind,
    pub id: u64,
    pub mult: AlgebraicExpression<T>,
    pub args: Vec<AlgebraicExpression<T>>,
}

impl<T: Clone + Ord + std::fmt::Display> SymbolicBusInteraction<T> {
    pub fn columns(&self) -> BTreeSet<String> {
        let mut cols = BTreeSet::new();
        cols.extend(powdr::collect_cols_names_algebraic(&self.mult));
        for a in &self.args {
            cols.extend(powdr::collect_cols_names_algebraic(&a));
        }
        cols
    }

    pub fn column_ids(&self) -> BTreeSet<u64> {
        let mut cols = BTreeSet::new();
        cols.extend(powdr::collect_cols_ids_algebraic(&self.mult));
        for a in &self.args {
            cols.extend(powdr::collect_cols_ids_algebraic(&a));
        }
        cols
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
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

impl<T: Clone + Ord + std::fmt::Display> SymbolicMachine<T> {
    pub fn columns(&self) -> BTreeSet<String> {
        let mut cols = BTreeSet::new();
        for c in &self.constraints {
            cols.extend(c.columns());
        }
        for b in &self.bus_interactions {
            cols.extend(b.columns());
        }
        cols
    }

    pub fn column_ids(&self) -> BTreeSet<u64> {
        let mut cols = BTreeSet::new();
        for c in &self.constraints {
            cols.extend(c.column_ids());
        }
        for b in &self.bus_interactions {
            cols.extend(b.column_ids());
        }
        cols
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

#[derive(Clone, Debug)]
pub enum MemoryType {
    Constant,
    Register,
    Memory,
}

impl<T: FieldElement> From<AlgebraicExpression<T>> for MemoryType {
    fn from(expr: AlgebraicExpression<T>) -> Self {
        match expr {
            AlgebraicExpression::Number(n) => {
                let n_u32 = n.to_integer().try_into_u32().unwrap();
                match n_u32 {
                    0 => MemoryType::Constant,
                    1 => MemoryType::Register,
                    2 => MemoryType::Memory,
                    _ => unreachable!("Expected 0, 1 or 2 but got {n}"),
                }
            }
            _ => unreachable!("Expected number"),
        }
    }
}

impl<T: FieldElement> From<MemoryType> for AlgebraicExpression<T> {
    fn from(ty: MemoryType) -> Self {
        match ty {
            MemoryType::Constant => AlgebraicExpression::Number(T::from(0u32)),
            MemoryType::Register => AlgebraicExpression::Number(T::from(1u32)),
            MemoryType::Memory => AlgebraicExpression::Number(T::from(2u32)),
        }
    }
}

#[derive(Clone, Debug)]
pub enum MemoryOp {
    Read,
    Write,
}

impl From<BusInteractionKind> for MemoryOp {
    fn from(kind: BusInteractionKind) -> Self {
        match kind {
            BusInteractionKind::Receive => MemoryOp::Read,
            BusInteractionKind::Send => MemoryOp::Write,
        }
    }
}

impl From<MemoryOp> for BusInteractionKind {
    fn from(op: MemoryOp) -> Self {
        match op {
            MemoryOp::Read => BusInteractionKind::Receive,
            MemoryOp::Write => BusInteractionKind::Send,
        }
    }
}

#[derive(Clone, Debug)]
pub struct MemoryBusInteraction<T> {
    pub ty: MemoryType,
    pub op: MemoryOp,
    pub addr: AlgebraicExpression<T>,
    pub data: Vec<AlgebraicExpression<T>>,
    pub bus_interaction: SymbolicBusInteraction<T>,
}

impl<T: FieldElement> MemoryBusInteraction<T> {
    pub fn try_addr_u32(&self) -> Option<u32> {
        match self.addr {
            AlgebraicExpression::Number(n) => n.to_integer().try_into_u32(),
            _ => None,
        }
    }
}

impl<T: FieldElement> From<SymbolicBusInteraction<T>> for MemoryBusInteraction<T> {
    fn from(bus_interaction: SymbolicBusInteraction<T>) -> Self {
        //println!("\n\nBus interaction is {bus_interaction:?}\n\n");
        //println!("\n\nAS = {}\n\n", bus_interaction.args[0]);
        let ty = bus_interaction.args[0].clone().into();
        let op = bus_interaction.kind.clone().into();
        let addr = bus_interaction.args[1].clone();
        let data = bus_interaction.args[2..bus_interaction.args.len() - 2].to_vec();
        MemoryBusInteraction {
            ty,
            op,
            addr,
            data,
            bus_interaction,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct PcLookupBusInteraction<T> {
    pub from_pc: AlgebraicExpression<T>,
    pub op: AlgebraicExpression<T>,
    pub args: Vec<AlgebraicExpression<T>>,
    pub bus_interaction: SymbolicBusInteraction<T>,
}

impl<T: FieldElement> From<SymbolicBusInteraction<T>> for PcLookupBusInteraction<T> {
    fn from(bus_interaction: SymbolicBusInteraction<T>) -> Self {
        let from_pc = bus_interaction.args[0].clone();
        let op = bus_interaction.args[1].clone();
        let args = bus_interaction.args[2..].to_vec();
        PcLookupBusInteraction {
            from_pc,
            op,
            args,
            bus_interaction,
        }
    }
}

pub enum VMBusInteraction<T> {
    Memory(MemoryBusInteraction<T>),
}

const EXECUTION_BUS_ID: u64 = 0;
const MEMORY_BUS_ID: u64 = 1;
const PC_LOOKUP_BUS_ID: u64 = 2;

impl<T: FieldElement> Autoprecompiles<T> {
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
            opcode: 0,
            args: Vec::new(),
        };
        let selected = [(0, new_instr)].into();
        let new_program = replace_autoprecompile_basic_blocks(&blocks, &selected);

        let (machine, col_subs) = generate_precompile(
            &blocks[0].statements,
            &self.instruction_kind,
            &self.instruction_machines,
        );

        let machine = optimize_precompile(machine);

        (new_program, vec![(new_instr_name, machine)], col_subs)
    }

    pub fn build(&self) -> (SymbolicMachine<T>, Vec<BTreeMap<String, String>>) {
        let (machine, subs) = generate_precompile(
            &self.program,
            &self.instruction_kind,
            &self.instruction_machines,
        );
        let c = machine.columns();
        let i = machine.column_ids();
        // println!("\n\nCollecting info");
        // println!("\nC:\n{c:?}");
        // println!("\nI:\n{i:?}");
        // for c in &machine.constraints {
        //     println!("Constraint: {}", c.expr);
        // }
        // for i in &machine.bus_interactions {
        //     println!(
        //         "\nBus interaction id = {}, kind = {:?}, mult = {}",
        //         i.id, i.kind, i.mult
        //     );
        //     for a in &i.args {
        //         println!("arg = {a}");
        //     }
        //     println!("\n");
        // }
        assert_eq!(c.len(), i.len());
        assert_eq!(machine.columns().len(), machine.column_ids().len());
        let machine = optimize_precompile(machine);
        let machine = powdr_optimize(machine);

        (machine, subs)
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

pub fn optimize_precompile<T: FieldElement>(mut machine: SymbolicMachine<T>) -> SymbolicMachine<T> {
    println!(
        "Before autoprecompile optimizations, columns: {}, constraints: {}, bus_interactions: {}",
        machine.columns().len(),
        machine.constraints.len(),
        machine.bus_interactions.len()
    );

    /*
        for c in &machine.constraints {
            println!("Constraint: {}", c.expr);
        }
    */

    let mut local_mem: BTreeMap<u32, Vec<AlgebraicExpression<T>>> = BTreeMap::new();
    let mut new_constraints: Vec<SymbolicConstraint<T>> = Vec::new();
    machine.bus_interactions.retain(|bus_int| {
        if bus_int.id != MEMORY_BUS_ID {
            return true;
        }

        let mem_int: MemoryBusInteraction<T> = bus_int.clone().into();

        if matches!(mem_int.ty, MemoryType::Constant | MemoryType::Memory) {
            return true;
        }

        let addr = match mem_int.try_addr_u32() {
            None => {
                panic!("Register memory access must have constant address");
            }
            Some(addr) => addr,
        };

        match mem_int.op {
            MemoryOp::Read => match local_mem.get(&addr) {
                Some(data) => {
                    assert_eq!(data.len(), mem_int.data.len());
                    /*
                        println!(
                            "Replacing bus interaction of addr {} by local constraints:",
                            mem_int.addr
                        );
                        println!("Data: {data:?}");
                        println!("Mem int data: {:?}", mem_int.data);
                    */
                    mem_int
                        .data
                        .iter()
                        .zip(data.iter())
                        .for_each(|(new_data, old_data)| {
                            let eq_expr = AlgebraicExpression::new_binary(
                                new_data.clone(),
                                AlgebraicBinaryOperator::Sub,
                                old_data.clone(),
                            );
                            //println!("New constraint: {eq_expr}");
                            new_constraints.push(eq_expr.into());
                        });
                    false
                }
                None => {
                    local_mem.insert(addr, mem_int.data.clone());
                    true
                }
            },
            MemoryOp::Write => {
                local_mem.insert(addr, mem_int.data.clone());
                true
            }
        }
    });

    let mut last_store: BTreeMap<u32, usize> = BTreeMap::new();
    machine
        .bus_interactions
        .iter()
        .enumerate()
        .for_each(|(i, bus_int)| {
            if bus_int.id != MEMORY_BUS_ID {
                return;
            }

            let mem_int: MemoryBusInteraction<T> = bus_int.clone().into();

            if matches!(mem_int.ty, MemoryType::Constant | MemoryType::Memory) {
                return;
            }

            let addr = match mem_int.try_addr_u32() {
                None => {
                    panic!("Register memory access must have constant address");
                }
                Some(addr) => addr,
            };

            match mem_int.op {
                MemoryOp::Read => {}
                MemoryOp::Write => {
                    last_store.insert(addr, i);
                }
            }
        });

    machine.bus_interactions = machine
        .bus_interactions
        .into_iter()
        .enumerate()
        .filter_map(|(i, bus_int)| {
            if bus_int.id != MEMORY_BUS_ID {
                return Some(bus_int);
            }

            let mem_int: MemoryBusInteraction<T> = bus_int.clone().into();

            if matches!(mem_int.ty, MemoryType::Constant | MemoryType::Memory) {
                return Some(bus_int);
            }

            let addr = match mem_int.try_addr_u32() {
                None => {
                    panic!("Register memory access must have constant address");
                }
                Some(addr) => addr,
            };

            match mem_int.op {
                MemoryOp::Read => {
                    return Some(bus_int);
                }
                MemoryOp::Write => {
                    if last_store
                        .get(&addr)
                        .is_some_and(|&last_index| last_index == i)
                    {
                        Some(bus_int)
                    } else {
                        //println!("Removing redundant register write");
                        //println!("Bus interaction: {:?}", bus_int);
                        None
                    }
                }
            }
        })
        .collect();

    machine.constraints.extend(new_constraints);

    println!(
        "After autoprecompile optimizations, columns: {}, constraints: {}, bus_interactions: {}",
        machine.columns().len(),
        machine.constraints.len(),
        machine.bus_interactions.len()
    );

    machine
}

pub fn generate_precompile<T: FieldElement>(
    statements: &Vec<SymbolicInstructionStatement<T>>,
    instruction_kinds: &BTreeMap<String, InstructionKind>,
    instruction_machines: &BTreeMap<String, (SymbolicInstructionDefinition, SymbolicMachine<T>)>,
) -> (SymbolicMachine<T>, Vec<BTreeMap<String, String>>) {
    println!("Generating autoprecompile inside powdr");
    let mut constraints: Vec<SymbolicConstraint<T>> = Vec::new();
    let mut bus_interactions: Vec<SymbolicBusInteraction<T>> = Vec::new();
    let mut col_subs: Vec<BTreeMap<String, String>> = Vec::new();
    let mut global_idx: usize = 3;
    let mut global_idx_subs: BTreeMap<String, usize> = BTreeMap::new();
    let mut global_idx_subs_rev: BTreeMap<usize, String> = BTreeMap::new();

    for (i, instr) in statements.iter().enumerate() {
        //println!("\n\nVisiting instruction {i}\n\n");
        //println!("Inlining instruction {} index {i}", &instr.name);
        match instruction_kinds.get(&instr.name).unwrap() {
            InstructionKind::Normal
            | InstructionKind::UnconditionalBranch
            | InstructionKind::ConditionalBranch => {
                let (instr_def, mut machine) =
                    instruction_machines.get(&instr.name).unwrap().clone();

                let pc_lookup: PcLookupBusInteraction<T> = machine
                    .bus_interactions
                    .iter()
                    .filter_map(|bus_int| match bus_int.id {
                        PC_LOOKUP_BUS_ID => Some(bus_int.clone().into()),
                        _ => None,
                    })
                    .exactly_one()
                    .expect("Expected single pc lookup");

                let mut sub_map: BTreeMap<String, AlgebraicExpression<T>> = Default::default();
                let mut local_constraints: Vec<SymbolicConstraint<T>> = Vec::new();

                if is_loadstore(instr.opcode) {
                    sub_map.extend(loadstore_chip_info(&machine, instr.opcode));
                }

                //println!("Inlining pc args");
                assert_eq!(instr.args.len(), pc_lookup.args.len());
                instr
                    .args
                    .iter()
                    .zip(pc_lookup.args.iter())
                    .for_each(|(instr_arg, pc_arg)| {
                        let arg = AlgebraicExpression::Number(instr_arg.clone());
                        //println!("\nInstruction arg: {instr_arg}");
                        //println!("\nPc arg: {pc_arg}\n");
                        match pc_arg {
                            AlgebraicExpression::Reference(ref arg_ref) => {
                                sub_map.insert(arg_ref.name.clone(), arg);
                            }
                            AlgebraicExpression::BinaryOperation(_expr) => {
                                local_constraints.push((arg - pc_arg.clone()).into());
                            }
                            AlgebraicExpression::UnaryOperation(_expr) => {
                                local_constraints.push((arg - pc_arg.clone()).into());
                            }
                            _ => {}
                        }
                    });

                /*
                   for l in &local_constraints {
                       println!("Local constraint: {}", l.expr);
                   }
                   println!("\nSubmap = {sub_map:?}\n");
                */

                let mut local_subs = BTreeMap::new();

                /*
                for l in &machine.constraints {
                    println!("\nMachine constraint: {}", l.expr);
                }
                for i in &machine.bus_interactions {
                    println!(
                        "\nBus interaction id = {}, kind = {:?}, mult = {}",
                        i.id, i.kind, i.mult
                    );
                    for a in &i.args {
                        println!("arg = {a}");
                    }
                    println!("\n");
                }
                */

                //println!("Computing local identities");
                let local_identities = machine
                    .constraints
                    .iter()
                    .chain(local_constraints.iter())
                    .map(|expr| {
                        //println!("handling local identity {}", expr.expr);
                        let mut expr = expr.expr.clone();
                        powdr::substitute_algebraic(&mut expr, &sub_map);
                        let subs = powdr::append_suffix_algebraic(&mut expr, &format!("{i}"));
                        expr = simplify_expression(expr);
                        global_idx = powdr::reassign_ids_algebraic(
                            &mut expr,
                            global_idx,
                            &mut global_idx_subs,
                            &mut global_idx_subs_rev,
                        );
                        //println!("became identity {expr}");
                        local_subs.extend(subs);
                        SymbolicConstraint { expr }
                    })
                    .collect::<Vec<_>>();

                constraints.extend(local_identities);

                //println!("Computing local bus interactions");
                for bus_int in &machine.bus_interactions {
                    //println!("\nhandling bus interaction\n");
                    let mut link = bus_int.clone();
                    link.args
                        .iter_mut()
                        .chain(std::iter::once(&mut link.mult))
                        .for_each(|e| {
                            //println!("\nHandling arg {e}");
                            powdr::substitute_algebraic(e, &sub_map);
                            *e = simplify_expression(e.clone());
                            let subs = powdr::append_suffix_algebraic(e, &format!("{i}"));
                            global_idx = powdr::reassign_ids_algebraic(
                                &mut *e,
                                global_idx,
                                &mut global_idx_subs,
                                &mut global_idx_subs_rev,
                            );
                            //println!("Became arg {e}\n");
                            local_subs.extend(subs);
                        });
                    bus_interactions.push(link);
                }

                col_subs.push(local_subs);

                //println!("Simplifying local bus interactions");
                // after the first round of simplifying,
                // we need to look for register memory bus interactions
                // and replace the addr by the first argument of the instruction
                for bus_int in &mut bus_interactions {
                    if bus_int.id != MEMORY_BUS_ID {
                        continue;
                    }

                    let addr_space = match bus_int.args[0] {
                        AlgebraicExpression::Number(n) => n.to_integer().try_into_u32().unwrap(),
                        _ => panic!(
                            "Address space must be a constant but got {}",
                            bus_int.args[0]
                        ),
                    };

                    if addr_space != 1 {
                        continue;
                    }

                    match bus_int.args[1] {
                        AlgebraicExpression::Number(_) => {}
                        _ => {
                            if let Some(arg) = bus_int.args.get_mut(1) {
                                //println!("Replacing runtime reg addr by constant");
                                *arg = instr.args[0].clone().into();
                            } else {
                                panic!("Expected address argument");
                            }
                        }
                    };
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

    //println!("\nFinal subs len: {}", global_idx_subs.len());
    //println!("\nFinal subs:\n{global_idx_subs:?}");

    (
        SymbolicMachine {
            constraints,
            bus_interactions,
        },
        col_subs,
    )
}

/// Use a SymbolicMachine to create a PILFile and run powdr's optimizations on it.
/// Then, translate the optimized constraints and interactions back to a SymbolicMachine
fn powdr_optimize<P: FieldElement>(symbolic_machine: SymbolicMachine<P>) -> SymbolicMachine<P> {
    println!(
        "Before powdr optimizations, columns: {}, constraints: {}, bus_interactions: {}",
        symbolic_machine.columns().len(),
        symbolic_machine.constraints.len(),
        symbolic_machine.bus_interactions.len()
    );

    let pilfile = symbolic_machine_to_pilfile(symbolic_machine);
    let analyzed: Analyzed<P> = analyze_ast(pilfile).expect("Failed to analyze AST");
    let optimized = optimize(analyzed);

    let mut powdr_exprs = Vec::new();
    let mut powdr_bus_interactions = Vec::new();
    for id in optimized.identities.iter() {
        match id {
            Identity::Polynomial(PolynomialIdentity { expression, .. }) => {
                powdr_exprs.push(SymbolicConstraint {
                    expr: expression.clone(),
                });
            }
            Identity::BusInteraction(BusInteractionIdentity {
                multiplicity,
                bus_id,
                payload,
                ..
            }) => {
                let id = if let AlgebraicExpression::<P>::Number(num) = bus_id {
                    num.to_integer().try_into_u64().unwrap()
                } else {
                    panic!("Bus ID must be a Number");
                };

                // This assumes that multiplicity is a Number or a Unary
                // Operation with UnaryOperator::Minus
                let kind = if let AlgebraicExpression::<P>::Number(_) = multiplicity {
                    BusInteractionKind::Send
                } else if let AlgebraicExpression::<P>::UnaryOperation(_) = multiplicity {
                    BusInteractionKind::Receive
                } else {
                    BusInteractionKind::Send
                    //panic!("Multiplicity must be a Number or a Unary Operation");
                };

                let interaction = SymbolicBusInteraction {
                    kind,
                    id,
                    mult: multiplicity.clone(),
                    args: payload.0.clone(),
                };
                powdr_bus_interactions.push(interaction);
            }
            _ => continue,
        }
    }
    let machine = SymbolicMachine {
        constraints: powdr_exprs,
        bus_interactions: powdr_bus_interactions,
    };

    println!(
        "After powdr optimizations, columns: {}, constraints: {}, bus_interactions: {}",
        machine.columns().len(),
        machine.constraints.len(),
        machine.bus_interactions.len()
    );

    machine
}

/// Create a PILFile from a SymbolicMachine
fn symbolic_machine_to_pilfile<P: FieldElement>(symbolic_machine: SymbolicMachine<P>) -> PILFile {
    let mut program = Vec::new();

    // Collect all unique polynomial references from constraints and bus interactions
    let refs: HashSet<AlgebraicExpression<_>> = symbolic_machine
        .constraints
        .iter()
        .flat_map(|constraint| collect_cols_algebraic(&constraint.expr))
        .chain(
            symbolic_machine
                .bus_interactions
                .iter()
                .flat_map(|interaction| {
                    interaction
                        .args
                        .iter()
                        .chain(std::iter::once(&interaction.mult))
                        .flat_map(|expr| collect_cols_algebraic(expr))
                }),
        )
        .collect::<HashSet<_>>();

    // Add PolynomialCommitDeclaration for all referenced columns into the program
    for algebraic_ref in &refs {
        if let AlgebraicExpression::Reference(ref r) = algebraic_ref {
            program.push(PilStatement::PolynomialCommitDeclaration(
                SourceRef::unknown(),
                None,
                vec![r.name.clone().into()],
                None,
            ));
        }
    }

    // Add Expressions for all constraints
    for constraint in &symbolic_machine.constraints {
        let zero = BigUint::from(0u32).into();
        let constraint = Expression::new_binary(
            algebraic_to_expression::<P>(&constraint.expr),
            BinaryOperator::Identity,
            zero,
        );
        program.push(PilStatement::Expression(SourceRef::unknown(), constraint));
    }

    // Add Expressions for all bus interactions
    for interaction in &symbolic_machine.bus_interactions {
        let mult_expr = algebraic_to_expression::<P>(&interaction.mult);
        let bus_id_expr = Expression::Number(
            SourceRef::unknown(),
            Number {
                value: BigUint::from(interaction.id),
                type_: None,
            },
        );

        let payload_array = Expression::ArrayLiteral(
            SourceRef::unknown(),
            ArrayLiteral {
                items: interaction
                    .args
                    .iter()
                    .map(|arg| algebraic_to_expression::<P>(arg))
                    .collect(),
            },
        );

        let latch_expr = BigUint::from(1u32).into();

        let path = ["prelude", "Constr", "BusInteraction"]
            .iter()
            .map(|s| SymbolPath::from_str(s).unwrap())
            .fold(SymbolPath::from_str("std").unwrap(), |acc, sym| {
                acc.join(sym)
            });
        let bus_interaction_expr = Expression::FunctionCall(
            SourceRef::unknown(),
            FunctionCall {
                function: Box::new(Expression::Reference(
                    SourceRef::unknown(),
                    NamespacedPolynomialReference::from(path),
                )),
                arguments: vec![mult_expr, bus_id_expr, payload_array, latch_expr],
            },
        );

        program.push(PilStatement::Expression(
            SourceRef::unknown(),
            bus_interaction_expr,
        ));
    }

    PILFile(program)
}

/// Transforms an AlgebraicExpression from the analyzed AST to the corresponding Expression in the parsed AST.
fn algebraic_to_expression<P: FieldElement>(expr: &AlgebraicExpression<P>) -> Expression {
    let dummy_src = SourceRef::unknown();

    match expr {
        AlgebraicExpression::Reference(reference) => {
            let parsed_ref = NamespacedPolynomialReference::from_identifier(reference.name.clone());
            Expression::Reference(dummy_src, parsed_ref)
        }
        AlgebraicExpression::PublicReference(name) => {
            Expression::PublicReference(dummy_src, name.to_string())
        }
        AlgebraicExpression::Number(value) => Expression::Number(
            dummy_src,
            Number {
                value: BigUint::from(value.to_integer().try_into_u32().unwrap()),
                type_: None,
            },
        ),
        AlgebraicExpression::BinaryOperation(bin_op) => {
            let parsed_operator = match bin_op.op {
                AlgebraicBinaryOperator::Add => BinaryOperator::Add,
                AlgebraicBinaryOperator::Sub => BinaryOperator::Sub,
                AlgebraicBinaryOperator::Mul => BinaryOperator::Mul,
                AlgebraicBinaryOperator::Pow => BinaryOperator::Pow,
            };

            let left = Box::new(algebraic_to_expression::<P>(&bin_op.left));
            let right = Box::new(algebraic_to_expression::<P>(&bin_op.right));

            Expression::BinaryOperation(
                dummy_src,
                BinaryOperation {
                    op: parsed_operator,
                    left,
                    right,
                },
            )
        }
        AlgebraicExpression::UnaryOperation(un_op) => {
            let op = match un_op.op {
                AlgebraicUnaryOperator::Minus => UnaryOperator::Minus,
            };

            let expr = Box::new(algebraic_to_expression::<P>(&un_op.expr));

            Expression::UnaryOperation(dummy_src, UnaryOperation { op, expr })
        }
        AlgebraicExpression::Challenge(_) => unreachable!(),
    }
}

fn is_loadstore(opcode: usize) -> bool {
    opcode >= 0x210 && opcode <= 0x215
}

fn loadstore_chip_info<T: FieldElement>(
    machine: &SymbolicMachine<T>,
    //pc_int: &PcLookupBusInteraction<T>,
    opcode: usize,
) -> BTreeMap<String, AlgebraicExpression<T>> {
    let is_load = if opcode == 0x210 || opcode == 0x211 || opcode == 0x212 {
        T::from(1u32)
    } else {
        T::from(0u32)
    };
    let is_load = AlgebraicExpression::Number(is_load);
    let is_load_expr = match &machine.constraints[7].expr {
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation { left, op, right }) => {
            left.clone()
        }
        _ => panic!("Expected subtraction."),
    };
    let cols = powdr::collect_cols_names_algebraic(&is_load_expr);
    assert_eq!(cols.len(), 1);
    let is_load_col = cols.into_iter().next().unwrap();

    [(is_load_col, is_load)].into()
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
        let (new_program, new_machines, _) = autoprecompiles.run();
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
    }
}
