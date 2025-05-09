use itertools::Itertools;
use optimizer::{optimize, ConcreteBusInteractionHandler};
use powdr_ast::analyzed::{PolyID, PolynomialType};
use powdr_ast::{
    analyzed::{
        AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression, AlgebraicReference,
        AlgebraicUnaryOperator,
    },
    parsed::visitor::AllChildren,
};
use powdr_constraint_solver::constraint_system::BusInteractionHandler;
use powdr_executor::witgen::evaluators::symbolic_evaluator::SymbolicEvaluator;
use powdr_executor::witgen::{AlgebraicVariable, PartialExpressionEvaluator};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Display;

use powdr_number::{FieldElement, LargeInt};
use powdr_pilopt::simplify_expression;

pub mod optimizer;
pub mod powdr;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SymbolicInstructionStatement<T> {
    pub name: String,
    pub opcode: usize,
    pub args: Vec<T>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SymbolicConstraint<T> {
    pub expr: AlgebraicExpression<T>,
}

impl<T: Display> Display for SymbolicConstraint<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.expr)
    }
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

impl<T: Display> Display for SymbolicBusInteraction<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "(id={}, mult={}, args=[{}])",
            self.id,
            self.mult,
            self.args.iter().join(", ")
        )
    }
}

impl<T: Clone + Ord + std::fmt::Display> SymbolicBusInteraction<T> {
    pub fn columns(&self) -> BTreeSet<String> {
        let mut cols = BTreeSet::new();
        cols.extend(powdr::collect_cols_names_algebraic(&self.mult));
        for a in &self.args {
            cols.extend(powdr::collect_cols_names_algebraic(a));
        }
        cols
    }

    pub fn column_ids(&self) -> BTreeSet<u64> {
        let mut cols = BTreeSet::new();
        cols.extend(powdr::collect_cols_ids_algebraic(&self.mult));
        for a in &self.args {
            cols.extend(powdr::collect_cols_ids_algebraic(a));
        }
        cols
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum BusInteractionKind {
    Send,
    Receive,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SymbolicMachine<T> {
    pub constraints: Vec<SymbolicConstraint<T>>,
    pub bus_interactions: Vec<SymbolicBusInteraction<T>>,
}

impl<T: Display> Display for SymbolicMachine<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for constraint in &self.constraints {
            writeln!(f, "{constraint}")?;
        }
        for bus_interaction in &self.bus_interactions {
            writeln!(f, "{bus_interaction}")?;
        }
        Ok(())
    }
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

    pub fn constraint_columns(&self) -> BTreeSet<String> {
        let mut cols = BTreeSet::new();
        for c in &self.constraints {
            cols.extend(c.columns());
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
    pub instruction_machines: BTreeMap<String, SymbolicMachine<T>>,
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

impl<T: FieldElement> TryFrom<SymbolicBusInteraction<T>> for MemoryBusInteraction<T> {
    type Error = ();

    fn try_from(bus_interaction: SymbolicBusInteraction<T>) -> Result<Self, ()> {
        (bus_interaction.id == MEMORY_BUS_ID)
            .then(|| {
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
            })
            .ok_or(())
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct PcLookupBusInteraction<T> {
    pub from_pc: AlgebraicExpression<T>,
    pub op: AlgebraicExpression<T>,
    pub args: Vec<AlgebraicExpression<T>>,
    pub bus_interaction: SymbolicBusInteraction<T>,
}

impl<T: FieldElement> TryFrom<SymbolicBusInteraction<T>> for PcLookupBusInteraction<T> {
    type Error = ();

    fn try_from(bus_interaction: SymbolicBusInteraction<T>) -> Result<Self, ()> {
        (bus_interaction.id == PC_LOOKUP_BUS_ID)
            .then(|| {
                let from_pc = bus_interaction.args[0].clone();
                let op = bus_interaction.args[1].clone();
                let args = bus_interaction.args[2..].to_vec();
                PcLookupBusInteraction {
                    from_pc,
                    op,
                    args,
                    bus_interaction,
                }
            })
            .ok_or(())
    }
}

pub enum VMBusInteraction<T> {
    Memory(MemoryBusInteraction<T>),
}

const EXECUTION_BUS_ID: u64 = 0;
const MEMORY_BUS_ID: u64 = 1;
const PC_LOOKUP_BUS_ID: u64 = 2;
const RANGE_CHECK_BUS_ID: u64 = 3;

impl<T: FieldElement> Autoprecompiles<T> {
    pub fn build(
        &self,
        bus_interaction_handler: impl BusInteractionHandler<T>
            + ConcreteBusInteractionHandler<T>
            + 'static
            + Clone,
        degree_bound: usize,
    ) -> (SymbolicMachine<T>, Vec<BTreeMap<String, String>>) {
        let (machine, subs) = generate_precompile(
            &self.program,
            &self.instruction_kind,
            &self.instruction_machines,
        );

        let c = machine.columns();
        let i = machine.column_ids();
        assert_eq!(c.len(), i.len());
        assert_eq!(machine.columns().len(), machine.column_ids().len());
        let machine = optimize_pc_lookup(machine);
        let machine = optimize_exec_bus(machine);
        let machine = optimize_precompile(machine);
        let machine = optimize(machine, bus_interaction_handler, degree_bound);
        let machine = remove_zero_mult(machine);
        let machine = remove_zero_constraint(machine);

        // add guards to constraints that are not satisfied by zeroes
        let machine = add_guards(machine);

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

// TODO: This should probably be done by pilopt
pub fn remove_zero_mult<T: FieldElement>(mut machine: SymbolicMachine<T>) -> SymbolicMachine<T> {
    machine
        .bus_interactions
        .retain(|bus_int| !powdr::is_zero(&bus_int.mult));

    machine
}

/// Adds an `is_valid` guard to all constraints and bus interactions.
/// Assumptions:
/// - There are exactly one execution bus receive and one execution bus send, in this order.
/// - There is exactly one program bus send.
pub fn add_guards<T: FieldElement>(mut machine: SymbolicMachine<T>) -> SymbolicMachine<T> {
    let max_id = machine.column_ids().iter().max().unwrap() + 1;

    let is_valid = AlgebraicExpression::Reference(AlgebraicReference {
        name: "is_valid".to_string(),
        poly_id: PolyID {
            ptype: PolynomialType::Committed,
            id: max_id,
        },
        next: false,
    });

    for c in &mut machine.constraints {
        let mut zeroed_expr = c.expr.clone();
        powdr::make_refs_zero(&mut zeroed_expr);
        let zeroed_expr = simplify_expression(zeroed_expr);
        if !powdr::is_zero(&zeroed_expr) {
            c.expr = is_valid.clone() * c.expr.clone();
            // TODO this would not have to be cloned if we had *=
            //c.expr *= guard.clone();
        }
    }

    let [execution_bus_receive, execution_bus_send] = machine
        .bus_interactions
        .iter_mut()
        .filter_map(|bus_int| match bus_int.id {
            EXECUTION_BUS_ID => Some(bus_int),
            _ => None,
        })
        .collect::<Vec<_>>()
        .try_into()
        .unwrap();

    execution_bus_receive.mult =
        AlgebraicExpression::new_unary(AlgebraicUnaryOperator::Minus, is_valid.clone());
    execution_bus_send.mult = is_valid.clone();

    let [program_bus_send] = machine
        .bus_interactions
        .iter_mut()
        .filter_map(|bus_int| match bus_int.id {
            PC_LOOKUP_BUS_ID => Some(bus_int),
            _ => None,
        })
        .collect::<Vec<_>>()
        .try_into()
        .unwrap();
    program_bus_send.mult = is_valid.clone();

    let mut is_valid_mults: Vec<SymbolicConstraint<T>> = Vec::new();
    for b in &mut machine.bus_interactions {
        match b.id {
            EXECUTION_BUS_ID => {
                // already handled
            }
            PC_LOOKUP_BUS_ID => {
                // already handled
            }
            _ => {
                // We check the value of the multiplicity when all variables are set to zero
                let mut zeroed_expr = b.mult.clone();
                powdr::make_refs_zero(&mut zeroed_expr);
                let zeroed_expr = simplify_expression(zeroed_expr);
                if !powdr::is_zero(&zeroed_expr) {
                    // if it's not zero, then we guard the multiplicity by `is_valid`
                    b.mult = is_valid.clone() * b.mult.clone();
                    // TODO this would not have to be cloned if we had *=
                    //c.expr *= guard.clone();
                } else {
                    // if it's zero, then we do not have to change the multiplicity, but we need to force it to be zero on non-valid rows with a constraint
                    let one = AlgebraicExpression::Number(1u64.into());
                    let e = ((one - is_valid.clone()) * b.mult.clone()).into();
                    is_valid_mults.push(e);
                }
            }
        }
    }

    machine.constraints.extend(is_valid_mults);

    machine
}

// TODO: This should probably be done by pilopt
pub fn remove_zero_constraint<T: FieldElement>(
    mut machine: SymbolicMachine<T>,
) -> SymbolicMachine<T> {
    machine.constraints.retain(|c| !powdr::is_zero(&c.expr));
    machine
}

pub fn remove_range_checks<T: FieldElement>(mut machine: SymbolicMachine<T>) -> SymbolicMachine<T> {
    let cols = machine.constraint_columns();

    machine.bus_interactions.retain(|bus_int| {
        if bus_int.id != RANGE_CHECK_BUS_ID {
            return true;
        }

        bus_int.args.iter().any(|a| {
            let a_cols = powdr::collect_cols_names_algebraic(a);
            a_cols.iter().any(|c| cols.contains(c))
        })
    });

    machine
}

pub fn exec_receive<T: FieldElement>(machine: &SymbolicMachine<T>) -> SymbolicBusInteraction<T> {
    let [r, _s] = machine
        .bus_interactions
        .iter()
        .filter_map(|bus_int| match bus_int.id {
            EXECUTION_BUS_ID => Some(bus_int.clone()),
            _ => None,
        })
        .collect::<Vec<_>>()
        .try_into()
        .unwrap();
    // TODO assert that r.mult matches -expr
    r
}

pub fn optimize_precompile<T: FieldElement>(mut machine: SymbolicMachine<T>) -> SymbolicMachine<T> {
    let mut receive = true;
    let mut local_reg_mem: BTreeMap<u32, Vec<AlgebraicExpression<T>>> = BTreeMap::new();
    let mut new_constraints: Vec<SymbolicConstraint<T>> = Vec::new();
    let mut prev_tss: Vec<AlgebraicExpression<T>> = Vec::new();
    let mut to_remove: BTreeSet<usize> = Default::default();
    let mut last_store: BTreeMap<u32, usize> = BTreeMap::new();
    machine
        .bus_interactions
        .iter()
        .enumerate()
        .for_each(|(i, bus_int)| {
            let mem_int: MemoryBusInteraction<T> = match bus_int.clone().try_into() {
                Ok(mem_int) => mem_int,
                Err(_) => {
                    return;
                }
            };

            if matches!(mem_int.ty, MemoryType::Constant | MemoryType::Memory) {
                return;
            }

            let addr = match mem_int.try_addr_u32() {
                None => {
                    panic!("Register memory access must have constant address");
                }
                Some(addr) => addr,
            };

            if receive {
                match local_reg_mem.get(&addr) {
                    Some(data) => {
                        assert_eq!(data.len(), mem_int.data.len());
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
                                new_constraints.push(eq_expr.into());
                            });

                        // If this receive's ts is a prev_ts, we can remove the constraint that
                        // decomposes this prev_ts and range checks on the limbs.
                        let prev_ts = mem_int.bus_interaction.args[6].clone();
                        assert!(powdr::is_ref(&prev_ts));
                        prev_tss.push(prev_ts);
                        to_remove.insert(i);
                    }
                    None => {
                        local_reg_mem.insert(addr, mem_int.data.clone());
                    }
                }
            } else {
                last_store.insert(addr, i);
                local_reg_mem.insert(addr, mem_int.data.clone());
            }

            receive = !receive;
        });

    let mut receive = true;
    machine.bus_interactions = machine
        .bus_interactions
        .into_iter()
        .enumerate()
        .filter_map(|(i, bus_int)| {
            let mem_int: MemoryBusInteraction<T> = match bus_int.clone().try_into() {
                Ok(mem_int) => mem_int,
                Err(_) => {
                    return Some(bus_int);
                }
            };

            if matches!(mem_int.ty, MemoryType::Constant | MemoryType::Memory) {
                return Some(bus_int);
            }

            let addr = match mem_int.try_addr_u32() {
                None => {
                    panic!("Register memory access must have constant address");
                }
                Some(addr) => addr,
            };

            let keep = if receive && !to_remove.contains(&i) {
                Some(bus_int)
            } else if receive && to_remove.contains(&i) {
                None
            } else if last_store
                .get(&addr)
                .is_some_and(|&last_index| last_index == i)
            {
                Some(bus_int)
            } else {
                None
            };

            receive = !receive;

            keep
        })
        .collect();

    let mut to_remove: BTreeSet<AlgebraicExpression<T>> = Default::default();
    machine.constraints.retain(|c| {
        for prev_ts in &prev_tss {
            if powdr::has_ref(&c.expr, prev_ts) {
                let (col1, col2) = powdr::find_byte_decomp(&c.expr);
                to_remove.insert(col1);
                to_remove.insert(col2);
                return false;
            }
        }

        true
    });

    machine.bus_interactions.retain(|bus_int| {
        if bus_int.id != RANGE_CHECK_BUS_ID {
            return true;
        }

        assert_eq!(bus_int.args.len(), 2);

        let col = bus_int.args[0].clone();
        if to_remove.contains(&col) {
            return false;
        }

        true
    });

    machine.constraints.extend(new_constraints);

    machine
}

pub fn optimize_pc_lookup<T: FieldElement>(mut machine: SymbolicMachine<T>) -> SymbolicMachine<T> {
    let mut first_pc = None;
    machine.bus_interactions.retain(|bus_int| {
        if bus_int.id == PC_LOOKUP_BUS_ID {
            if first_pc.is_none() {
                first_pc = Some(bus_int.clone());
            }
            return false;
        }
        true
    });
    let mut first_pc = first_pc.unwrap();
    assert_eq!(first_pc.args.len(), 9);
    first_pc.args[1] = AlgebraicExpression::Number(T::from(4351u32));
    first_pc.args[2] = AlgebraicExpression::Number(T::from(0u32));
    first_pc.args[3] = AlgebraicExpression::Number(T::from(0u32));
    first_pc.args[4] = AlgebraicExpression::Number(T::from(0u32));
    first_pc.args[5] = AlgebraicExpression::Number(T::from(0u32));
    first_pc.args[6] = AlgebraicExpression::Number(T::from(0u32));
    first_pc.args[7] = AlgebraicExpression::Number(T::from(0u32));
    first_pc.args[8] = AlgebraicExpression::Number(T::from(0u32));

    machine.bus_interactions.push(first_pc);

    machine
}

pub fn optimize_exec_bus<T: FieldElement>(mut machine: SymbolicMachine<T>) -> SymbolicMachine<T> {
    let mut first_seen = false;
    let mut receive = true;
    let mut latest_send = None;
    let mut subs_pc: BTreeMap<AlgebraicExpression<T>, AlgebraicExpression<T>> = Default::default();
    let mut subs_ts: BTreeMap<AlgebraicExpression<T>, AlgebraicExpression<T>> = Default::default();
    machine.bus_interactions.retain(|bus_int| {
        if bus_int.id != EXECUTION_BUS_ID {
            return true;
        }

        if receive {
            // TODO assert that mult matches -expr
        }

        // Keep the first receive
        let keep = if !first_seen {
            first_seen = true;
            true
        } else if !receive {
            // Save the latest send and remove the bus interaction
            let mut pc_expr = bus_int.args[0].clone();
            powdr::substitute_algebraic_algebraic(&mut pc_expr, &subs_pc);
            pc_expr = simplify_expression(pc_expr);

            let mut ts_expr = bus_int.args[1].clone();
            powdr::substitute_algebraic_algebraic(&mut ts_expr, &subs_ts);
            ts_expr = simplify_expression(ts_expr);

            let mut send = bus_int.clone();
            send.args[0] = pc_expr;
            send.args[1] = ts_expr;

            latest_send = Some(send);
            false
        } else {
            // Equate the latest send to the new receive and remove the bus interaction
            subs_pc.insert(
                bus_int.args[0].clone(),
                latest_send.clone().unwrap().args[0].clone(),
            );
            subs_ts.insert(
                bus_int.args[1].clone(),
                latest_send.clone().unwrap().args[1].clone(),
            );
            false
        };

        receive = !receive;

        keep
    });

    // Re-add the last send
    machine.bus_interactions.push(latest_send.unwrap());

    for c in &mut machine.constraints {
        powdr::substitute_algebraic_algebraic(&mut c.expr, &subs_pc);
        powdr::substitute_algebraic_algebraic(&mut c.expr, &subs_ts);
        c.expr = simplify_expression(c.expr.clone());
    }
    for b in &mut machine.bus_interactions {
        powdr::substitute_algebraic_algebraic(&mut b.mult, &subs_pc);
        powdr::substitute_algebraic_algebraic(&mut b.mult, &subs_ts);
        b.mult = simplify_expression(b.mult.clone());
        for a in &mut b.args {
            powdr::substitute_algebraic_algebraic(a, &subs_pc);
            powdr::substitute_algebraic_algebraic(a, &subs_ts);
            *a = simplify_expression(a.clone());
        }
    }

    machine
}

pub fn generate_precompile<T: FieldElement>(
    statements: &[SymbolicInstructionStatement<T>],
    instruction_kinds: &BTreeMap<String, InstructionKind>,
    instruction_machines: &BTreeMap<String, SymbolicMachine<T>>,
) -> (SymbolicMachine<T>, Vec<BTreeMap<String, String>>) {
    let mut constraints: Vec<SymbolicConstraint<T>> = Vec::new();
    let mut bus_interactions: Vec<SymbolicBusInteraction<T>> = Vec::new();
    let mut col_subs: Vec<BTreeMap<String, String>> = Vec::new();
    let mut global_idx: usize = 3;
    let mut global_idx_subs: BTreeMap<String, usize> = BTreeMap::new();
    let mut global_idx_subs_rev: BTreeMap<usize, String> = BTreeMap::new();

    for (i, instr) in statements.iter().enumerate() {
        match instruction_kinds.get(&instr.name).unwrap() {
            InstructionKind::Normal
            | InstructionKind::UnconditionalBranch
            | InstructionKind::ConditionalBranch => {
                let machine = instruction_machines.get(&instr.name).unwrap().clone();

                let pc_lookup: PcLookupBusInteraction<T> = machine
                    .bus_interactions
                    .iter()
                    .filter_map(|bus_int| bus_int.clone().try_into().ok())
                    .exactly_one()
                    .expect("Expected single pc lookup");

                let mut sub_map: BTreeMap<String, AlgebraicExpression<T>> = Default::default();
                let mut local_constraints: Vec<SymbolicConstraint<T>> = Vec::new();

                let is_valid: AlgebraicExpression<T> = exec_receive(&machine).mult.clone();
                let one = AlgebraicExpression::Number(1u64.into());
                local_constraints.push((is_valid.clone() + one).into());

                let mut sub_map_loadstore: BTreeMap<String, AlgebraicExpression<T>> =
                    Default::default();
                if is_loadstore(instr.opcode) {
                    sub_map_loadstore.extend(loadstore_chip_info(&machine, instr.opcode));
                }

                add_opcode_constraints(&mut local_constraints, instr.opcode, &pc_lookup.op);

                assert_eq!(instr.args.len(), pc_lookup.args.len());
                instr
                    .args
                    .iter()
                    .zip(pc_lookup.args.iter())
                    .for_each(|(instr_arg, pc_arg)| {
                        let arg = AlgebraicExpression::Number(*instr_arg);
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

                let mut local_subs = BTreeMap::new();
                let local_identities = machine
                    .constraints
                    .iter()
                    .chain(local_constraints.iter())
                    .map(|expr| {
                        let mut expr = expr.expr.clone();
                        powdr::substitute_algebraic(&mut expr, &sub_map);
                        powdr::substitute_algebraic(&mut expr, &sub_map_loadstore);
                        let subs = powdr::append_suffix_algebraic(&mut expr, &format!("{i}"));
                        expr = simplify_expression(expr);
                        global_idx = powdr::reassign_ids_algebraic(
                            &mut expr,
                            global_idx,
                            &mut global_idx_subs,
                            &mut global_idx_subs_rev,
                        );
                        local_subs.extend(subs);
                        SymbolicConstraint { expr }
                    })
                    .collect::<Vec<_>>();

                constraints.extend(local_identities);

                for bus_int in &machine.bus_interactions {
                    let mut link = bus_int.clone();
                    link.args
                        .iter_mut()
                        .chain(std::iter::once(&mut link.mult))
                        .for_each(|e| {
                            powdr::substitute_algebraic(e, &sub_map);
                            powdr::substitute_algebraic(e, &sub_map_loadstore);
                            *e = simplify_expression(e.clone());
                            let subs = powdr::append_suffix_algebraic(e, &format!("{i}"));
                            global_idx = powdr::reassign_ids_algebraic(
                                &mut *e,
                                global_idx,
                                &mut global_idx_subs,
                                &mut global_idx_subs_rev,
                            );
                            local_subs.extend(subs);
                        });
                    bus_interactions.push(link);
                }

                col_subs.push(local_subs);

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
                                *arg = instr.args[0].into();
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

    (
        SymbolicMachine {
            constraints,
            bus_interactions,
        },
        col_subs,
    )
}

fn add_opcode_constraints<T: FieldElement>(
    constraints: &mut Vec<SymbolicConstraint<T>>,
    opcode: usize,
    expected_opcode: &AlgebraicExpression<T>,
) {
    let opcode_a = AlgebraicExpression::Number((opcode as u64).into());
    match try_compute_opcode_map(expected_opcode) {
        Ok(opcode_to_flag) => {
            let active_flag = opcode_to_flag.get(&(opcode as u64)).unwrap();
            for flag_ref in opcode_to_flag.values() {
                let expected_value = if flag_ref == active_flag {
                    AlgebraicExpression::Number(1u32.into())
                } else {
                    AlgebraicExpression::Number(0u32.into())
                };
                let flag_expr = AlgebraicExpression::Reference(flag_ref.clone());
                let constraint = flag_expr - expected_value;
                constraints.push(constraint.into());
            }
        }
        Err(_) => {
            if try_set_loadstore_flags(constraints, opcode, expected_opcode).is_err() {
                // We were not able to extract the flags, so we keep them as witness columns
                // and add a constraint that the expected opcode needs to equal the compile-time opcode.
                constraints.push((expected_opcode.clone() - opcode_a).into());
            }
        }
    }
}

// See: https://github.com/openvm-org/openvm/blob/51f07d50d20174b23091f48e25d9ea421b4e2787/extensions/rv32im/transpiler/src/instructions.rs#L100-L113
const LOADW_OPCODE: usize = 0x210;
const STOREW_OPCODE: usize = 0x210 + 3;

fn try_set_loadstore_flags<T: FieldElement>(
    constraints: &mut Vec<SymbolicConstraint<T>>,
    opcode: usize,
    expected_opcode: &AlgebraicExpression<T>,
) -> Result<(), ()> {
    if opcode != LOADW_OPCODE && opcode != STOREW_OPCODE {
        // For other instructions, the flags are not unique :/
        return Err(());
    }
    // Find the 4 flags, sorted by name.
    let flag_refs = expected_opcode
        .all_children()
        .filter_map(|expr| match expr {
            AlgebraicExpression::Reference(column_reference) => {
                Some((column_reference.name.clone(), column_reference))
            }
            _ => None,
        })
        .sorted_by(|(n1, _), (n2, _)| n1.cmp(n2))
        .unique_by(|(name, _)| name.clone())
        .map(|(_, column_reference)| column_reference)
        .collect::<Vec<_>>();
    assert_eq!(flag_refs.len(), 4);

    let mut set_flag = |column_ref: &AlgebraicReference, value: u64| {
        let value = AlgebraicExpression::Number(value.into());
        let flag_expr = AlgebraicExpression::Reference(column_ref.clone());
        let constraint = flag_expr - value;
        constraints.push(constraint.into());
    };

    let mut set_flags = |f1, f2, f3, f4| {
        set_flag(flag_refs[0], f1);
        set_flag(flag_refs[1], f2);
        set_flag(flag_refs[2], f3);
        set_flag(flag_refs[3], f4);
    };

    // See: https://github.com/openvm-org/openvm/blob/51f07d50d20174b23091f48e25d9ea421b4e2787/extensions/rv32im/circuit/src/loadstore/core.rs#L311-L330
    match opcode {
        LOADW_OPCODE => set_flags(2, 0, 0, 0),
        STOREW_OPCODE => set_flags(0, 0, 0, 1),
        _ => panic!(),
    }

    Ok(())
}

fn try_compute_opcode_map<T: FieldElement>(
    expected_opcode: &AlgebraicExpression<T>,
) -> Result<BTreeMap<u64, AlgebraicReference>, ()> {
    // Parse the expected opcode as an algebraic expression:
    // flag1 * c1 + flag2 * c2 + ... + offset
    let imm = BTreeMap::new();
    let affine_expression = PartialExpressionEvaluator::new(SymbolicEvaluator, &imm)
        .evaluate(expected_opcode)
        .map_err(|_| ())?;

    // The above would not include any entry for `flag * 0` or `0 * flag`.
    // If it exists, we collect it here.
    let zero = AlgebraicExpression::Number(0u32.into());
    let zero_flag = expected_opcode
        .all_children()
        .find_map(|expr| match expr {
            AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
                op: AlgebraicBinaryOperator::Mul,
                left,
                right,
            }) => {
                if **left == zero {
                    Some((**right).clone())
                } else if **right == zero {
                    Some((**left).clone())
                } else {
                    None
                }
            }
            _ => None,
        })
        .and_then(|expr| match expr {
            AlgebraicExpression::Reference(reference) => Some(reference),
            _ => None,
        });

    let offset = affine_expression.offset();

    if !offset.is_zero() && zero_flag.is_none() {
        // We didn't find any flag with factor zero, and the offset is not zero.
        // Probably something went wrong.
        return Err(());
    }

    Ok(affine_expression
        .nonzero_coefficients()
        .map(|(k, factor)| {
            let opcode = (offset + *factor).to_degree();
            let flag = match k {
                AlgebraicVariable::Column(column) => (*column).clone(),
                AlgebraicVariable::Public(_) => unreachable!(),
            };
            (opcode, flag)
        })
        .chain(zero_flag.map(|flag| (offset.to_degree(), flag.clone())))
        .collect())
}

fn is_loadstore(opcode: usize) -> bool {
    (0x210..=0x215).contains(&opcode)
}

fn loadstore_chip_info<T: FieldElement>(
    machine: &SymbolicMachine<T>,
    opcode: usize,
) -> BTreeMap<String, AlgebraicExpression<T>> {
    let is_load = if opcode == 0x210 || opcode == 0x211 || opcode == 0x212 {
        T::from(1u32)
    } else {
        T::from(0u32)
    };
    let is_load = AlgebraicExpression::Number(is_load);
    let is_load_expr = match &machine.constraints[7].expr {
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation { left, .. }) => left.clone(),
        _ => panic!("Expected subtraction."),
    };
    let cols = powdr::collect_cols_names_algebraic(&is_load_expr);
    assert_eq!(cols.len(), 1);
    let is_load_col = cols.into_iter().next().unwrap();

    [(is_load_col, is_load)].into()
}
