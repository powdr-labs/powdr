use constraint_optimizer::IsBusStateful;
use itertools::Itertools;
use powdr::{Column, UniqueColumns};
use powdr_ast::analyzed::{
    AlgebraicBinaryOperation, AlgebraicExpression, AlgebraicReference, AlgebraicUnaryOperator,
};
use powdr_ast::analyzed::{PolyID, PolynomialType};
use powdr_ast::parsed::visitor::Children;
use powdr_constraint_solver::constraint_system::BusInteractionHandler;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::fmt::Display;
use std::iter::once;

use powdr_number::{FieldElement, LargeInt};
use powdr_pilopt::simplify_expression;

pub mod constraint_optimizer;
pub mod optimizer;
pub mod powdr;
pub mod register_optimizer;

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

impl<T: Clone + Ord + std::fmt::Display> Children<AlgebraicExpression<T>>
    for SymbolicConstraint<T>
{
    fn children(&self) -> Box<dyn Iterator<Item = &AlgebraicExpression<T>> + '_> {
        Box::new(once(&self.expr))
    }

    fn children_mut(&mut self) -> Box<dyn Iterator<Item = &mut AlgebraicExpression<T>> + '_> {
        Box::new(once(&mut self.expr))
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

impl<T: Clone + Ord + std::fmt::Display> Children<AlgebraicExpression<T>>
    for SymbolicBusInteraction<T>
{
    fn children(&self) -> Box<dyn Iterator<Item = &AlgebraicExpression<T>> + '_> {
        Box::new(once(&self.mult).chain(&self.args))
    }

    fn children_mut(&mut self) -> Box<dyn Iterator<Item = &mut AlgebraicExpression<T>> + '_> {
        Box::new(once(&mut self.mult).chain(&mut self.args))
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

impl<T: Clone + Ord + std::fmt::Display> Children<AlgebraicExpression<T>> for SymbolicMachine<T> {
    fn children(&self) -> Box<dyn Iterator<Item = &AlgebraicExpression<T>> + '_> {
        Box::new(
            self.constraints
                .iter()
                .flat_map(|c| c.children())
                .chain(self.bus_interactions.iter().flat_map(|i| i.children())),
        )
    }

    fn children_mut(&mut self) -> Box<dyn Iterator<Item = &mut AlgebraicExpression<T>> + '_> {
        Box::new(
            self.constraints
                .iter_mut()
                .flat_map(|c| c.children_mut())
                .chain(
                    self.bus_interactions
                        .iter_mut()
                        .flat_map(|i| i.children_mut()),
                ),
        )
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
    Native,
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
                    3 => MemoryType::Native,
                    _ => unreachable!("Expected 0, 1, 2 or 3 but got {n}"),
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
            MemoryType::Native => AlgebraicExpression::Number(T::from(3u32)),
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
                // TODO: Timestamp is ignored, we could use it to assert that the bus interactions
                // are in the right order.
                let ty = bus_interaction.args[0].clone().into();
                let op = bus_interaction.kind.clone().into();
                let addr = bus_interaction.args[1].clone();
                let data = bus_interaction.args[2..bus_interaction.args.len() - 1].to_vec();
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

impl<T: FieldElement> Autoprecompiles<T> {
    pub fn build(
        &self,
        bus_interaction_handler: impl BusInteractionHandler<T> + IsBusStateful<T> + 'static + Clone,
        degree_bound: usize,
        opcode: u32,
    ) -> (SymbolicMachine<T>, Vec<Vec<u64>>) {
        let (machine, subs) = generate_precompile(
            &self.program,
            &self.instruction_kind,
            &self.instruction_machines,
        );

        let machine = optimizer::optimize(machine, bus_interaction_handler, opcode, degree_bound);

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

/// Adds an `is_valid` guard to all constraints and bus interactions.
/// Assumptions:
/// - There are exactly one execution bus receive and one execution bus send, in this order.
/// - There is exactly one program bus send.
pub fn add_guards<T: FieldElement>(mut machine: SymbolicMachine<T>) -> SymbolicMachine<T> {
    let max_id = machine
        .unique_columns()
        .map(|c| {
            assert_eq!(c.id.ptype, PolynomialType::Committed);
            c.id.id
        })
        .max()
        .unwrap()
        + 1;

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

pub fn generate_precompile<T: FieldElement>(
    statements: &[SymbolicInstructionStatement<T>],
    instruction_kinds: &BTreeMap<String, InstructionKind>,
    instruction_machines: &BTreeMap<String, SymbolicMachine<T>>,
) -> (SymbolicMachine<T>, Vec<Vec<u64>>) {
    let mut constraints: Vec<SymbolicConstraint<T>> = Vec::new();
    let mut bus_interactions: Vec<SymbolicBusInteraction<T>> = Vec::new();
    let mut col_subs: Vec<Vec<u64>> = Vec::new();
    let mut global_idx: u64 = 3;

    for (i, instr) in statements.iter().enumerate() {
        match instruction_kinds.get(&instr.name).unwrap() {
            InstructionKind::Normal
            | InstructionKind::UnconditionalBranch
            | InstructionKind::ConditionalBranch => {
                let machine = instruction_machines.get(&instr.name).unwrap().clone();

                let (next_global_idx, subs, machine) = powdr::reassign_ids(machine, global_idx, i);
                global_idx = next_global_idx;

                let pc_lookup: PcLookupBusInteraction<T> = machine
                    .bus_interactions
                    .iter()
                    .filter_map(|bus_int| bus_int.clone().try_into().ok())
                    .exactly_one()
                    .expect("Expected single pc lookup");

                let mut sub_map: BTreeMap<Column, AlgebraicExpression<T>> = Default::default();
                let mut local_constraints: Vec<SymbolicConstraint<T>> = Vec::new();

                let is_valid: AlgebraicExpression<T> = exec_receive(&machine).mult.clone();
                let one = AlgebraicExpression::Number(1u64.into());
                local_constraints.push((is_valid.clone() + one).into());

                let mut sub_map_loadstore: BTreeMap<Column, AlgebraicExpression<T>> =
                    Default::default();
                if is_loadstore(instr.opcode) {
                    sub_map_loadstore.extend(loadstore_chip_info(&machine, instr.opcode));
                }

                // Constrain the opcode expression to equal the actual opcode.
                let opcode_constant = AlgebraicExpression::Number((instr.opcode as u64).into());
                local_constraints.push((pc_lookup.op.clone() - opcode_constant).into());

                assert_eq!(instr.args.len(), pc_lookup.args.len());
                instr
                    .args
                    .iter()
                    .zip_eq(&pc_lookup.args)
                    .for_each(|(instr_arg, pc_arg)| {
                        let arg = AlgebraicExpression::Number(*instr_arg);
                        match pc_arg {
                            AlgebraicExpression::Reference(ref arg_ref) => {
                                sub_map.insert(Column::from(arg_ref), arg);
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

                let local_identities = machine
                    .constraints
                    .iter()
                    .chain(&local_constraints)
                    .map(|expr| {
                        let mut expr = expr.expr.clone();
                        powdr::substitute_algebraic(&mut expr, &sub_map);
                        powdr::substitute_algebraic(&mut expr, &sub_map_loadstore);
                        expr = simplify_expression(expr);
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
                        });
                    bus_interactions.push(link);
                }

                col_subs.push(subs);

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

fn is_loadstore(opcode: usize) -> bool {
    (0x210..=0x215).contains(&opcode)
}

fn loadstore_chip_info<T: FieldElement>(
    machine: &SymbolicMachine<T>,
    opcode: usize,
) -> BTreeMap<Column, AlgebraicExpression<T>> {
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
    let is_load_col = if let AlgebraicExpression::Reference(r) = &*is_load_expr {
        r.into()
    } else {
        panic!("expected a single reference")
    };

    [(is_load_col, is_load)].into()
}
