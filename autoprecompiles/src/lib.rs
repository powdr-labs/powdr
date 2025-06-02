use constraint_optimizer::IsBusStateful;
use itertools::Itertools;
use powdr::UniqueColumns;
use powdr_ast::analyzed::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression, AlgebraicReference,
    AlgebraicUnaryOperation, AlgebraicUnaryOperator,
};
use powdr_ast::analyzed::{PolyID, PolynomialType};
use powdr_ast::parsed::visitor::Children;
use powdr_constraint_solver::constraint_system::BusInteractionHandler;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::fmt::Display;
use std::iter::once;
use symbolic_machine_generator::statements_to_symbolic_machine;

use powdr_number::{FieldElement, LargeInt};
use powdr_pilopt::simplify_expression;

pub mod constraint_optimizer;
pub mod optimizer;
pub mod powdr;
pub mod register_optimizer;
pub mod symbolic_machine_generator;

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

impl<T: Copy> SymbolicBusInteraction<T> {
    pub fn try_multiplicity_to_number(&self) -> Option<T> {
        match self.mult {
            AlgebraicExpression::Number(n) => Some(n),
            _ => None,
        }
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

impl<T: Clone + Ord + std::fmt::Display> SymbolicMachine<T> {
    pub fn degree(&self) -> usize {
        let mut cache = Default::default();
        let ints = Default::default();
        self.children()
            .map(|e| e.degree_with_cache(&ints, &mut cache))
            .max()
            .unwrap_or(0)
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

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
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
    Send,
    Receive,
}

#[derive(Clone, Debug)]
pub struct MemoryBusInteraction<T> {
    pub ty: MemoryType,
    pub op: MemoryOp,
    pub addr: AlgebraicExpression<T>,
    pub data: Vec<AlgebraicExpression<T>>,
}

impl<T: FieldElement> MemoryBusInteraction<T> {
    pub fn try_addr_u32(&self) -> Option<u32> {
        match self.addr {
            AlgebraicExpression::Number(n) => n.to_integer().try_into_u32(),
            _ => None,
        }
    }
}

impl<T: FieldElement> MemoryBusInteraction<T> {
    /// Tries to convert a `SymbolicBusInteraction` to a `MemoryBusInteraction` of the given memory type.
    ///
    /// Returns `Ok(None)` if we know that the bus interaction is not a memory bus interaction of the given type.
    /// Returns `Err(_)` if the bus interaction is a memory bus interaction of the given type but could not be converted properly
    /// (usually because the multiplicity is not -1 or 1).
    /// Otherwise returns `Ok(Some(memory_bus_interaction))`
    fn try_from_symbolic_bus_interaction_with_memory_kind(
        bus_interaction: &SymbolicBusInteraction<T>,
        memory_type: MemoryType,
    ) -> Result<Option<Self>, ()> {
        if bus_interaction.id != MEMORY_BUS_ID {
            return Ok(None);
        }
        // TODO: Timestamp is ignored, we could use it to assert that the bus interactions
        // are in the right order.
        let ty = bus_interaction.args[0].clone().into();
        if ty != memory_type {
            return Ok(None);
        }
        let op = match bus_interaction.try_multiplicity_to_number() {
            Some(n) if n == 1.into() => MemoryOp::Send,
            Some(n) if n == (-1).into() => MemoryOp::Receive,
            _ => return Err(()),
        };
        let addr = bus_interaction.args[1].clone();
        let data = bus_interaction.args[2..bus_interaction.args.len() - 1].to_vec();
        Ok(Some(MemoryBusInteraction { ty, op, addr, data }))
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

pub const EXECUTION_BUS_ID: u64 = 0;
pub const MEMORY_BUS_ID: u64 = 1;
pub const PC_LOOKUP_BUS_ID: u64 = 2;

pub fn build<T: FieldElement>(
    program: Vec<SymbolicInstructionStatement<T>>,
    instruction_kind: BTreeMap<String, InstructionKind>,
    instruction_machines: BTreeMap<String, SymbolicMachine<T>>,
    bus_interaction_handler: impl BusInteractionHandler<T> + IsBusStateful<T> + Clone,
    degree_bound: usize,
    opcode: u32,
) -> (SymbolicMachine<T>, Vec<Vec<u64>>) {
    let (machine, subs) =
        statements_to_symbolic_machine(&program, &instruction_kind, &instruction_machines);

    let machine = optimizer::optimize(machine, bus_interaction_handler, Some(opcode), degree_bound);

    // add guards to constraints that are not satisfied by zeroes
    let pre_degree = machine.degree();
    let machine = add_guards(machine);
    assert_eq!(
        pre_degree,
        machine.degree(),
        "Degree should not change after adding guards"
    );

    (machine, subs)
}

fn satisfies_zero_witness<T: FieldElement>(expr: &AlgebraicExpression<T>) -> bool {
    let mut zeroed_expr = expr.clone();
    powdr::make_refs_zero(&mut zeroed_expr);
    let zeroed_expr = simplify_expression(zeroed_expr);
    powdr::is_zero(&zeroed_expr)
}

/// Adds `is_valid` guards to constraints without increasing its degree.
/// This implementation always guards the LHS of multiplications.
/// In the future this could be changed to minimize the number of guards added.
/// Assumption:
/// - `expr` is already simplified, i.e., expressions like (3 + 4) and (x * 1) do not appear.
fn add_guards_constraint<T: FieldElement>(
    expr: AlgebraicExpression<T>,
    is_valid: &AlgebraicExpression<T>,
) -> AlgebraicExpression<T> {
    if satisfies_zero_witness(&expr) {
        return expr;
    }

    match expr {
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation { left, op, right }) => {
            let left = add_guards_constraint(*left, is_valid);
            let right = match op {
                AlgebraicBinaryOperator::Add | AlgebraicBinaryOperator::Sub => {
                    Box::new(add_guards_constraint(*right, is_valid))
                }
                AlgebraicBinaryOperator::Mul => right,
                AlgebraicBinaryOperator::Pow => unimplemented!(),
            };
            AlgebraicExpression::new_binary(left, op, *right)
        }
        AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperation { op, expr }) => {
            let inner = add_guards_constraint(*expr, is_valid);
            AlgebraicExpression::new_unary(op, inner)
        }
        AlgebraicExpression::Number(..) => expr * is_valid.clone(),
        _ => expr,
    }
}

/// Adds an `is_valid` guard to all constraints and bus interactions.
/// Assumptions:
/// - There are exactly one execution bus receive and one execution bus send, in this order.
/// - There is exactly one program bus send.
fn add_guards<T: FieldElement>(mut machine: SymbolicMachine<T>) -> SymbolicMachine<T> {
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

    machine.constraints = machine
        .constraints
        .into_iter()
        .map(|c| add_guards_constraint(c.expr, &is_valid).into())
        .collect();

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
                if !satisfies_zero_witness(&b.mult) {
                    // guard the multiplicity by `is_valid`
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
