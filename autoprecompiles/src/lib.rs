use constraint_optimizer::IsBusStateful;
use itertools::Itertools;
use legacy_expression::ast_compatibility::CompatibleWithAstExpression;
use legacy_expression::{AlgebraicExpression, AlgebraicReference, PolyID, PolynomialType};
use powdr::UniqueColumns;
use powdr_constraint_solver::constraint_system::BusInteractionHandler;
use powdr_expression::{
    visitors::Children, AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicUnaryOperation,
    AlgebraicUnaryOperator,
};
use serde::{Deserialize, Serialize};
use std::fmt::Display;
use std::{collections::BTreeMap, iter::once};
use symbolic_machine_generator::statements_to_symbolic_machine;

use powdr_number::FieldElement;

mod bitwise_lookup_optimizer;
pub mod constraint_optimizer;
pub mod legacy_expression;
pub mod memory_optimizer;
pub mod optimizer;
pub mod powdr;
mod stats_logger;
pub mod symbolic_machine_generator;

pub fn simplify_expression<T: FieldElement>(e: AlgebraicExpression<T>) -> AlgebraicExpression<T> {
    // Wrap powdr_pilopt::simplify_expression, which uses powdr_ast::analyzed::AlgebraicExpression.
    let ast_expression = e.into_ast_expression();
    let ast_expression = powdr_pilopt::simplify_expression(ast_expression);
    AlgebraicExpression::try_from_ast_expression(ast_expression).unwrap()
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SymbolicInstructionStatement<T> {
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
        self.children().map(|e| e.degree()).max().unwrap_or(0)
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

pub const EXECUTION_BUS_ID: u64 = 0;
pub const MEMORY_BUS_ID: u64 = 1;
pub const PC_LOOKUP_BUS_ID: u64 = 2;
pub const VARIABLE_RANGE_CHECKER_BUS_ID: u64 = 3;
pub const BITWISE_LOOKUP_BUS_ID: u64 = 6;
pub const TUPLE_RANGE_CHECKER_BUS_ID: u64 = 7;

/// A configuration of a VM in which execution is happening.
pub struct VmConfig<'a, T: FieldElement, B> {
    /// Maps an opcode to its AIR.
    pub instruction_machines: &'a BTreeMap<usize, SymbolicMachine<T>>,
    /// The bus interaction handler, used by the constraint solver to reason about bus interactions.
    pub bus_interaction_handler: B,
    // TODO: Add bus map
}

pub fn build<T: FieldElement, B: BusInteractionHandler<T> + IsBusStateful<T> + Clone>(
    program: Vec<SymbolicInstructionStatement<T>>,
    vm_config: VmConfig<T, B>,
    degree_bound: usize,
    opcode: u32,
) -> Result<(SymbolicMachine<T>, Vec<Vec<u64>>), crate::constraint_optimizer::Error> {
    let (machine, subs) = statements_to_symbolic_machine(&program, vm_config.instruction_machines);

    let machine = optimizer::optimize(
        machine,
        vm_config.bus_interaction_handler,
        Some(opcode),
        degree_bound,
    )?;

    // add guards to constraints that are not satisfied by zeroes
    let machine = add_guards(machine);

    Ok((machine, subs))
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
    let pre_degree = machine.degree();

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

    assert_eq!(
        pre_degree,
        machine.degree(),
        "Degree should not change after adding guards"
    );

    // This needs to be added after the assertion above because it's a quadratic constraint
    // so it may increase the degree of the machine.
    machine.constraints.push(powdr::make_bool(is_valid).into());

    machine
}
