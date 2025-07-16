use crate::adapter::{Adapter, AdapterApc};
use crate::bus_map::{BusMap, BusType};
use crate::expression_conversion::algebraic_to_grouped_expression;
use crate::symbolic_machine_generator::convert_machine;
pub use blocks::{BasicBlock, PgoConfig};
use expression::{AlgebraicExpression, AlgebraicReference};
use itertools::Itertools;
use powdr::UniqueReferences;
use powdr_expression::{
    visitors::Children, AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicUnaryOperation,
    AlgebraicUnaryOperator,
};
use serde::{Deserialize, Serialize};
use std::fmt::Display;
use std::io::BufWriter;
use std::iter::once;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use symbolic_machine_generator::statements_to_symbolic_machine;

use powdr_number::FieldElement;

pub mod adapter;
mod bitwise_lookup_optimizer;
pub mod blocks;
pub mod bus_map;
pub mod constraint_optimizer;
pub mod expression;
pub mod expression_conversion;
pub mod memory_optimizer;
pub mod optimizer;
pub mod powdr;
mod stats_logger;
pub mod symbolic_machine_generator;
pub use powdr_constraint_solver::inliner::DegreeBound;

#[derive(Clone)]
pub struct PowdrConfig {
    /// Number of autoprecompiles to generate.
    pub autoprecompiles: u64,
    /// Number of basic blocks to skip for autoprecompiles.
    /// This is either the largest N if no PGO, or the costliest N with PGO.
    pub skip_autoprecompiles: u64,
    /// Max degree of constraints.
    pub degree_bound: DegreeBound,
    /// The path to the APC candidates dir, if any.
    pub apc_candidates_dir_path: Option<PathBuf>,
    /// The opcode id of the first APC instruction. Other APC instructions will have consecutive ids.
    pub first_apc_opcode: usize,
}

impl PowdrConfig {
    pub fn new(
        autoprecompiles: u64,
        skip_autoprecompiles: u64,
        degree_bound: DegreeBound,
        first_apc_opcode: usize,
    ) -> Self {
        Self {
            autoprecompiles,
            skip_autoprecompiles,
            degree_bound,
            apc_candidates_dir_path: None,
            first_apc_opcode,
        }
    }

    pub fn with_apc_candidates_dir<P: AsRef<Path>>(mut self, path: P) -> Self {
        self.apc_candidates_dir_path = Some(path.as_ref().to_path_buf());
        self
    }
}

#[derive(Debug, Clone, PartialEq, Hash, Eq, Serialize, Deserialize)]
pub struct SymbolicInstructionStatement<T> {
    pub opcode: T,
    pub args: Vec<T>,
}

impl<T> IntoIterator for SymbolicInstructionStatement<T> {
    type IntoIter = std::iter::Chain<std::iter::Once<T>, std::vec::IntoIter<T>>;
    type Item = T;

    fn into_iter(self) -> Self::IntoIter {
        once(self.opcode).chain(self.args)
    }
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

impl<T: Clone + Ord + std::fmt::Display> SymbolicMachine<T> {
    pub fn main_columns(&self) -> impl Iterator<Item = AlgebraicReference> + use<'_, T> {
        self.unique_references()
    }
}

impl<T: Display> Display for SymbolicMachine<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for bus_interaction in &self.bus_interactions {
            writeln!(f, "{bus_interaction}")?;
        }
        for constraint in &self.constraints {
            writeln!(f, "{constraint} = 0")?;
        }
        Ok(())
    }
}

impl<T: Display> SymbolicMachine<T> {
    pub fn render(&self, bus_map: &BusMap) -> String {
        let mut output = String::new();
        let bus_interactions_by_bus = self
            .bus_interactions
            .iter()
            .map(|bus_interaction| (bus_interaction.id, bus_interaction))
            .into_group_map()
            .into_iter()
            // sorted_by_key is stable, so we'll keep the order within each bus
            .sorted_by_key(|(bus_id, _)| *bus_id)
            .collect::<Vec<_>>();
        for (bus_id, bus_interactions) in &bus_interactions_by_bus {
            let bus_type = bus_map.bus_type(*bus_id);
            output.push_str(&format!("\n// Bus {bus_id} ({bus_type}):\n",));
            for bus_interaction in bus_interactions {
                output.push_str(&format!(
                    "mult={}, args=[{}]\n",
                    bus_interaction.mult,
                    bus_interaction.args.iter().join(", ")
                ));
            }
        }

        if !self.constraints.is_empty() {
            output.push_str("\n// Algebraic constraints:\n");
        }

        for constraint in &self.constraints {
            output.push_str(&format!("{constraint} = 0\n"));
        }

        output.trim().to_string()
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
    pub instruction: SymbolicInstructionStatement<AlgebraicExpression<T>>,
    pub bus_interaction: SymbolicBusInteraction<T>,
}

impl<T: FieldElement> PcLookupBusInteraction<T> {
    fn try_from_symbolic_bus_interaction(
        bus_interaction: &SymbolicBusInteraction<T>,
        pc_lookup_bus_id: u64,
    ) -> Result<Self, ()> {
        (bus_interaction.id == pc_lookup_bus_id)
            .then(|| {
                let from_pc = bus_interaction.args[0].clone();
                let opcode = bus_interaction.args[1].clone();
                let args = bus_interaction.args[2..].to_vec();
                PcLookupBusInteraction {
                    from_pc,
                    instruction: SymbolicInstructionStatement { opcode, args },
                    bus_interaction: bus_interaction.clone(),
                }
            })
            .ok_or(())
    }
}

/// A configuration of a VM in which execution is happening.
pub struct VmConfig<'a, M, B> {
    /// Maps an opcode to its AIR.
    pub instruction_machine_handler: &'a M,
    /// The bus interaction handler, used by the constraint solver to reason about bus interactions.
    pub bus_interaction_handler: B,
    /// The bus map that maps bus id to bus type
    pub bus_map: BusMap,
}

// We implement Clone manually because deriving it adds a Clone bound to the `InstructionMachineHandler`
impl<'a, M, B: Clone> Clone for VmConfig<'a, M, B> {
    fn clone(&self) -> Self {
        VmConfig {
            instruction_machine_handler: self.instruction_machine_handler,
            bus_interaction_handler: self.bus_interaction_handler.clone(),
            bus_map: self.bus_map.clone(),
        }
    }
}

pub trait InstructionMachineHandler<T, I> {
    /// Returns the AIR for the given opcode.
    fn get_instruction_air(&self, instruction: &I) -> Option<&SymbolicMachine<T>>;
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Apc<T, I> {
    pub block: BasicBlock<I>,
    pub opcode: u32,
    pub machine: SymbolicMachine<T>,
    pub subs: Vec<Vec<u64>>,
}

impl<T, I> Apc<T, I> {
    pub fn subs(&self) -> &[Vec<u64>] {
        &self.subs
    }

    pub fn machine(&self) -> &SymbolicMachine<T> {
        &self.machine
    }
}

pub fn build<A: Adapter>(
    block: BasicBlock<A::Instruction>,
    vm_config: VmConfig<A::InstructionMachineHandler, A::BusInteractionHandler>,
    degree_bound: DegreeBound,
    opcode: u32,
    apc_candidates_dir_path: Option<&Path>,
) -> Result<AdapterApc<A>, crate::constraint_optimizer::Error> {
    let (machine, subs) = statements_to_symbolic_machine::<A>(
        &block.statements,
        vm_config.instruction_machine_handler,
        &vm_config.bus_map,
    );

    let machine = optimizer::optimize(
        machine,
        vm_config.bus_interaction_handler,
        opcode,
        degree_bound,
        &vm_config.bus_map,
    )?;

    // add guards to constraints that are not satisfied by zeroes
    let machine = add_guards(machine, vm_config.bus_map);

    let machine = convert_machine(machine, &A::into_field);

    let apc = Apc {
        block,
        machine,
        subs,
        opcode,
    };

    if let Some(path) = apc_candidates_dir_path {
        let ser_path = path
            .join(format!("apc_candidate_{opcode}"))
            .with_extension("cbor");
        std::fs::create_dir_all(path).expect("Failed to create directory for APC candidates");
        let file =
            std::fs::File::create(&ser_path).expect("Failed to create file for APC candidate");
        let writer = BufWriter::new(file);
        serde_cbor::to_writer(writer, &apc).expect("Failed to write APC candidate to file");
    }

    Ok(apc)
}

fn satisfies_zero_witness<T: FieldElement>(expr: &AlgebraicExpression<T>) -> bool {
    let mut zeroed_expr = expr.clone();
    powdr::make_refs_zero(&mut zeroed_expr);
    let zeroed_expr = algebraic_to_grouped_expression(&zeroed_expr);
    zeroed_expr.try_to_number().unwrap().is_zero()
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
fn add_guards<T: FieldElement>(
    mut machine: SymbolicMachine<T>,
    bus_map: BusMap,
) -> SymbolicMachine<T> {
    let pre_degree = machine.degree();
    let exec_bus_id = bus_map.get_bus_id(&BusType::ExecutionBridge).unwrap();
    let pc_lookup_bus_id = bus_map.get_bus_id(&BusType::PcLookup).unwrap();

    let max_id = machine.unique_references().map(|c| c.id).max().unwrap() + 1;

    let is_valid = AlgebraicExpression::Reference(AlgebraicReference {
        name: Arc::new("is_valid".to_string()),
        id: max_id,
    });

    machine.constraints = machine
        .constraints
        .into_iter()
        .map(|c| add_guards_constraint(c.expr, &is_valid).into())
        .collect();

    let [execution_bus_receive, execution_bus_send] = machine
        .bus_interactions
        .iter_mut()
        .filter(|bus_int| bus_int.id == exec_bus_id)
        .collect::<Vec<_>>()
        .try_into()
        .unwrap();

    execution_bus_receive.mult =
        AlgebraicExpression::new_unary(AlgebraicUnaryOperator::Minus, is_valid.clone());
    execution_bus_send.mult = is_valid.clone();

    let [program_bus_send] = machine
        .bus_interactions
        .iter_mut()
        .filter(|bus_int| bus_int.id == pc_lookup_bus_id)
        .collect::<Vec<_>>()
        .try_into()
        .unwrap();
    program_bus_send.mult = is_valid.clone();

    let mut is_valid_mults: Vec<SymbolicConstraint<T>> = Vec::new();
    for b in &mut machine.bus_interactions {
        // already handled exec and pc lookup bus types
        if b.id != exec_bus_id && b.id != pc_lookup_bus_id {
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
