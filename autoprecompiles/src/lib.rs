use crate::adapter::{Adapter, AdapterApc, AdapterVmConfig};
use crate::blocks::BasicBlock;
use crate::bus_map::{BusMap, BusType};
use crate::evaluation::AirStats;
use crate::expression_conversion::algebraic_to_grouped_expression;
use crate::symbolic_machine_generator::convert_machine_field_type;
use expression::{AlgebraicExpression, AlgebraicReference};
use itertools::Itertools;
use powdr::UniqueReferences;
use powdr_expression::AlgebraicUnaryOperator;
use powdr_expression::{
    visitors::Children, AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicUnaryOperation,
};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Display;
use std::io::BufWriter;
use std::iter::once;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use symbolic_machine_generator::statements_to_symbolic_machine;

use powdr_number::FieldElement;

pub mod adapter;
pub mod blocks;
pub mod bus_map;
pub mod constraint_optimizer;
pub mod evaluation;
pub mod execution_profile;
pub mod expression;
pub mod expression_conversion;
pub mod low_degree_bus_interaction_optimizer;
pub mod memory_optimizer;
pub mod optimizer;
pub mod pgo;
pub mod powdr;
pub mod range_constraint_optimizer;
mod stats_logger;
pub mod symbolic_machine_generator;
pub use pgo::{PgoConfig, PgoType};
pub use powdr_constraint_solver::inliner::DegreeBound;
pub mod trace_handler;

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
}

impl PowdrConfig {
    pub fn new(autoprecompiles: u64, skip_autoprecompiles: u64, degree_bound: DegreeBound) -> Self {
        Self {
            autoprecompiles,
            skip_autoprecompiles,
            degree_bound,
            apc_candidates_dir_path: None,
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
        let expr = match expr {
            AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperation {
                op: AlgebraicUnaryOperator::Minus,
                expr,
            }) => *expr, // Remove the negation at the outside.
            other => other,
        };
        Self { expr }
    }
}

impl<T> Children<AlgebraicExpression<T>> for SymbolicConstraint<T> {
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

impl<T> Children<AlgebraicExpression<T>> for SymbolicBusInteraction<T> {
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

/// A machine comprised of algebraic constraints, bus interactions and potentially derived columns.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SymbolicMachine<T> {
    /// Constraints whose expressions have to evaluate to zero for an assignment to be satisfying.
    pub constraints: Vec<SymbolicConstraint<T>>,
    /// Bus interactions that model communication with other machines / chips or static lookups.
    pub bus_interactions: Vec<SymbolicBusInteraction<T>>,
    /// Columns that have been newly created during the optimization process with a method
    /// to compute their values from other columns.
    pub derived_columns: Vec<(AlgebraicReference, ComputationMethod<T>)>,
}

/// Attempts to generate a witness for the given symbolic machine using the provided memory snapshot.
///
/// This function:
/// 1. Collects input columns from receive memory bus interactions (using MemoryBusInteraction trait)
/// 2. Assigns values to those columns based on the memory snapshot
/// 3. Attempts to solve for all other columns using the constraint solver
/// 4. Verifies all constraints evaluate to zero
///
/// Returns `true` if a valid witness was found and all constraints are satisfied.
///
/// # Type Parameters
/// - `T`: The field element type
/// - `M`: The memory bus interaction type that implements `MemoryBusInteraction`
pub fn try_witness<T, M>(
    machine: &SymbolicMachine<T>,
    memory: &BTreeMap<u32, BTreeMap<u32, u32>>,
    memory_bus_id: u64,
    _pc: u32,
    _timestamp: u32,
) -> bool
where
    T: FieldElement,
    M: memory_optimizer::MemoryBusInteraction<T, expression::AlgebraicReference>,
{
    use crate::expression::{AlgebraicEvaluator, WitnessEvaluator};
    use memory_optimizer::MemoryOp;
    use powdr_constraint_solver::constraint_system::{
        BusInteraction, ConstraintSystem, DefaultBusInteractionHandler,
    };
    use powdr_constraint_solver::grouped_expression::GroupedExpression;
    use powdr_constraint_solver::solver::solve_system;

    // Convert machine constraints to GroupedExpression-based algebraic constraints
    let mut algebraic_constraints = Vec::new();
    for constraint in &machine.constraints {
        let expr = expression_conversion::algebraic_to_grouped_expression(&constraint.expr);
        algebraic_constraints.push(
            powdr_constraint_solver::constraint_system::AlgebraicConstraint::assert_zero(expr),
        );
    }

    // Convert bus interactions to constraint solver format
    let mut bus_interactions = Vec::new();
    for bus_int in &machine.bus_interactions {
        let mult = expression_conversion::algebraic_to_grouped_expression(&bus_int.mult);
        let bus_id = GroupedExpression::from_number(T::from(bus_int.id));
        let payload: Vec<_> = bus_int
            .args
            .iter()
            .map(|arg| expression_conversion::algebraic_to_grouped_expression(arg))
            .collect();
        bus_interactions.push(BusInteraction {
            bus_id,
            multiplicity: mult,
            payload,
        });
    }

    // Use MemoryBusInteraction trait to identify receive (read) memory operations
    // and add constraints based on memory snapshot
    for bus_int in &bus_interactions {
        let mem_int = match M::try_from_bus_interaction(bus_int, memory_bus_id) {
            Ok(Some(m)) => m,
            Ok(None) => continue, // Not a memory bus interaction
            Err(_) => continue,   // Could not parse (e.g., multiplicity not concrete)
        };

        // Only process receive (read) operations
        if mem_int.op() != MemoryOp::GetPrevious {
            continue;
        }

        // Extract address components and try to look up in memory
        let addr_parts: Vec<_> = mem_int.addr().into_iter().collect();
        if addr_parts.len() < 2 {
            continue;
        }

        // First part is address space, second is the address
        let addr_space = match addr_parts[0].try_to_number() {
            Some(n) => n.to_degree() as u32,
            None => continue,
        };
        let addr = match addr_parts[1].try_to_number() {
            Some(n) => n.to_degree() as u32,
            None => continue,
        };

        // Look up value in memory snapshot
        if let Some(addr_space_mem) = memory.get(&addr_space) {
            if let Some(&value) = addr_space_mem.get(&addr) {
                // Add constraints for each data limb (data is byte limbs)
                let data = mem_int.data();
                let mut remaining = value;
                for limb in data {
                    let byte_value = (remaining & 0xFF) as u64;
                    remaining >>= 8;
                    let value_expr = GroupedExpression::from_number(T::from(byte_value));
                    algebraic_constraints.push(
                        powdr_constraint_solver::constraint_system::AlgebraicConstraint::assert_zero(
                            limb.clone() - value_expr,
                        ),
                    );
                }
            }
        }
    }

    let constraint_system: ConstraintSystem<T, expression::AlgebraicReference> = ConstraintSystem {
        algebraic_constraints,
        bus_interactions,
        derived_variables: vec![],
    };

    // Try to solve the constraint system
    let result = solve_system(constraint_system, DefaultBusInteractionHandler::default());

    match result {
        Ok(assignments) => {
            // Create a witness map from solved assignments
            let mut witness: BTreeMap<u64, T> = BTreeMap::new();
            for (var, expr) in assignments {
                if let Some(value) = expr.try_to_number() {
                    witness.insert(var.id, value);
                }
            }

            // Verify all constraints evaluate to zero
            let evaluator: WitnessEvaluator<T, T, T> = WitnessEvaluator::new(&witness);
            for constraint in &machine.constraints {
                let value = evaluator.eval_expr(&constraint.expr);
                if !value.is_zero() {
                    return false;
                }
            }
            true
        }
        Err(_) => false,
    }
}

type ComputationMethod<T> =
    powdr_constraint_solver::constraint_system::ComputationMethod<T, AlgebraicExpression<T>>;

impl<T> SymbolicMachine<T> {
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

impl<T: Display + Ord + Clone> SymbolicMachine<T> {
    pub fn render<C: Display + Clone + PartialEq + Eq>(&self, bus_map: &BusMap<C>) -> String {
        let main_columns = self.main_columns().sorted().collect_vec();
        let mut output = format!(
            "Symbolic machine using {} unique main columns:\n  {}\n",
            main_columns.len(),
            main_columns.iter().join("\n  ")
        );
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

impl<T> SymbolicMachine<T> {
    pub fn degree(&self) -> usize {
        self.children().map(|e| e.degree()).max().unwrap_or(0)
    }
}

impl<T> Children<AlgebraicExpression<T>> for SymbolicMachine<T> {
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

/// A configuration of a VM in which execution is happening.
pub struct VmConfig<'a, M, B, C> {
    /// Maps an opcode to its AIR.
    pub instruction_handler: &'a M,
    /// The bus interaction handler, used by the constraint solver to reason about bus interactions.
    pub bus_interaction_handler: B,
    /// The bus map that maps bus id to bus type
    pub bus_map: BusMap<C>,
}

// We implement Clone manually because deriving it adds a Clone bound to the `InstructionMachineHandler`
impl<'a, M, B: Clone, C: Clone> Clone for VmConfig<'a, M, B, C> {
    fn clone(&self) -> Self {
        VmConfig {
            instruction_handler: self.instruction_handler,
            bus_interaction_handler: self.bus_interaction_handler.clone(),
            bus_map: self.bus_map.clone(),
        }
    }
}

pub trait InstructionHandler {
    type Field;
    type Instruction;
    type AirId;

    /// Returns the AIR for the given instruction.
    fn get_instruction_air_and_id(
        &self,
        instruction: &Self::Instruction,
    ) -> (Self::AirId, &SymbolicMachine<Self::Field>);

    /// Returns the AIR stats for the given instruction.
    fn get_instruction_air_stats(&self, instruction: &Self::Instruction) -> AirStats;

    /// Returns whether the given instruction is allowed in an autoprecompile.
    fn is_allowed(&self, instruction: &Self::Instruction) -> bool;

    /// Returns whether the given instruction is a branching instruction.
    fn is_branching(&self, instruction: &Self::Instruction) -> bool;
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Substitution {
    /// The index of the original column in the original air
    pub original_poly_index: usize,
    /// The `poly_id` of the target column in the APC air
    pub apc_poly_id: u64,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Apc<T, I> {
    /// The basic block this APC is based on
    pub block: BasicBlock<I>,
    /// The symbolic machine for this APC
    pub machine: SymbolicMachine<T>,
    /// For each original air, the substitutions from original columns to APC columns
    pub subs: Vec<Vec<Substitution>>,
}

impl<T, I> Apc<T, I> {
    pub fn subs(&self) -> &[Vec<Substitution>] {
        &self.subs
    }

    pub fn machine(&self) -> &SymbolicMachine<T> {
        &self.machine
    }

    /// The PC of the first line of the basic block. Can be used to identify the APC.
    pub fn start_pc(&self) -> u64 {
        self.block.start_pc
    }

    /// The instructions in the basic block.
    pub fn instructions(&self) -> &[I] {
        &self.block.statements
    }

    /// Create a new APC based on the given basic block, symbolic machine and column allocator
    /// The column allocator only issues the subs which are actually used in the machine
    fn new(
        block: BasicBlock<I>,
        machine: SymbolicMachine<T>,
        column_allocator: ColumnAllocator,
    ) -> Self {
        // Get all poly_ids in the machine
        let all_references = machine
            .unique_references()
            .map(|r| r.id)
            .collect::<BTreeSet<_>>();
        // Only keep substitutions from the column allocator if the target poly_id is used in the machine
        let subs = column_allocator
            .subs
            .into_iter()
            .map(|subs| {
                subs.into_iter()
                    .enumerate()
                    .filter_map(|(original_poly_index, apc_poly_id)| {
                        all_references
                            .contains(&apc_poly_id)
                            .then_some(Substitution {
                                original_poly_index,
                                apc_poly_id,
                            })
                    })
                    .collect_vec()
            })
            .collect();
        Self {
            block,
            machine,
            subs,
        }
    }
}

/// Allocates global poly_ids and keeps track of substitutions
pub struct ColumnAllocator {
    /// For each original air, for each original column index, the associated poly_id in the APC air
    subs: Vec<Vec<u64>>,
    /// The next poly_id to issue
    next_poly_id: u64,
}

impl ColumnAllocator {
    pub fn from_max_poly_id_of_machine(machine: &SymbolicMachine<impl FieldElement>) -> Self {
        Self {
            subs: Vec::new(),
            next_poly_id: machine.main_columns().map(|c| c.id).max().unwrap_or(0) + 1,
        }
    }

    pub fn issue_next_poly_id(&mut self) -> u64 {
        let id = self.next_poly_id;
        self.next_poly_id += 1;
        id
    }
}

pub fn build<A: Adapter>(
    block: BasicBlock<A::Instruction>,
    vm_config: AdapterVmConfig<A>,
    degree_bound: DegreeBound,
    apc_candidates_dir_path: Option<&Path>,
) -> Result<AdapterApc<A>, crate::constraint_optimizer::Error> {
    let start = std::time::Instant::now();

    let (machine, column_allocator) = statements_to_symbolic_machine::<A>(
        &block,
        vm_config.instruction_handler,
        &vm_config.bus_map,
    );

    let labels = [("apc_start_pc", block.start_pc.to_string())];
    metrics::counter!("before_opt_cols", &labels)
        .absolute(machine.unique_references().count() as u64);
    metrics::counter!("before_opt_constraints", &labels)
        .absolute(machine.unique_references().count() as u64);
    metrics::counter!("before_opt_interactions", &labels)
        .absolute(machine.unique_references().count() as u64);

    let (machine, column_allocator) = optimizer::optimize::<A>(
        machine,
        vm_config.bus_interaction_handler,
        degree_bound,
        &vm_config.bus_map,
        column_allocator,
    )?;

    // add guards to constraints that are not satisfied by zeroes
    let (machine, column_allocator) = add_guards(machine, column_allocator);

    metrics::counter!("after_opt_cols", &labels)
        .absolute(machine.unique_references().count() as u64);
    metrics::counter!("after_opt_constraints", &labels)
        .absolute(machine.unique_references().count() as u64);
    metrics::counter!("after_opt_interactions", &labels)
        .absolute(machine.unique_references().count() as u64);

    let machine = convert_machine_field_type(machine, &A::into_field);

    let apc = Apc::new(block, machine, column_allocator);

    if let Some(path) = apc_candidates_dir_path {
        let ser_path = path
            .join(format!("apc_candidate_{}", apc.start_pc()))
            .with_extension("cbor");
        std::fs::create_dir_all(path).expect("Failed to create directory for APC candidates");
        let file =
            std::fs::File::create(&ser_path).expect("Failed to create file for APC candidate");
        let writer = BufWriter::new(file);
        serde_cbor::to_writer(writer, &apc).expect("Failed to write APC candidate to file");
    }

    metrics::gauge!("apc_gen_time_ms", &labels).set(start.elapsed().as_millis() as f64);

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

/// Adds an `is_valid` guard to all constraints and bus interactions, if needed.
fn add_guards<T: FieldElement>(
    mut machine: SymbolicMachine<T>,
    mut column_allocator: ColumnAllocator,
) -> (SymbolicMachine<T>, ColumnAllocator) {
    let pre_degree = machine.degree();

    let is_valid_ref = AlgebraicReference {
        name: Arc::new("is_valid".to_string()),
        id: column_allocator.issue_next_poly_id(),
    };
    let is_valid = AlgebraicExpression::Reference(is_valid_ref.clone());

    machine
        .derived_columns
        .push((is_valid_ref, ComputationMethod::Constant(T::one())));

    machine.constraints = machine
        .constraints
        .into_iter()
        .map(|c| add_guards_constraint(c.expr, &is_valid).into())
        .collect();

    let mut is_valid_mults: Vec<SymbolicConstraint<T>> = Vec::new();
    for b in &mut machine.bus_interactions {
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

    machine.constraints.extend(is_valid_mults);

    // if pre_degree is 0, is_valid is added to the multiplicities of the bus interactions, thus the degree increases from 0 to 1
    if pre_degree != 0 && !machine.bus_interactions.is_empty() {
        assert_eq!(
            pre_degree,
            machine.degree(),
            "Degree should not change after adding guards"
        );
    }

    // This needs to be added after the assertion above because it's a quadratic constraint
    // so it may increase the degree of the machine.
    machine.constraints.push(powdr::make_bool(is_valid).into());

    (machine, column_allocator)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::memory_optimizer::{
        MemoryBusInteraction, MemoryBusInteractionConversionError, MemoryOp,
    };
    use num_traits::One;
    use powdr_constraint_solver::constraint_system::BusInteraction;
    use powdr_constraint_solver::grouped_expression::GroupedExpression;
    use powdr_number::GoldilocksField;
    use std::fmt::Display;
    use std::hash::Hash;
    use std::sync::Arc;

    type F = GoldilocksField;

    fn make_ref(name: &str, id: u64) -> AlgebraicReference {
        AlgebraicReference {
            name: Arc::new(name.to_string()),
            id,
        }
    }

    fn num(v: u64) -> AlgebraicExpression<F> {
        AlgebraicExpression::Number(F::from(v))
    }

    fn var(name: &str, id: u64) -> AlgebraicExpression<F> {
        AlgebraicExpression::Reference(make_ref(name, id))
    }

    /// Test implementation of MemoryBusInteraction for testing try_witness
    /// Memory bus format: [address_space, address, data..., timestamp]
    struct TestMemoryBusInteraction<T, V> {
        op: MemoryOp,
        address_space: T,
        address: GroupedExpression<T, V>,
        data: Vec<GroupedExpression<T, V>>,
    }

    impl<T: FieldElement, V: Ord + Clone + Eq + Display + Hash> MemoryBusInteraction<T, V>
        for TestMemoryBusInteraction<T, V>
    {
        type Address = Vec<GroupedExpression<T, V>>;

        fn try_from_bus_interaction(
            bus_interaction: &BusInteraction<GroupedExpression<T, V>>,
            memory_bus_id: u64,
        ) -> Result<Option<Self>, MemoryBusInteractionConversionError> {
            // Check bus ID
            match bus_interaction.bus_id.try_to_number() {
                None => return Err(MemoryBusInteractionConversionError),
                Some(id) if id == memory_bus_id.into() => {}
                Some(_) => return Ok(None),
            }

            // Check multiplicity to determine operation type
            let op = match bus_interaction.multiplicity.try_to_number() {
                Some(n) if n == T::one() => MemoryOp::SetNew,
                Some(n) if n == -T::one() => MemoryOp::GetPrevious,
                _ => return Err(MemoryBusInteractionConversionError),
            };

            // Parse payload: [address_space, address, data..., timestamp]
            if bus_interaction.payload.len() < 3 {
                return Err(MemoryBusInteractionConversionError);
            }

            let address_space = match bus_interaction.payload[0].try_to_number() {
                Some(n) => n,
                None => return Err(MemoryBusInteractionConversionError),
            };

            let address = bus_interaction.payload[1].clone();
            let data = bus_interaction.payload[2..bus_interaction.payload.len() - 1].to_vec();

            Ok(Some(TestMemoryBusInteraction {
                op,
                address_space,
                address,
                data,
            }))
        }

        fn addr(&self) -> Self::Address {
            vec![
                GroupedExpression::from_number(self.address_space),
                self.address.clone(),
            ]
        }

        fn data(&self) -> &[GroupedExpression<T, V>] {
            &self.data
        }

        fn op(&self) -> MemoryOp {
            self.op
        }

        fn register_address(&self) -> Option<usize> {
            None
        }
    }

    type TestMBI = TestMemoryBusInteraction<F, AlgebraicReference>;

    const MEMORY_BUS_ID: u64 = 1;

    #[test]
    fn test_try_witness_simple_constraint() {
        // Test: x - 5 = 0 should be solvable
        let machine = SymbolicMachine {
            constraints: vec![SymbolicConstraint {
                expr: var("x", 0) - num(5),
            }],
            bus_interactions: vec![],
            derived_columns: vec![],
        };

        let memory = BTreeMap::new();
        assert!(try_witness::<F, TestMBI>(
            &machine,
            &memory,
            MEMORY_BUS_ID,
            0,
            0
        ));
    }

    #[test]
    fn test_try_witness_unsatisfiable_constraint() {
        // Test: 1 = 0 is never satisfiable
        let machine = SymbolicMachine {
            constraints: vec![SymbolicConstraint { expr: num(1) }],
            bus_interactions: vec![],
            derived_columns: vec![],
        };

        let memory = BTreeMap::new();
        assert!(!try_witness::<F, TestMBI>(
            &machine,
            &memory,
            MEMORY_BUS_ID,
            0,
            0
        ));
    }

    #[test]
    fn test_try_witness_two_variables() {
        // Test: x = 3, y = x * 2 should give x=3, y=6
        let machine = SymbolicMachine {
            constraints: vec![
                SymbolicConstraint {
                    expr: var("x", 0) - num(3),
                },
                SymbolicConstraint {
                    expr: var("y", 1) - var("x", 0) * num(2),
                },
            ],
            bus_interactions: vec![],
            derived_columns: vec![],
        };

        let memory = BTreeMap::new();
        assert!(try_witness::<F, TestMBI>(
            &machine,
            &memory,
            MEMORY_BUS_ID,
            0,
            0
        ));
    }

    #[test]
    fn test_try_witness_empty_machine() {
        // Empty machine should be trivially satisfiable
        let machine: SymbolicMachine<F> = SymbolicMachine {
            constraints: vec![],
            bus_interactions: vec![],
            derived_columns: vec![],
        };

        let memory = BTreeMap::new();
        assert!(try_witness::<F, TestMBI>(
            &machine,
            &memory,
            MEMORY_BUS_ID,
            0,
            0
        ));
    }

    #[test]
    fn test_try_witness_with_memory_lookup() {
        // Test that a receive memory bus interaction can read from memory snapshot
        // Memory bus interaction format: [address_space, address, data_limbs..., timestamp]
        // Data is 4 byte limbs, value 0x2A = 42 = [42, 0, 0, 0] in little-endian byte limbs
        let machine: SymbolicMachine<F> = SymbolicMachine {
            constraints: vec![
                // Constraint that first data limb should equal 42 (the low byte)
                SymbolicConstraint {
                    expr: var("data0", 2) - num(42),
                },
                // Other limbs should be 0
                SymbolicConstraint {
                    expr: var("data1", 3) - num(0),
                },
                SymbolicConstraint {
                    expr: var("data2", 4) - num(0),
                },
                SymbolicConstraint {
                    expr: var("data3", 5) - num(0),
                },
            ],
            bus_interactions: vec![SymbolicBusInteraction {
                id: MEMORY_BUS_ID,
                mult: AlgebraicExpression::Number(-F::one()), // Receive = -1
                args: vec![
                    num(1),          // address_space = 1 (registers)
                    num(10),         // address = 10
                    var("data0", 2), // data limb 0
                    var("data1", 3), // data limb 1
                    var("data2", 4), // data limb 2
                    var("data3", 5), // data limb 3
                    num(0),          // timestamp
                ],
            }],
            derived_columns: vec![],
        };

        // Memory: address_space 1 -> address 10 -> value 42
        let mut memory = BTreeMap::new();
        let mut addr_space_1 = BTreeMap::new();
        addr_space_1.insert(10, 42);
        memory.insert(1, addr_space_1);

        assert!(try_witness::<F, TestMBI>(
            &machine,
            &memory,
            MEMORY_BUS_ID,
            0,
            0
        ));
    }

    #[test]
    fn test_try_witness_memory_lookup_wrong_value() {
        // Test that constraint fails when memory value doesn't match expected
        // Memory has 42, but constraint expects first limb to be 100
        let machine: SymbolicMachine<F> = SymbolicMachine {
            constraints: vec![
                // Constraint expects first data limb = 100, but memory has 42
                SymbolicConstraint {
                    expr: var("data0", 2) - num(100),
                },
            ],
            bus_interactions: vec![SymbolicBusInteraction {
                id: MEMORY_BUS_ID,
                mult: AlgebraicExpression::Number(-F::one()),
                args: vec![
                    num(1),          // address_space
                    num(10),         // address
                    var("data0", 2), // data limb 0
                    var("data1", 3), // data limb 1
                    var("data2", 4), // data limb 2
                    var("data3", 5), // data limb 3
                    num(0),          // timestamp
                ],
            }],
            derived_columns: vec![],
        };

        let mut memory = BTreeMap::new();
        let mut addr_space_1 = BTreeMap::new();
        addr_space_1.insert(10, 42); // 42 = [42, 0, 0, 0] in byte limbs
        memory.insert(1, addr_space_1);

        // This should fail because memory lookup sets data0=42 but constraint expects data0=100
        assert!(!try_witness::<F, TestMBI>(
            &machine,
            &memory,
            MEMORY_BUS_ID,
            0,
            0
        ));
    }
}
