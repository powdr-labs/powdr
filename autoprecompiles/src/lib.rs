use crate::adapter::{
    Adapter, AdapterApc, AdapterApcOverPowdrField, AdapterOptimisticConstraints, AdapterVmConfig,
};
use crate::bus_map::{BusMap, BusType};
use crate::empirical_constraints::EmpiricalConstraints;
use crate::evaluation::AirStats;
use crate::execution::OptimisticConstraints;
use crate::expression_conversion::algebraic_to_grouped_expression;
use crate::symbolic_machine_generator::convert_apc_field_type;
use adapter::AdapterBlock;
use blocks::Block;
use execution::{
    ExecutionState, LocalOptimisticLiteral, OptimisticConstraint, OptimisticExpression,
    OptimisticLiteral,
};
use expression::{AlgebraicExpression, AlgebraicReference};
use itertools::Itertools;
use powdr::UniqueReferences;
use powdr_expression::AlgebraicUnaryOperator;
use powdr_expression::{
    visitors::Children, AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicUnaryOperation,
};
use serde::{Deserialize, Serialize};
use std::collections::BTreeSet;
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
pub mod empirical_constraints;
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
pub mod equivalence_classes;
pub mod execution;
pub mod trace_handler;

#[derive(Clone)]
pub struct PowdrConfig {
    /// Number of autoprecompiles to generate.
    pub autoprecompiles: u64,
    /// Number of basic blocks to skip for autoprecompiles.
    /// This is either the largest N if no PGO, or the costliest N with PGO.
    pub skip_autoprecompiles: u64,
    /// Maximum number of basic blocks included in a superblock
    pub superblock_max_bb_count: u8,
    /// Max degree of constraints.
    pub degree_bound: DegreeBound,
    /// The path to the APC candidates dir, if any.
    pub apc_candidates_dir_path: Option<PathBuf>,
    /// Whether to use optimistic precompiles.
    pub should_use_optimistic_precompiles: bool,
}

impl PowdrConfig {
    pub fn new(
        autoprecompiles: u64,
        skip_autoprecompiles: u64,
        superblock_max_bb_count: u8,
        degree_bound: DegreeBound,
    ) -> Self {
        assert!(
            superblock_max_bb_count > 0,
            "superblock_max_bb_count must be greater than 0"
        );
        Self {
            autoprecompiles,
            skip_autoprecompiles,
            superblock_max_bb_count,
            degree_bound,
            apc_candidates_dir_path: None,
            should_use_optimistic_precompiles: false,
        }
    }

    pub fn with_apc_candidates_dir<P: AsRef<Path>>(mut self, path: P) -> Self {
        self.apc_candidates_dir_path = Some(path.as_ref().to_path_buf());
        self
    }

    pub fn with_optimistic_precompiles(mut self, should_use_optimistic_precompiles: bool) -> Self {
        self.should_use_optimistic_precompiles = should_use_optimistic_precompiles;
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
pub struct Apc<T, I, A, V> {
    /// The block this APC is based on
    pub block: Block<I>,
    /// The symbolic machine for this APC
    pub machine: SymbolicMachine<T>,
    /// For each original instruction, the substitutions from original columns to APC columns
    pub subs: Vec<Vec<Substitution>>,
    /// The optimistic constraints to be satisfied for this apc to be run
    pub optimistic_constraints: OptimisticConstraints<A, V>,
}

impl<T, I, A, V> Apc<T, I, A, V> {
    pub fn subs(&self) -> &[Vec<Substitution>] {
        &self.subs
    }

    pub fn machine(&self) -> &SymbolicMachine<T> {
        &self.machine
    }

    /// The PCs of the original basic blocks composing this APC. Can be used to identify the APC.
    pub fn original_pcs(&self) -> Vec<u64> {
        self.block.original_pcs()
    }

    /// The instructions in the basic block.
    pub fn instructions(&self) -> impl Iterator<Item = &I> + Clone {
        self.block.statements()
    }

    /// Create a new APC based on the given basic block, symbolic machine and column allocator
    /// The column allocator only issues the subs which are actually used in the machine
    fn new(
        block: Block<I>,
        machine: SymbolicMachine<T>,
        optimistic_constraints: OptimisticConstraints<A, V>,
        column_allocator: &ColumnAllocator,
    ) -> Self {
        // Get all poly_ids in the machine
        let all_references = machine
            .unique_references()
            .map(|r| r.id)
            .collect::<BTreeSet<_>>();
        // Only keep substitutions from the column allocator if the target poly_id is used in the machine
        let subs = column_allocator
            .subs
            .iter()
            .map(|subs| {
                subs.iter()
                    .enumerate()
                    .filter_map(|(original_poly_index, apc_poly_id)| {
                        all_references
                            .contains(apc_poly_id)
                            .then_some(Substitution {
                                original_poly_index,
                                apc_poly_id: *apc_poly_id,
                            })
                    })
                    .collect_vec()
            })
            .collect();
        Self {
            block,
            machine,
            subs,
            optimistic_constraints,
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
    block: Block<A::Instruction>,
    vm_config: AdapterVmConfig<A>,
    degree_bound: DegreeBound,
    apc_candidates_dir_path: Option<&Path>,
    _empirical_constraints: &EmpiricalConstraints,
) -> Result<AdapterApc<A>, crate::constraint_optimizer::Error> {
    let start = std::time::Instant::now();

    let (machine, column_allocator) = statements_to_symbolic_machine::<A>(
        &block,
        vm_config.instruction_handler,
        &vm_config.bus_map,
    );

    // TODO: Generate constraints for optimistic precompiles.
    // let constraint_generator = ConstraintGenerator::<A>::new(
    //     empirical_constraints,
    //     &column_allocator.subs,
    //     machine.main_columns(),
    //     &block,
    // );
    // let range_analyzer_constraints = constraint_generator.range_constraints();
    // let equivalence_analyzer_constraints = constraint_generator.equivalence_constraints();

    // // Add empirical constraints to the baseline
    // machine.constraints.extend(range_analyzer_constraints);
    // machine.constraints.extend(equivalence_analyzer_constraints);

    if let Some(path) = apc_candidates_dir_path {
        serialize_apc_from_machine::<A>(
            block.clone(),
            machine.clone(),
            &column_allocator,
            path,
            Some("unopt"),
        );
    }

    let labels = [("apc_original_pcs", block.original_pcs().iter().join(","))];
    metrics::counter!("before_opt_cols", &labels)
        .absolute(machine.unique_references().count() as u64);
    metrics::counter!("before_opt_constraints", &labels)
        .absolute(machine.unique_references().count() as u64);
    metrics::counter!("before_opt_interactions", &labels)
        .absolute(machine.unique_references().count() as u64);

    // Block boundaries (instruction indices where new basic blocks start in a superblock)
    let block_boundaries: std::collections::HashSet<usize> = block
        .insn_indexed_pcs()
        .into_iter()
        .skip(1) // Skip index 0, it's always the start
        .map(|(idx, _)| idx)
        .collect();

    let (machine, column_allocator) = optimizer::optimize::<A>(
        machine,
        vm_config.bus_interaction_handler,
        degree_bound,
        &vm_config.bus_map,
        column_allocator,
        &block_boundaries,
    )?;

    // add guards to constraints that are not satisfied by zeroes
    let (machine, column_allocator) = add_guards(machine, column_allocator);

    metrics::counter!("after_opt_cols", &labels)
        .absolute(machine.unique_references().count() as u64);
    metrics::counter!("after_opt_constraints", &labels)
        .absolute(machine.unique_references().count() as u64);
    metrics::counter!("after_opt_interactions", &labels)
        .absolute(machine.unique_references().count() as u64);

    // TODO: add optimistic constraints here
    let pc_constraints = superblock_pc_constraints::<A>(&block);
    let optimistic_constraints = OptimisticConstraints::from_constraints(pc_constraints);

    let apc = Apc::new(block, machine, optimistic_constraints, &column_allocator);

    if let Some(path) = apc_candidates_dir_path {
        serialize_apc::<A>(&apc, path, None);

        // For debugging, also serialize a human-readable version of the final precompile
        let rendered = apc.machine.render(&vm_config.bus_map);
        let path = make_path(path, apc.original_pcs(), None, "txt");
        std::fs::write(path, rendered).unwrap();
    }

    let apc = convert_apc_field_type(apc, &A::into_field);

    metrics::gauge!("apc_gen_time_ms", &labels).set(start.elapsed().as_millis() as f64);

    Ok(apc)
}

/// Generate optimistic constraints for superblock jumps (doesn't enforce the starting PC).
fn superblock_pc_constraints<A: Adapter>(
    block: &Block<A::Instruction>,
) -> Vec<
    OptimisticConstraint<
        <<A as Adapter>::ExecutionState as ExecutionState>::RegisterAddress,
        <<A as Adapter>::ExecutionState as ExecutionState>::Value,
    >,
> {
    block
        .insn_indexed_pcs()
        .into_iter()
        .skip(1)
        .map(|(instr_idx, pc)| {
            let left = OptimisticExpression::Literal(OptimisticLiteral {
                instr_idx,
                val: LocalOptimisticLiteral::Pc,
            });
            let Ok(pc_value) =
                <<A as Adapter>::ExecutionState as ExecutionState>::Value::try_from(pc)
            else {
                panic!("PC doesn't fit in Value type");
            };
            let right = OptimisticExpression::Number(pc_value);
            OptimisticConstraint { left, right }
        })
        .collect()
}

fn make_path(
    base_path: &Path,
    original_pcs: Vec<u64>,
    suffix: Option<&str>,
    extension: &str,
) -> PathBuf {
    let suffix = suffix.map(|s| format!("_{s}")).unwrap_or_default();
    base_path
        .join(format!(
            "apc_candidate_{}{suffix}",
            original_pcs.into_iter().join("_")
        ))
        .with_extension(extension)
}

fn serialize_apc<A: Adapter>(apc: &AdapterApcOverPowdrField<A>, path: &Path, suffix: Option<&str>) {
    std::fs::create_dir_all(path).expect("Failed to create directory for APC candidates");

    let ser_path = make_path(path, apc.original_pcs(), suffix, "cbor");
    let file_unopt =
        std::fs::File::create(&ser_path).expect("Failed to create file for {suffix} APC candidate");
    let writer_unopt = BufWriter::new(file_unopt);
    serde_cbor::to_writer(writer_unopt, &apc)
        .expect("Failed to write {suffix} APC candidate to file");
}

fn serialize_apc_from_machine<A: Adapter>(
    block: AdapterBlock<A>,
    machine: SymbolicMachine<A::PowdrField>,
    column_allocator: &ColumnAllocator,
    path: &Path,
    suffix: Option<&str>,
) {
    let apc = Apc::new(
        block,
        machine,
        AdapterOptimisticConstraints::<A>::empty(),
        column_allocator,
    );
    serialize_apc::<A>(&apc, path, suffix);
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
