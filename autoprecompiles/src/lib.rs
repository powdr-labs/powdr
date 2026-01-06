use crate::adapter::{
    Adapter, AdapterApc, AdapterApcOverPowdrField, AdapterBasicBlock, AdapterOptimisticConstraints,
    AdapterVmConfig,
};
use crate::blocks::BasicBlock;
use crate::bus_map::{BusMap, BusType};
use crate::empirical_constraints::{ConstraintGenerator, EmpiricalConstraints};
use crate::evaluation::AirStats;
use crate::execution::OptimisticConstraints;
use crate::expression_conversion::{algebraic_to_grouped_expression, grouped_expression_to_algebraic};
use crate::symbolic_machine_generator::convert_apc_field_type;
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
    /// Max degree of constraints.
    pub degree_bound: DegreeBound,
    /// The path to the APC candidates dir, if any.
    pub apc_candidates_dir_path: Option<PathBuf>,
    /// Whether to use optimistic precompiles.
    pub should_use_optimistic_precompiles: bool,
}

impl PowdrConfig {
    pub fn new(autoprecompiles: u64, skip_autoprecompiles: u64, degree_bound: DegreeBound) -> Self {
        Self {
            autoprecompiles,
            skip_autoprecompiles,
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
    /// The basic block this APC is based on
    pub block: BasicBlock<I>,
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
    block: BasicBlock<A::Instruction>,
    vm_config: AdapterVmConfig<A>,
    degree_bound: DegreeBound,
    apc_candidates_dir_path: Option<&Path>,
    empirical_constraints: &EmpiricalConstraints,
) -> Result<AdapterApc<A>, crate::constraint_optimizer::Error> {
    let start = std::time::Instant::now();

    let (mut machine, column_allocator) = statements_to_symbolic_machine::<A>(
        &block,
        vm_config.instruction_handler,
        &vm_config.bus_map,
    );

    // Generate constraints for optimistic precompiles.
    let constraint_generator = ConstraintGenerator::<A>::new(
        empirical_constraints,
        &column_allocator.subs,
        machine.main_columns(),
        &block,
    );
    let range_analyzer_constraints = constraint_generator.range_constraints();
    let equivalence_analyzer_constraints = constraint_generator.equivalence_constraints();

    // Add empirical constraints to the baseline
    machine.constraints.extend(range_analyzer_constraints);
    machine.constraints.extend(equivalence_analyzer_constraints);

    // Cache pre-optimization constraints for later analysis
    let pre_opt_constraints: Vec<SymbolicConstraint<A::PowdrField>> = machine.constraints.clone();

    if let Some(path) = apc_candidates_dir_path {
        serialize_apc_from_machine::<A>(
            block.clone(),
            machine.clone(),
            &column_allocator,
            path,
            Some("unopt"),
        );
    }

    let labels = [("apc_start_pc", block.start_pc.to_string())];
    metrics::counter!("before_opt_cols", &labels)
        .absolute(machine.unique_references().count() as u64);
    metrics::counter!("before_opt_constraints", &labels)
        .absolute(machine.unique_references().count() as u64);
    metrics::counter!("before_opt_interactions", &labels)
        .absolute(machine.unique_references().count() as u64);

    let (machine, column_allocator, constraint_history) = optimizer::optimize::<A>(
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

    // TODO: add optimistic constraints here
    let optimistic_constraints = OptimisticConstraints::from_constraints(vec![]);

    let apc = Apc::new(block, machine, optimistic_constraints, &column_allocator);

    if let Some(path) = apc_candidates_dir_path {
        serialize_apc::<A>(&apc, path, None);

        // For debugging, also serialize a human-readable version of the final precompile
        let rendered = apc.machine.render(&vm_config.bus_map);
        let path = make_path(path, apc.start_pc(), None, "txt");
        std::fs::write(path, rendered).unwrap();
    }

    // Debug: Track singleton subs and check if they can be solved
    {
        use std::collections::BTreeMap;

        // Helper: compute degree of each poly_id in an expression
        fn compute_poly_degrees<T>(
            expr: &AlgebraicExpression<T>,
        ) -> BTreeMap<u64, usize> {
            fn helper<T>(
                expr: &AlgebraicExpression<T>,
                current_mult: usize,
                degrees: &mut BTreeMap<u64, usize>,
            ) {
                match expr {
                    AlgebraicExpression::Reference(r) => {
                        *degrees.entry(r.id).or_insert(0) =
                            degrees.get(&r.id).unwrap_or(&0).max(&current_mult).clone();
                    }
                    AlgebraicExpression::Number(_) => {}
                    AlgebraicExpression::BinaryOperation(op) => {
                        match op.op {
                            AlgebraicBinaryOperator::Add | AlgebraicBinaryOperator::Sub => {
                                helper(&op.left, current_mult, degrees);
                                helper(&op.right, current_mult, degrees);
                            }
                            AlgebraicBinaryOperator::Mul => {
                                // For multiplication A * B, degree of var v is:
                                // degree(v, A) + degree(v, B)
                                let mut left_degrees = BTreeMap::new();
                                let mut right_degrees = BTreeMap::new();
                                helper(&op.left, 1, &mut left_degrees);
                                helper(&op.right, 1, &mut right_degrees);

                                // Collect all poly_ids from both sides
                                let all_ids: BTreeSet<u64> = left_degrees
                                    .keys()
                                    .chain(right_degrees.keys())
                                    .cloned()
                                    .collect();

                                for id in all_ids {
                                    let left_deg = left_degrees.get(&id).copied().unwrap_or(0);
                                    let right_deg = right_degrees.get(&id).copied().unwrap_or(0);
                                    let new_deg = (left_deg + right_deg) * current_mult;
                                    *degrees.entry(id).or_insert(0) =
                                        degrees.get(&id).unwrap_or(&0).max(&new_deg).clone();
                                }
                            }
                        }
                    }
                    AlgebraicExpression::UnaryOperation(op) => {
                        helper(&op.expr, current_mult, degrees);
                    }
                }
            }
            let mut degrees = BTreeMap::new();
            helper(expr, 1, &mut degrees);
            degrees
        }

        // Helper: collect all poly_ids from an expression
        fn collect_poly_ids<T>(expr: &AlgebraicExpression<T>, ids: &mut BTreeSet<u64>) {
            match expr {
                AlgebraicExpression::Reference(r) => {
                    ids.insert(r.id);
                }
                AlgebraicExpression::Number(_) => {}
                AlgebraicExpression::BinaryOperation(op) => {
                    collect_poly_ids(&op.left, ids);
                    collect_poly_ids(&op.right, ids);
                }
                AlgebraicExpression::UnaryOperation(op) => {
                    collect_poly_ids(&op.expr, ids);
                }
            }
        }

        // Collect poly_ids that appear in constraints
        let mut poly_ids_in_constraints: BTreeSet<u64> = BTreeSet::new();
        for constraint in &apc.machine.constraints {
            collect_poly_ids(&constraint.expr, &mut poly_ids_in_constraints);
        }

        // Collect poly_ids that appear in bus interactions
        let mut poly_ids_in_bus: BTreeSet<u64> = BTreeSet::new();
        for bus in &apc.machine.bus_interactions {
            collect_poly_ids(&bus.mult, &mut poly_ids_in_bus);
            for arg in &bus.args {
                collect_poly_ids(arg, &mut poly_ids_in_bus);
            }
        }

        // Collect apc_poly_ids where the inner vec has length 1
        let singleton_poly_ids: BTreeSet<u64> = apc
            .subs
            .iter()
            .enumerate()
            .filter_map(|(instr_idx, subs)| {
                if subs.len() == 1 {
                    let poly_id = subs[0].apc_poly_id;
                    let in_constraints = poly_ids_in_constraints.contains(&poly_id);
                    let in_bus = poly_ids_in_bus.contains(&poly_id);
                    let location = match (in_constraints, in_bus) {
                        (true, true) => "constraints+bus",
                        (true, false) => "constraints",
                        (false, true) => "bus",
                        (false, false) => "NONE",
                    };
                    println!(
                        "[DEBUG] Instruction {} has singleton sub: original_poly_index={}, apc_poly_id={} [{}]",
                        instr_idx, subs[0].original_poly_index, poly_id, location
                    );
                    Some(poly_id)
                } else {
                    None
                }
            })
            .collect();

        if singleton_poly_ids.is_empty() {
            println!("[DEBUG] No singleton poly_ids found");
        } else {
            println!("[DEBUG] Singleton poly_ids (initially unknown): {:?}", singleton_poly_ids);

            // All poly_ids in the machine
            let all_poly_ids: BTreeSet<u64> = apc
                .machine
                .unique_references()
                .map(|r| r.id)
                .collect();

            // Known = all poly_ids except singletons
            let mut known: BTreeSet<u64> = all_poly_ids
                .difference(&singleton_poly_ids)
                .cloned()
                .collect();
            let mut unknown: BTreeSet<u64> = singleton_poly_ids.clone();

            println!("[DEBUG] Initially known poly_ids: {} total", known.len());
            println!("[DEBUG] Initially unknown poly_ids: {:?}", unknown);

            // Helper to render expression with unknowns named, knowns as "known"
            fn render_expr_with_unknowns<T: std::fmt::Display>(
                expr: &AlgebraicExpression<T>,
                unknown: &BTreeSet<u64>,
            ) -> String {
                match expr {
                    AlgebraicExpression::Reference(r) => {
                        if unknown.contains(&r.id) {
                            format!("{}(#{})", r.name, r.id)
                        } else {
                            "known".to_string()
                        }
                    }
                    AlgebraicExpression::Number(n) => format!("{}", n),
                    AlgebraicExpression::BinaryOperation(op) => {
                        let left = render_expr_with_unknowns(&op.left, unknown);
                        let right = render_expr_with_unknowns(&op.right, unknown);
                        let op_str = match op.op {
                            AlgebraicBinaryOperator::Add => "+",
                            AlgebraicBinaryOperator::Sub => "-",
                            AlgebraicBinaryOperator::Mul => "*",
                        };
                        format!("({} {} {})", left, op_str, right)
                    }
                    AlgebraicExpression::UnaryOperation(op) => {
                        let inner = render_expr_with_unknowns(&op.expr, unknown);
                        format!("-{}", inner)
                    }
                }
            }

            // Filter pre-opt constraints to only those with post-opt poly_ids
            let post_opt_poly_ids: BTreeSet<u64> = all_poly_ids.clone();
            let filtered_pre_opt: Vec<_> = pre_opt_constraints
                .iter()
                .filter(|c| {
                    let mut poly_ids = BTreeSet::new();
                    collect_poly_ids(&c.expr, &mut poly_ids);
                    // Keep constraint if ALL its poly_ids exist in post-opt
                    !poly_ids.is_empty() && poly_ids.iter().all(|id| post_opt_poly_ids.contains(id))
                })
                .collect();

            println!(
                "[DEBUG] Pre-opt constraints: {} total, {} filtered (with post-opt poly_ids only)",
                pre_opt_constraints.len(),
                filtered_pre_opt.len()
            );

            // Convert constraint_history (GroupedExpression) to AlgebraicExpression and filter
            let history_constraints: Vec<(String, AlgebraicExpression<A::PowdrField>)> = constraint_history
                .iter()
                .filter_map(|(step, grouped_expr)| {
                    let expr: AlgebraicExpression<A::PowdrField> = grouped_expression_to_algebraic(grouped_expr.clone());
                    let mut poly_ids = BTreeSet::new();
                    collect_poly_ids(&expr, &mut poly_ids);
                    // Keep constraint if ALL its poly_ids exist in post-opt
                    if !poly_ids.is_empty() && poly_ids.iter().all(|id| post_opt_poly_ids.contains(id)) {
                        Some((step.clone(), expr))
                    } else {
                        None
                    }
                })
                .collect();

            println!(
                "[DEBUG] Constraint history: {} total, {} filtered (with post-opt poly_ids only)",
                constraint_history.len(),
                history_constraints.len()
            );

            // Print all constraints before solving
            println!("[DEBUG] === ALL CONSTRAINTS (unknowns named, knowns as 'known') ===");

            // Print pre-opt constraints
            println!("[DEBUG] --- Pre-optimization constraints ---");
            for (idx, constraint) in filtered_pre_opt.iter().enumerate() {
                let mut constraint_poly_ids = BTreeSet::new();
                collect_poly_ids(&constraint.expr, &mut constraint_poly_ids);
                let has_unknown = constraint_poly_ids.iter().any(|id| unknown.contains(id));
                if has_unknown {
                    let rendered = render_expr_with_unknowns(&constraint.expr, &unknown);
                    println!("[DEBUG] [pre-opt {}] {}", idx, rendered);
                }
            }

            // Print post-opt constraints
            println!("[DEBUG] --- Post-optimization constraints ---");
            for (idx, constraint) in apc.machine.constraints.iter().enumerate() {
                let mut constraint_poly_ids = BTreeSet::new();
                collect_poly_ids(&constraint.expr, &mut constraint_poly_ids);
                let has_unknown = constraint_poly_ids.iter().any(|id| unknown.contains(id));
                if has_unknown {
                    let rendered = render_expr_with_unknowns(&constraint.expr, &unknown);
                    println!("[DEBUG] [post-opt {}] {}", idx, rendered);
                }
            }

            // Print history constraints (from optimization steps)
            println!("[DEBUG] --- Constraint history (filtered to post-opt poly_ids) ---");
            for (step, expr) in &history_constraints {
                let mut constraint_poly_ids = BTreeSet::new();
                collect_poly_ids(expr, &mut constraint_poly_ids);
                let has_unknown = constraint_poly_ids.iter().any(|id| unknown.contains(id));
                if has_unknown {
                    let rendered = render_expr_with_unknowns(expr, &unknown);
                    println!("[DEBUG] [history:{}] {}", step, rendered);
                }
            }
            println!("[DEBUG] === END CONSTRAINTS ===");

            // Combine constraints for solving: (label, &expr)
            // We need owned expressions for history_constraints, so use a different approach
            let pre_opt_exprs: Vec<(String, AlgebraicExpression<A::PowdrField>)> = filtered_pre_opt
                .iter()
                .enumerate()
                .map(|(i, c)| (format!("pre-opt {}", i), c.expr.clone()))
                .collect();
            let post_opt_exprs: Vec<(String, AlgebraicExpression<A::PowdrField>)> = apc.machine
                .constraints
                .iter()
                .enumerate()
                .map(|(i, c)| (format!("post-opt {}", i), c.expr.clone()))
                .collect();

            let all_solve_constraints: Vec<(String, AlgebraicExpression<A::PowdrField>)> = pre_opt_exprs
                .into_iter()
                .chain(post_opt_exprs)
                .chain(history_constraints.clone())
                .collect();

            // Show constraint counts by source
            let pre_count = filtered_pre_opt.len();
            let post_count = apc.machine.constraints.len();
            let hist_count = history_constraints.len();
            println!(
                "[DEBUG] Total constraints for solving: {} (pre-opt: {}, post-opt: {}, history: {})",
                all_solve_constraints.len(), pre_count, post_count, hist_count
            );

            // Analyze all constraints with unknowns to show why they can/cannot be solved
            // Deduplicate by (unknowns, solvability) to reduce verbosity
            println!("[DEBUG] === CONSTRAINT SOLVABILITY ANALYSIS ===");
            let mut seen_patterns: BTreeSet<(Vec<(u64, usize)>, String)> = BTreeSet::new();
            let mut solvable_count = 0;
            let mut unsolvable_count = 0;

            for (label, expr) in &all_solve_constraints {
                let poly_degrees = compute_poly_degrees(expr);

                // Check if this constraint has any unknowns
                let mut unknowns_in_constraint: Vec<(u64, usize)> = poly_degrees
                    .iter()
                    .filter(|(id, _)| unknown.contains(id))
                    .map(|(&id, &deg)| (id, deg))
                    .collect();
                unknowns_in_constraint.sort();

                if unknowns_in_constraint.is_empty() {
                    continue;
                }

                let deg1_unknowns: Vec<u64> = unknowns_in_constraint
                    .iter()
                    .filter(|(_, deg)| *deg == 1)
                    .map(|(id, _)| *id)
                    .collect();

                let solvability = if deg1_unknowns.len() == 1 {
                    solvable_count += 1;
                    format!("CAN SOLVE poly_id {}", deg1_unknowns[0])
                } else if deg1_unknowns.is_empty() {
                    unsolvable_count += 1;
                    let max_deg = unknowns_in_constraint.iter().map(|(_, d)| *d).max().unwrap_or(0);
                    format!("CANNOT SOLVE: all unknowns at degree {}", max_deg)
                } else {
                    unsolvable_count += 1;
                    format!("CANNOT SOLVE: {} unknowns at degree 1", deg1_unknowns.len())
                };

                // Only print if we haven't seen this pattern before
                let pattern = (unknowns_in_constraint.clone(), solvability.clone());
                if seen_patterns.insert(pattern) {
                    let rendered = render_expr_with_unknowns(expr, &unknown);
                    println!("[DEBUG] [{}] {} => {}", label, rendered, solvability);
                }
            }
            println!(
                "[DEBUG] Summary: {} constraints can solve, {} cannot (showing unique patterns only)",
                solvable_count, unsolvable_count
            );
            println!("[DEBUG] === END SOLVABILITY ANALYSIS ===");

            // Iteratively solve
            let mut iteration = 0;
            loop {
                iteration += 1;
                let mut solved_this_round: Vec<u64> = Vec::new();

                for (label, expr) in &all_solve_constraints {
                    let poly_degrees = compute_poly_degrees(expr);

                    // Find unknowns with degree 1 in this constraint
                    let unknowns_deg1: Vec<u64> = poly_degrees
                        .iter()
                        .filter(|(id, &deg)| unknown.contains(id) && deg == 1)
                        .map(|(&id, _)| id)
                        .collect();

                    // If exactly one unknown with degree 1, we can solve it
                    if unknowns_deg1.len() == 1 {
                        let solved_id = unknowns_deg1[0];
                        if !solved_this_round.contains(&solved_id) {
                            let rendered = render_expr_with_unknowns(expr, &unknown);
                            println!(
                                "[DEBUG] Iteration {}: [{}] solves poly_id {} (expr: {})",
                                iteration, label, solved_id, rendered
                            );
                            solved_this_round.push(solved_id);
                        }
                    }
                }

                if solved_this_round.is_empty() {
                    println!("[DEBUG] Iteration {}: No progress, stopping", iteration);
                    break;
                }

                for id in solved_this_round {
                    unknown.remove(&id);
                    known.insert(id);
                }

                println!(
                    "[DEBUG] After iteration {}: {} unknown, {} known",
                    iteration,
                    unknown.len(),
                    known.len()
                );

                if unknown.is_empty() {
                    break;
                }
            }

            // Final report
            let solved: BTreeSet<u64> = singleton_poly_ids
                .difference(&unknown)
                .cloned()
                .collect();

            println!("[DEBUG] === FINAL RESULTS ===");
            println!("[DEBUG] Solved singleton poly_ids: {:?}", solved);
            println!("[DEBUG] Unsolved singleton poly_ids: {:?}", unknown);

            if unknown.is_empty() {
                println!("[DEBUG] SUCCESS: All singleton poly_ids are solvable!");
            } else {
                println!(
                    "[DEBUG] FAILURE: {} singleton poly_ids could not be solved",
                    unknown.len()
                );

                // Categorize unsolved poly_ids
                let only_in_bus: Vec<u64> = unknown
                    .iter()
                    .filter(|id| !poly_ids_in_constraints.contains(id) && poly_ids_in_bus.contains(id))
                    .cloned()
                    .collect();
                let in_constraints: Vec<u64> = unknown
                    .iter()
                    .filter(|id| poly_ids_in_constraints.contains(id))
                    .cloned()
                    .collect();
                let in_neither: Vec<u64> = unknown
                    .iter()
                    .filter(|id| !poly_ids_in_constraints.contains(id) && !poly_ids_in_bus.contains(id))
                    .cloned()
                    .collect();

                println!("[DEBUG] === UNSOLVED POLY_ID CATEGORIES ===");
                println!("[DEBUG] Only in bus interactions ({} poly_ids): {:?}", only_in_bus.len(), only_in_bus);
                println!("[DEBUG] In constraints ({} poly_ids): {:?}", in_constraints.len(), in_constraints);
                println!("[DEBUG] In neither ({} poly_ids): {:?}", in_neither.len(), in_neither);

                // For poly_ids only in bus, show which bus interactions
                if !only_in_bus.is_empty() {
                    println!("[DEBUG] === BUS INTERACTIONS for bus-only poly_ids ===");
                    for &unsolved_id in only_in_bus.iter().take(5) {  // Limit to first 5 to avoid spam
                        println!("[DEBUG] Poly_id {} appears in bus interactions:", unsolved_id);
                        for (bus_idx, bus) in apc.machine.bus_interactions.iter().enumerate() {
                            let mut bus_poly_ids = BTreeSet::new();
                            collect_poly_ids(&bus.mult, &mut bus_poly_ids);
                            for arg in &bus.args {
                                collect_poly_ids(arg, &mut bus_poly_ids);
                            }
                            if bus_poly_ids.contains(&unsolved_id) {
                                println!("  [bus {}] {}", bus_idx, bus);
                            }
                        }
                    }
                    if only_in_bus.len() > 5 {
                        println!("[DEBUG] ... and {} more bus-only poly_ids", only_in_bus.len() - 5);
                    }
                }

                // For poly_ids in constraints, show why they couldn't be solved
                if !in_constraints.is_empty() {
                    println!("[DEBUG] === DIAGNOSTIC: Why constraint poly_ids are unsolved ===");
                    for &unsolved_id in in_constraints.iter().take(10) {  // Limit to first 10
                        println!("[DEBUG] Poly_id {} appears in:", unsolved_id);

                        for (label, expr) in &all_solve_constraints {
                            let poly_degrees = compute_poly_degrees(expr);

                            if let Some(&deg) = poly_degrees.get(&unsolved_id) {
                                // Count unknowns with degree 1
                                let unknowns_deg1: Vec<u64> = poly_degrees
                                    .iter()
                                    .filter(|(id, &d)| unknown.contains(id) && d == 1)
                                    .map(|(&id, _)| id)
                                    .collect();

                                // Count all unknowns
                                let all_unknowns: Vec<(u64, usize)> = poly_degrees
                                    .iter()
                                    .filter(|(id, _)| unknown.contains(id))
                                    .map(|(&id, &d)| (id, d))
                                    .collect();

                                let reason = if deg > 1 {
                                    format!("degree {} (not linear)", deg)
                                } else if unknowns_deg1.len() > 1 {
                                    format!("{} unknowns at deg 1: {:?}", unknowns_deg1.len(), unknowns_deg1)
                                } else if unknowns_deg1.is_empty() {
                                    "no unknowns at degree 1".to_string()
                                } else {
                                    "should be solvable?".to_string()
                                };

                                let rendered = render_expr_with_unknowns(expr, &unknown);
                                println!(
                                    "  [{}] {} | deg={}, unknowns={:?}, reason: {}",
                                    label, rendered, deg, all_unknowns, reason
                                );
                            }
                        }
                    }
                    if in_constraints.len() > 10 {
                        println!("[DEBUG] ... and {} more constraint poly_ids", in_constraints.len() - 10);
                    }
                }
            }
        }
    }

    let apc = convert_apc_field_type(apc, &A::into_field);

    metrics::gauge!("apc_gen_time_ms", &labels).set(start.elapsed().as_millis() as f64);

    Ok(apc)
}

fn make_path(base_path: &Path, start_pc: u64, suffix: Option<&str>, extension: &str) -> PathBuf {
    let suffix = suffix.map(|s| format!("_{s}")).unwrap_or_default();
    base_path
        .join(format!("apc_candidate_{start_pc}{suffix}"))
        .with_extension(extension)
}

fn serialize_apc<A: Adapter>(apc: &AdapterApcOverPowdrField<A>, path: &Path, suffix: Option<&str>) {
    std::fs::create_dir_all(path).expect("Failed to create directory for APC candidates");

    let ser_path = make_path(path, apc.start_pc(), suffix, "cbor");
    let file_unopt =
        std::fs::File::create(&ser_path).expect("Failed to create file for {suffix} APC candidate");
    let writer_unopt = BufWriter::new(file_unopt);
    serde_cbor::to_writer(writer_unopt, &apc)
        .expect("Failed to write {suffix} APC candidate to file");
}

fn serialize_apc_from_machine<A: Adapter>(
    block: AdapterBasicBlock<A>,
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
