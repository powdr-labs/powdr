use crate::adapter::{Adapter, AdapterApc, AdapterVmConfig};
use crate::blocks::{PcStep, SuperBlock};
use crate::bus_map::{BusMap, BusType};
use crate::empirical_constraints::{ConstraintGenerator, EmpiricalConstraints};
use crate::evaluation::AirStats;
use crate::execution::OptimisticConstraints;
use crate::export::ExportOptions;
use crate::expression_conversion::algebraic_to_grouped_expression;
use crate::optimistic::algebraic_references::BlockCellAlgebraicReferenceMapper;
use crate::optimistic::config::optimistic_precompile_config;
use crate::optimistic::execution_constraint_generator::generate_execution_constraints;
use crate::optimistic::execution_literals::optimistic_literals;
use crate::symbolic_machine::{SymbolicConstraint, SymbolicMachine};
use crate::symbolic_machine_generator::convert_apc_field_type;
use expression::{AlgebraicExpression, AlgebraicReference};
use itertools::Itertools;
use powdr::UniqueReferences;
use powdr_constraint_solver::constraint_system::ComputationMethod;
use powdr_expression::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicUnaryOperation,
};
use serde::{Deserialize, Serialize};
use std::collections::BTreeSet;
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
pub mod symbolic_machine;
pub mod symbolic_machine_generator;
pub use pgo::{PgoConfig, PgoType};
pub use powdr_constraint_solver::inliner::DegreeBound;
pub mod equivalence_classes;
pub mod execution;
pub mod export;
pub mod optimistic;
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

    /// Returns the degree bound used for the instructions
    fn degree_bound(&self) -> DegreeBound;

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

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Substitution {
    /// The index of the original column in the original air
    pub original_poly_index: usize,
    /// The `poly_id` of the target column in the APC air
    pub apc_poly_id: u64,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Apc<T, I, A, V> {
    /// The block this APC is based on
    pub block: SuperBlock<I>,
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

    /// The instructions in the block.
    pub fn instructions(&self) -> impl Iterator<Item = &I> + Clone {
        self.block.instructions()
    }

    /// The PCs of the original basic blocks composing this APC. Can be used to identify the APC.
    pub fn start_pcs(&self) -> Vec<u64> {
        self.block.start_pcs()
    }

    /// Create a new APC based on the given super block, symbolic machine and column allocator
    /// The column allocator only issues the subs which are actually used in the machine
    fn new(
        block: SuperBlock<I>,
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

    /// Returns whether the given poly_id is known (i.e., was issued by this allocator)
    pub fn is_known_id(&self, poly_id: u64) -> bool {
        poly_id < self.next_poly_id
    }
}

pub fn build<A: Adapter>(
    block: SuperBlock<A::Instruction>,
    vm_config: AdapterVmConfig<A>,
    degree_bound: DegreeBound,
    mut export_options: ExportOptions,
    empirical_constraints: &EmpiricalConstraints,
) -> Result<AdapterApc<A>, crate::constraint_optimizer::Error> {
    let basic_block = block
        .try_as_basic_block()
        .expect("superblocks not supported yet");
    let start = std::time::Instant::now();

    let (mut machine, column_allocator) = statements_to_symbolic_machine::<A>(
        basic_block,
        vm_config.instruction_handler,
        &vm_config.bus_map,
    );

    // Generate constraints for optimistic precompiles.
    let should_generate_execution_constraints =
        optimistic_precompile_config().restrict_optimistic_precompiles;
    let algebraic_references =
        BlockCellAlgebraicReferenceMapper::new(&column_allocator.subs, machine.main_columns());
    let empirical_constraints = empirical_constraints.for_block(basic_block);

    // TODO: Use execution constraints
    let (empirical_constraints, _execution_constraints) = if should_generate_execution_constraints {
        // Filter constraints to only contain execution-checkable columns,
        // generate execution constraints for them.
        let optimistic_literals = optimistic_literals::<A>(basic_block, &vm_config, &degree_bound);

        let empirical_constraints = empirical_constraints.filtered(
            |block_cell| {
                let algebraic_reference = algebraic_references
                    .get_algebraic_reference(block_cell)
                    .unwrap();
                optimistic_literals.contains_key(algebraic_reference)
            },
            <A::Instruction as PcStep>::pc_step(),
        );

        let empirical_constraints =
            ConstraintGenerator::<A>::new(empirical_constraints, algebraic_references, basic_block)
                .generate_constraints();

        let execution_constraints =
            generate_execution_constraints(&empirical_constraints, &optimistic_literals);
        (empirical_constraints, execution_constraints)
    } else {
        // Don't filter empirical constraints, return empty execution constraints.
        let empirical_constraints =
            ConstraintGenerator::<A>::new(empirical_constraints, algebraic_references, basic_block)
                .generate_constraints();
        (empirical_constraints, vec![])
    };

    // Add empirical constraints to the baseline
    machine
        .constraints
        .extend(empirical_constraints.into_iter().map(Into::into));

    if export_options.export_requested() {
        export_options.export_apc_from_machine::<A>(
            block.clone(),
            machine.clone(),
            &column_allocator,
            &vm_config.bus_map,
            Some("unopt"),
        );
    }

    let labels = [("apc_start_pc", block.start_pcs().into_iter().join("_"))];
    metrics::counter!("before_opt_cols", &labels)
        .absolute(machine.unique_references().count() as u64);
    metrics::counter!("before_opt_constraints", &labels)
        .absolute(machine.unique_references().count() as u64);
    metrics::counter!("before_opt_interactions", &labels)
        .absolute(machine.unique_references().count() as u64);

    let (machine, column_allocator) = optimizer::optimize::<_, _, _, A::MemoryBusInteraction<_>>(
        machine,
        vm_config.bus_interaction_handler,
        degree_bound,
        &vm_config.bus_map,
        column_allocator,
        &mut export_options,
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

    if export_options.export_requested() {
        export_options.export_apc::<A>(&apc, None, &vm_config.bus_map);
    }

    let apc = convert_apc_field_type(apc, &A::into_field);

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
