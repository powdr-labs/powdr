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
#[derive(Debug)]
pub struct ColumnAllocator {
    /// For each original air, for each original column index, the associated poly_id in the APC air
    pub subs: Vec<Vec<u64>>,
    /// The next poly_id to issue
    next_poly_id: u64,
}

impl ColumnAllocator {
    fn issue_next_poly_id(&mut self) -> u64 {
        let id = self.next_poly_id;
        self.next_poly_id += 1;
        id
    }
}

#[derive(Serialize, Deserialize)]
pub struct ExecutionStats {
    pub air_id_by_pc: BTreeMap<u32, usize>,
    pub column_names_by_air_id: BTreeMap<usize, Vec<String>>,
    pub column_ranges_by_pc: BTreeMap<u32, Vec<(u32, u32)>>,
    pub equivalence_classes_by_block: BTreeMap<u64, Vec<Vec<(usize, usize)>>>,
}

fn add_ai_constraints<A: Adapter>(
    execution_stats: &ExecutionStats,
    subs: &[Vec<u64>],
    block: &BasicBlock<A::Instruction>,
    columns: impl Iterator<Item = AlgebraicReference>,
) -> (
    Vec<SymbolicConstraint<A::PowdrField>>,
    Vec<SymbolicConstraint<A::PowdrField>>,
) {
    let range_constraints = &execution_stats.column_ranges_by_pc;
    let equivalence_classes_by_block = &execution_stats.equivalence_classes_by_block;

    let mut range_analyzer_constraints = Vec::new();
    let mut equivalence_analyzer_constraints = Vec::new();

    // Mapping (instruction index, column index) -> AlgebraicReference
    let reverse_subs = subs
        .iter()
        .enumerate()
        .flat_map(|(instr_index, subs)| {
            subs.iter()
                .enumerate()
                .map(move |(col_index, &poly_id)| (poly_id, (instr_index, col_index)))
        })
        .collect::<BTreeMap<_, _>>();
    let algebraic_references = columns
        .map(|r| (reverse_subs.get(&r.id).unwrap().clone(), r.clone()))
        .collect::<BTreeMap<_, _>>();

    for i in 0..block.statements.len() {
        let pc = (block.start_pc + (i * 4) as u64) as u32;
        let Some(range_constraints) = range_constraints.get(&pc) else {
            continue;
        };
        for (col_index, range) in range_constraints.iter().enumerate() {
            if range.0 == range.1 {
                let value = A::PowdrField::from(range.0 as u64);
                let Some(reference) = algebraic_references.get(&(i, col_index)).cloned() else {
                    println!("Missing reference for (i: {}, col_index: {})", i, col_index);
                    continue;
                };
                let constraint =
                    AlgebraicExpression::Reference(reference) - AlgebraicExpression::Number(value);

                range_analyzer_constraints.push(SymbolicConstraint { expr: constraint });
            }
        }
    }

    if let Some(equivalence_classes) = equivalence_classes_by_block.get(&block.start_pc) {
        for equivalence_class in equivalence_classes {
            let first = equivalence_class.first().unwrap();
            let Some(first_ref) = algebraic_references.get(first).cloned() else {
                println!(
                    "Missing reference for (i: {}, col_index: {})",
                    first.0, first.1
                );
                continue;
            };
            for other in equivalence_class.iter().skip(1) {
                let Some(other_ref) = algebraic_references.get(other).cloned() else {
                    println!(
                        "Missing reference for (i: {}, col_index: {})",
                        other.0, other.1
                    );
                    continue;
                };
                let constraint = AlgebraicExpression::Reference(first_ref.clone())
                    - AlgebraicExpression::Reference(other_ref.clone());
                equivalence_analyzer_constraints.push(SymbolicConstraint { expr: constraint });
            }
        }
    }

    (range_analyzer_constraints, equivalence_analyzer_constraints)
}

pub fn build<A: Adapter>(
    block: BasicBlock<A::Instruction>,
    vm_config: AdapterVmConfig<A>,
    degree_bound: DegreeBound,
    apc_candidates_dir_path: Option<&Path>,
    execution_stats: &ExecutionStats,
) -> Result<AdapterApc<A>, crate::constraint_optimizer::Error> {
    let start = std::time::Instant::now();

    let (mut machine, column_allocator) = statements_to_symbolic_machine::<A>(
        &block,
        vm_config.instruction_handler,
        &vm_config.bus_map,
    );

    let (range_analyzer_constraints, equivalence_analyzer_constraints) = add_ai_constraints::<A>(
        execution_stats,
        &column_allocator.subs,
        &block,
        machine.main_columns(),
    );

    let labels = [("apc_start_pc", block.start_pc.to_string())];
    metrics::counter!("before_opt_cols", &labels)
        .absolute(machine.unique_references().count() as u64);
    metrics::counter!("before_opt_constraints", &labels)
        .absolute(machine.unique_references().count() as u64);
    metrics::counter!("before_opt_interactions", &labels)
        .absolute(machine.unique_references().count() as u64);

    let machine = optimizer::optimize::<A>(
        machine,
        vm_config.bus_interaction_handler,
        degree_bound,
        &vm_config.bus_map,
    )
    .unwrap();

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

    // TODO: Why do we need this?
    if pre_degree != 0 {
        assert_eq!(
            pre_degree,
            machine.degree(),
            "Degree should not change after adding guards, but changed from {} to {}",
            pre_degree,
            machine.degree(),
        );
    }

    // This needs to be added after the assertion above because it's a quadratic constraint
    // so it may increase the degree of the machine.
    machine.constraints.push(powdr::make_bool(is_valid).into());

    (machine, column_allocator)
}
