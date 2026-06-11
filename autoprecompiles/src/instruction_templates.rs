//! Pre-optimization of single-instruction machines ("instruction templates").
//!
//! The same instruction encoding usually appears many times across the
//! autoprecompile candidates of a program. Instead of starting every candidate
//! from the raw instruction AIRs, each distinct encoding is optimized once,
//! independently of the block it appears in, and candidates are assembled from
//! the pre-optimized machines. The block-level optimizer then only has to do
//! the cross-instruction work (execution-bus chaining, memory pairing,
//! inlining, range constraint batching).

use std::collections::HashMap;
use std::sync::{Arc, Mutex};

use itertools::Itertools;
use powdr_constraint_solver::constraint_system::ComputationMethod;
use powdr_expression::visitors::ExpressionVisitable;
use powdr_number::FieldElement;

use crate::adapter::Adapter;
use crate::blocks::{Instruction, PcStep};
use crate::expression::{AlgebraicExpression, AlgebraicReference};
use crate::powdr::UniqueReferences;
use crate::symbolic_machine::SymbolicConstraint;
use crate::symbolic_machine_generator::{convert_machine_field_type, exec_receive};
use crate::{BusMap, BusType, ColumnAllocator, DegreeBound, InstructionHandler, SymbolicMachine};

/// The pc lookup row of an instruction, with the pc-dependent positions masked
/// out. Since the row consists of the pc and the full instruction encoding,
/// this uniquely identifies the (pc-independent part of the) machine built for
/// an instruction.
type TemplateKey<T> = Vec<Option<T>>;

type TemplateCache<T> = Mutex<HashMap<TemplateKey<T>, Arc<Template<T>>>>;

/// A pre-optimized single-instruction machine.
struct Template<T> {
    /// The optimized machine, with column ids in `0..id_space`. The ids of the
    /// raw AIR columns (`0..raw_width`) are preserved, so the per-instruction
    /// column substitutions are the same as for an unoptimized machine.
    machine: SymbolicMachine<T>,
    /// The size of the id space of `machine`: the raw AIR columns plus any
    /// columns introduced by the optimizer (not all of them survive).
    id_space: u64,
    /// The number of columns of the raw instruction AIR.
    raw_width: u64,
}

/// A cache of pre-optimized single-instruction machines, keyed by the
/// instruction encoding. One instance is shared across all candidates of a
/// pipeline run (the templates are only valid for a fixed bus map and degree
/// bound).
pub struct InstructionTemplates<A: Adapter> {
    cache: TemplateCache<A::PowdrField>,
    bus_interaction_handler: A::BusInteractionHandler,
    degree_bound: DegreeBound,
}

impl<A: Adapter> InstructionTemplates<A> {
    pub fn new(
        bus_interaction_handler: A::BusInteractionHandler,
        degree_bound: DegreeBound,
    ) -> Self {
        Self {
            cache: Mutex::new(HashMap::new()),
            bus_interaction_handler,
            degree_bound,
        }
    }

    /// Returns the machine for the instruction at `pc`, instantiated from the
    /// (possibly newly built) template for its encoding:
    /// - column ids are offset by `next_poly_id` and column names are suffixed
    ///   with `_{index}`, like `powdr::globalize_references` does;
    /// - the pc-dependent pc-lookup arguments are pinned to their values at `pc`.
    ///
    /// Returns the machine, the per-instruction column substitutions and the
    /// updated next poly id.
    pub(crate) fn instantiate(
        &self,
        instr: &A::Instruction,
        pc: u64,
        index: usize,
        next_poly_id: u64,
        instruction_handler: &A::InstructionHandler,
        bus_map: &BusMap<A::CustomBusTypes>,
    ) -> (SymbolicMachine<A::PowdrField>, Vec<u64>, u64) {
        let row = self.pc_lookup_row(instr, pc);
        // Detect the pc-dependent positions of the row by probing a second pc.
        // (Assumes that a position whose value coincides at the two pcs is
        // independent of the pc, which holds for rows of the form
        // `[pc, encoding...]`.)
        let step = u64::from(<A::Instruction as PcStep>::pc_step());
        let probe_pc = pc.checked_sub(step).unwrap_or(pc + step);
        let probe_row = self.pc_lookup_row(instr, probe_pc);
        let key: TemplateKey<A::PowdrField> = row
            .iter()
            .zip_eq(&probe_row)
            .map(|(a, b)| (a == b).then_some(*a))
            .collect();

        let template = self.get_or_build(&key, instr, instruction_handler, bus_map);

        let mut machine = relabel_machine(template.machine.clone(), next_poly_id, index);

        // Pin the pc-dependent pc lookup arguments to their values at `pc`.
        let pc_lookup = machine
            .bus_interactions
            .iter()
            .filter(|bus_int| bus_int.id == bus_map.get_bus_id(&BusType::PcLookup).unwrap())
            .exactly_one()
            .expect("Expected the template to retain a single pc lookup");
        let pin_constraints: Vec<SymbolicConstraint<_>> = pc_lookup
            .args
            .iter()
            .zip_eq(&key)
            .zip_eq(&row)
            .filter(|((_, masked), _)| masked.is_none())
            .map(|((arg, _), value)| (arg.clone() - (*value).into()).into())
            .collect();
        machine.constraints.extend(pin_constraints);

        let subs = (next_poly_id..next_poly_id + template.raw_width).collect();
        (machine, subs, next_poly_id + template.id_space)
    }

    fn pc_lookup_row(&self, instr: &A::Instruction, pc: u64) -> Vec<A::PowdrField> {
        instr
            .pc_lookup_row(pc)
            .into_iter()
            .map(|x| A::from_field(x))
            .collect()
    }

    fn get_or_build(
        &self,
        key: &TemplateKey<A::PowdrField>,
        instr: &A::Instruction,
        instruction_handler: &A::InstructionHandler,
        bus_map: &BusMap<A::CustomBusTypes>,
    ) -> Arc<Template<A::PowdrField>> {
        if let Some(template) = self.cache.lock().unwrap().get(key) {
            return template.clone();
        }
        // The lock is not held while building, so concurrent workers may build
        // the same template; the result is deterministic, so we can keep either.
        let template = Arc::new(self.build_template(key, instr, instruction_handler, bus_map));
        self.cache
            .lock()
            .unwrap()
            .entry(key.clone())
            .or_insert(template)
            .clone()
    }

    /// Builds the machine for the instruction with all pc-independent pc-lookup
    /// arguments pinned (the pc-dependent ones are pinned per instance) and
    /// optimizes it.
    fn build_template(
        &self,
        key: &TemplateKey<A::PowdrField>,
        instr: &A::Instruction,
        instruction_handler: &A::InstructionHandler,
        bus_map: &BusMap<A::CustomBusTypes>,
    ) -> Template<A::PowdrField> {
        let machine = instruction_handler
            .get_instruction_air_and_id(instr)
            .1
            .clone();
        let mut machine: SymbolicMachine<A::PowdrField> =
            convert_machine_field_type(machine, &|x| A::from_field(x));

        let reference_ids = machine.unique_references().map(|r| r.id).collect_vec();
        let raw_width = reference_ids.len() as u64;
        assert_eq!(
            *reference_ids.iter().max().unwrap(),
            raw_width - 1,
            "The reference ids must be contiguous"
        );

        // Constrain `is_valid` to be 1, like `statements_to_symbolic_machines`
        // does for the non-template path.
        let minus_is_valid: AlgebraicExpression<_> = exec_receive(
            &machine,
            bus_map.get_bus_id(&BusType::ExecutionBridge).unwrap(),
        )
        .mult
        .clone();
        let one = AlgebraicExpression::Number(1u64.into());
        machine.constraints.push((minus_is_valid + one).into());

        // Pin the pc-independent pc lookup arguments.
        let pc_lookup = machine
            .bus_interactions
            .iter()
            .filter(|bus_int| bus_int.id == bus_map.get_bus_id(&BusType::PcLookup).unwrap())
            .exactly_one()
            .expect("Expected single pc lookup");
        let pin_constraints: Vec<SymbolicConstraint<_>> = pc_lookup
            .args
            .iter()
            .zip_eq(key)
            .filter_map(|(arg, value)| value.map(|value| (arg.clone() - value.into()).into()))
            .collect();
        machine.constraints.extend(pin_constraints);

        let column_allocator = ColumnAllocator {
            subs: Vec::new(),
            next_poly_id: raw_width,
        };
        let (machine, id_space) = match crate::optimizer::optimize_instruction_machine::<
            _,
            _,
            A::MemoryBusInteraction<_>,
        >(
            machine.clone(),
            self.bus_interaction_handler.clone(),
            self.degree_bound,
            column_allocator,
        ) {
            Ok((optimized, column_allocator)) => {
                // The block-level pipeline relies on the bus interface of the
                // instruction: the pc lookup (pinned per instance and removed
                // at block level) and the execution bridge receive/send pair.
                let pc_lookup_bus_id = bus_map.get_bus_id(&BusType::PcLookup).unwrap();
                assert_eq!(
                    optimized
                        .bus_interactions
                        .iter()
                        .filter(|b| b.id == pc_lookup_bus_id)
                        .count(),
                    1,
                    "Expected the template optimizer to retain the pc lookup"
                );
                (optimized, column_allocator.next_poly_id)
            }
            Err(e) => {
                tracing::warn!(
                    "Instruction template optimization failed ({e:?}); using the unoptimized instruction machine"
                );
                (machine, raw_width)
            }
        };

        Template {
            machine,
            id_space,
            raw_width,
        }
    }
}

/// Offsets all column ids of the machine by `offset` and suffixes all column
/// names with `_{suffix}`, including the derived columns (which
/// `SymbolicMachine`'s expression visitor does not reach).
fn relabel_machine<T: FieldElement>(
    mut machine: SymbolicMachine<T>,
    offset: u64,
    suffix: usize,
) -> SymbolicMachine<T> {
    let relabel = |r: &mut AlgebraicReference| {
        r.name = Arc::new(format!("{}_{suffix}", r.name));
        r.id += offset;
    };
    let mut relabel_expr = |e: &mut AlgebraicExpression<T>| {
        if let AlgebraicExpression::Reference(r) = e {
            relabel(r);
        }
    };
    machine.pre_visit_expressions_mut(&mut relabel_expr);
    for derived in &mut machine.derived_columns {
        relabel(&mut derived.variable);
        match &mut derived.computation_method {
            ComputationMethod::Constant(_) => {}
            ComputationMethod::QuotientOrZero(e1, e2) => {
                e1.pre_visit_expressions_mut(&mut relabel_expr);
                e2.pre_visit_expressions_mut(&mut relabel_expr);
            }
        }
    }
    machine
}
