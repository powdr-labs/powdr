use itertools::Itertools;
use powdr_constraint_solver::constraint_system::ComputationMethod;
use powdr_number::ExpressionConvertible;
use rayon::prelude::*;
use std::collections::{BTreeMap, HashMap};
use std::fmt::Display;
use std::{cmp::Eq, hash::Hash};

use crate::blocks::PcStep;
use crate::expression::AlgebraicReference;
use crate::{Apc, InstructionHandler};
use powdr_expression::AlgebraicExpression;

pub struct OriginalRowReference<'a, D> {
    pub data: &'a D,
    pub start: usize,
    pub length: usize,
}

/// A location in one apc call's dummy trace: instruction row and column within it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DummyCoord {
    pub instruction: usize,
    pub index: usize,
}

/// A derived column's computation method with its references resolved to `DummyCoord`s.
pub type ResolvedMethod<F> = ComputationMethod<F, AlgebraicExpression<F, DummyCoord>>;

pub struct TraceData<'a, F, D> {
    /// For each call of the apc, the values of each original instruction's dummy trace.
    pub dummy_values: Vec<Vec<OriginalRowReference<'a, D>>>,
    /// The mapping from dummy trace index to APC index for each instruction (surviving columns,
    /// copied directly into the APC row).
    pub dummy_trace_index_to_apc_index_by_instruction: Vec<Vec<(usize, usize)>>,
    /// The mapping from poly_id to the index in the list of apc columns.
    /// The values are always unique and contiguous.
    pub apc_poly_id_to_index: BTreeMap<u64, usize>,
    /// `is_new` columns to fill: each is `(APC row index, method)`, with references resolved to
    /// `DummyCoord`s so witgen reads inputs straight from the dummy trace.
    pub columns_to_compute: Vec<(usize, ResolvedMethod<F>)>,
}

/// Rewrite a computation method's poly-id references to their dummy-trace coordinates. Panics if a
/// reference isn't backed by the dummy trace; derived columns only reference substituted columns, so
/// it can't happen.
pub fn resolve_computation_method<F: Clone>(
    method: &ComputationMethod<F, AlgebraicExpression<F, AlgebraicReference>>,
    apc_poly_id_to_dummy_index: &BTreeMap<u64, DummyCoord>,
) -> ResolvedMethod<F> {
    method
        .clone()
        .convert_expression_type(&|e: AlgebraicExpression<F, AlgebraicReference>| {
            e.to_expression(
                &|n: &F| AlgebraicExpression::Number(n.clone()),
                &|r: &AlgebraicReference| {
                    let coord = apc_poly_id_to_dummy_index.get(&r.id).unwrap_or_else(|| {
                        panic!(
                            "derived column references poly id {} which is not backed by the \
                             original instruction trace",
                            r.id
                        )
                    });
                    AlgebraicExpression::Reference(*coord)
                },
            )
        })
}

pub trait TraceTrait<F>: Send + Sync {
    type Values: Send + Sync;

    fn width(&self) -> usize;

    fn values(&self) -> &Self::Values;
}

// TODO: refactor `Apc` so we don't have to pass A, V here
pub fn generate_trace<'a, IH, M: TraceTrait<IH::Field>, A, V>(
    air_id_to_dummy_trace: &'a HashMap<IH::AirId, M>,
    instruction_handler: &'a IH,
    apc_call_count: usize,
    apc: &'a Apc<IH::Field, IH::Instruction, A, V>,
) -> TraceData<'a, IH::Field, M::Values>
where
    IH: InstructionHandler,
    IH::Field: Display + Clone + Send + Sync,
    IH::AirId: Eq + Hash + Send + Sync,
    IH::Instruction: PcStep,
{
    // Keep only instructions that produce dummy records
    let instructions_with_subs = apc
        .instructions()
        .zip_eq(apc.subs.iter())
        .filter(|(_, subs)| !subs.is_empty());
    let instructions_with_subs = instructions_with_subs.collect::<Vec<_>>();

    let original_instruction_air_ids = instructions_with_subs
        .iter()
        .map(|(instruction, _)| {
            instruction_handler
                .get_instruction_air_and_id(instruction)
                .0
        })
        .collect::<Vec<_>>();

    let air_id_occurrences = original_instruction_air_ids.iter().counts();

    let apc_poly_id_to_index: BTreeMap<u64, usize> = apc
        .machine
        .main_columns()
        .enumerate()
        .map(|(index, c)| (c.id, index))
        .collect();

    let original_instruction_table_offsets = original_instruction_air_ids
        .iter()
        .scan(
            HashMap::default(),
            |counts: &mut HashMap<&IH::AirId, usize>, air_id| {
                let count = counts.entry(air_id).or_default();
                let current_count = *count;
                *count += 1;
                Some(current_count)
            },
        )
        .collect::<Vec<_>>();

    // Per instruction, the surviving columns' `(dummy index, APC index)` for the row copy; and, keyed
    // by poly_id, every substituted column's dummy location — used just below to resolve derived
    // columns, then dropped (not returned).
    let mut dummy_trace_index_to_apc_index_by_instruction: Vec<Vec<(usize, usize)>> =
        Vec::with_capacity(instructions_with_subs.len());
    let mut apc_poly_id_to_dummy_index: BTreeMap<u64, DummyCoord> = BTreeMap::new();
    for (instruction_index, (_, subs)) in instructions_with_subs.iter().enumerate() {
        let mut surviving = Vec::new();
        for substitution in subs.iter() {
            apc_poly_id_to_dummy_index.insert(
                substitution.apc_poly_id,
                DummyCoord {
                    instruction: instruction_index,
                    index: substitution.original_poly_index,
                },
            );
            if let Some(index) = apc_poly_id_to_index.get(&substitution.apc_poly_id) {
                surviving.push((substitution.original_poly_index, *index));
            }
        }
        dummy_trace_index_to_apc_index_by_instruction.push(surviving);
    }

    let dummy_values = (0..apc_call_count)
        .into_par_iter()
        .map(|trace_row| {
            original_instruction_air_ids
                .iter()
                .zip_eq(original_instruction_table_offsets.iter())
                .map(|(air_id, dummy_table_offset)| {
                    let trace = air_id_to_dummy_trace.get(air_id).unwrap();
                    let values = trace.values();
                    let width = trace.width();
                    let occurrences_per_record = air_id_occurrences.get(air_id).unwrap();
                    let start = (trace_row * occurrences_per_record + dummy_table_offset) * width;
                    OriginalRowReference {
                        data: values,
                        start,
                        length: width,
                    }
                })
                .collect_vec()
        })
        .collect();

    // Pre-resolve the `is_new` derived columns: each carries its APC row index and a computation
    // method whose references point straight at the dummy trace, so witgen needs neither the
    // `apc_poly_id_to_dummy_index` map nor a per-row lookup.
    let columns_to_compute = apc
        .machine
        .derived_columns
        .iter()
        .filter(|d| d.is_new)
        .map(|d| {
            (
                apc_poly_id_to_index[&d.variable.id],
                resolve_computation_method(&d.computation_method, &apc_poly_id_to_dummy_index),
            )
        })
        .collect();

    TraceData {
        dummy_values,
        dummy_trace_index_to_apc_index_by_instruction,
        apc_poly_id_to_index,
        columns_to_compute,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;

    /// Resolving a reference not backed by the dummy trace panics legibly. (`u64` field: the resolver
    /// only rewrites references, never evaluates.)
    #[test]
    #[should_panic(expected = "not backed by the original")]
    fn resolve_panics_on_column_not_in_dummy_trace() {
        let method: ComputationMethod<u64, AlgebraicExpression<u64, AlgebraicReference>> =
            ComputationMethod::QuotientOrZero(
                AlgebraicExpression::Reference(AlgebraicReference {
                    name: Arc::new("c830".to_string()),
                    id: 830,
                }),
                AlgebraicExpression::Number(1),
            );
        let empty: BTreeMap<u64, DummyCoord> = BTreeMap::new();
        let _ = resolve_computation_method::<u64>(&method, &empty);
    }
}
