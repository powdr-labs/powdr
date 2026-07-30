use itertools::Itertools;
use powdr_constraint_solver::constraint_system::ComputationMethod;
use powdr_number::ExpressionConvertible;
use rayon::prelude::*;
use std::collections::{BTreeMap, HashMap};
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

/// Witness-independent APC trace-generation data, built once per APC (see [`CachedApc::build`])
/// rather than per shard. Only [`CachedApc::dummy_values`] depends on the execution records.
pub struct CachedApc<F, AirId> {
    /// Mapping from poly_id to the index in the list of apc columns. Unique and contiguous.
    pub apc_poly_id_to_index: BTreeMap<u64, usize>,
    /// Per instruction, the surviving columns' `(dummy index, APC index)` for the row copy.
    pub dummy_trace_index_to_apc_index_by_instruction: Vec<Vec<(usize, usize)>>,
    /// `is_new` columns to fill: each is `(APC row index, method)`, references resolved to
    /// `DummyCoord`s so witgen reads inputs straight from the dummy trace.
    pub columns_to_compute: Vec<(usize, ResolvedMethod<F>)>,
    /// Per instruction, its dummy trace `(air id, row offset in the air block, occurrences per call)`.
    dummy_layout: Vec<(AirId, usize, usize)>,
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

impl<F: Clone, AirId: Eq + Hash + Clone> CachedApc<F, AirId> {
    /// Precompute everything that does not depend on the execution records.
    // TODO: refactor `Apc` so we don't have to pass A, V here
    pub fn build<IH, A, V>(apc: &Apc<F, IH::Instruction, A, V>, instruction_handler: &IH) -> Self
    where
        IH: InstructionHandler<Field = F, AirId = AirId>,
        IH::Instruction: PcStep,
    {
        // Keep only instructions that produce dummy records.
        let instructions_with_subs = apc
            .instructions()
            .zip_eq(apc.subs.iter())
            .filter(|(_, subs)| !subs.is_empty())
            .collect::<Vec<_>>();

        let air_ids = instructions_with_subs
            .iter()
            .map(|(instruction, _)| {
                instruction_handler
                    .get_instruction_air_and_id(instruction)
                    .0
            })
            .collect::<Vec<_>>();

        let air_id_occurrences = air_ids.iter().counts();

        let apc_poly_id_to_index: BTreeMap<u64, usize> = apc
            .machine
            .main_columns()
            .enumerate()
            .map(|(index, c)| (c.id, index))
            .collect();

        let dummy_layout = air_ids
            .iter()
            .scan(
                HashMap::default(),
                |counts: &mut HashMap<&AirId, usize>, air_id| {
                    let offset = counts.entry(air_id).or_default();
                    let row_offset = *offset;
                    *offset += 1;
                    Some((air_id.clone(), row_offset, air_id_occurrences[air_id]))
                },
            )
            .collect();

        // Per instruction, surviving columns' copy pairs; and, keyed by poly_id, every substituted
        // column's dummy location — used just below to resolve derived columns, then dropped.
        let mut dummy_trace_index_to_apc_index_by_instruction =
            Vec::with_capacity(instructions_with_subs.len());
        let mut apc_poly_id_to_dummy_index: BTreeMap<u64, DummyCoord> = BTreeMap::new();
        for (instruction, (_, subs)) in instructions_with_subs.iter().enumerate() {
            let mut surviving = Vec::new();
            for sub in subs.iter() {
                apc_poly_id_to_dummy_index.insert(
                    sub.apc_poly_id,
                    DummyCoord {
                        instruction,
                        index: sub.original_poly_index,
                    },
                );
                if let Some(index) = apc_poly_id_to_index.get(&sub.apc_poly_id) {
                    surviving.push((sub.original_poly_index, *index));
                }
            }
            dummy_trace_index_to_apc_index_by_instruction.push(surviving);
        }

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

        Self {
            apc_poly_id_to_index,
            dummy_trace_index_to_apc_index_by_instruction,
            columns_to_compute,
            dummy_layout,
        }
    }
}

impl<F, AirId: Eq + Hash + Sync> CachedApc<F, AirId> {
    /// For each apc call, a reference into the dummy trace of each original instruction.
    pub fn dummy_values<'a, M: TraceTrait<F>>(
        &self,
        air_id_to_dummy_trace: &'a HashMap<AirId, M>,
        apc_call_count: usize,
    ) -> Vec<Vec<OriginalRowReference<'a, M::Values>>> {
        let dummy_layout = &self.dummy_layout;
        (0..apc_call_count)
            .into_par_iter()
            .map(|trace_row| {
                dummy_layout
                    .iter()
                    .map(|(air_id, row_offset, occurrences)| {
                        let trace = air_id_to_dummy_trace.get(air_id).unwrap();
                        let width = trace.width();
                        let start = (trace_row * occurrences + row_offset) * width;
                        OriginalRowReference {
                            data: trace.values(),
                            start,
                            length: width,
                        }
                    })
                    .collect()
            })
            .collect()
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
