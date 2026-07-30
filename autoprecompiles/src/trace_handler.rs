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

pub trait TraceTrait: Send + Sync {
    type Values: Send + Sync;

    fn width(&self) -> usize;

    fn values(&self) -> &Self::Values;
}

/// Per instruction (with substitutions), its dummy trace `(air id, row offset within the air's
/// block, occurrences per apc call)`. Needs the instruction handler, so it is computed by the trace
/// generator rather than cached on the APC (which is backend- and handler-agnostic).
pub fn dummy_layout<IH, A, V>(
    apc: &Apc<IH::Field, IH::Instruction, A, V>,
    instruction_handler: &IH,
) -> Vec<(IH::AirId, usize, usize)>
where
    IH: InstructionHandler,
    IH::AirId: Eq + Hash + Clone,
    IH::Instruction: PcStep,
{
    let air_ids = apc
        .instructions()
        .zip_eq(apc.subs())
        .filter(|(_, subs)| !subs.is_empty())
        .map(|(instruction, _)| {
            instruction_handler
                .get_instruction_air_and_id(instruction)
                .0
        })
        .collect::<Vec<_>>();

    let occurrences = air_ids.iter().counts();

    air_ids
        .iter()
        .scan(
            HashMap::default(),
            |counts: &mut HashMap<&IH::AirId, usize>, air_id| {
                let offset = counts.entry(air_id).or_default();
                let row_offset = *offset;
                *offset += 1;
                Some((air_id.clone(), row_offset, occurrences[air_id]))
            },
        )
        .collect()
}

/// For each apc call, a reference into the dummy trace of each original instruction, following
/// `layout` (see [`dummy_layout`]).
pub fn dummy_values<'a, M: TraceTrait, AirId: Eq + Hash + Sync>(
    layout: &[(AirId, usize, usize)],
    air_id_to_dummy_trace: &'a HashMap<AirId, M>,
    apc_call_count: usize,
) -> Vec<Vec<OriginalRowReference<'a, M::Values>>> {
    (0..apc_call_count)
        .into_par_iter()
        .map(|trace_row| {
            layout
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
