use powdr_constraint_solver::constraint_system::ComputationMethod;
use powdr_number::ExpressionConvertible;
use std::collections::BTreeMap;

use crate::expression::AlgebraicReference;
use powdr_expression::AlgebraicExpression;

pub struct OriginalRowReference<'a, D> {
    pub data: &'a D,
    pub start: usize,
    pub length: usize,
}

/// A location in one apc call's dummy trace: instruction row and column within it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
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

pub trait TraceTrait<F>: Send + Sync {
    type Values: Send + Sync;

    fn width(&self) -> usize;

    fn values(&self) -> &Self::Values;
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
