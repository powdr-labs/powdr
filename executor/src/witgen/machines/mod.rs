use std::collections::HashMap;

use ast::analyzed::AlgebraicExpression as Expression;
use ast::analyzed::AlgebraicReference;
use ast::parsed::SelectedExpressions;
use number::FieldElement;

use self::block_machine::BlockMachine;
use self::double_sorted_witness_machine::DoubleSortedWitnesses;
pub use self::fixed_lookup_machine::FixedLookup;
use self::sorted_witness_machine::SortedWitnesses;
use ast::analyzed::IdentityKind;

use super::affine_expression::AffineExpression;
use super::generator::Generator;
use super::EvalResult;
use super::FixedData;
use super::MutableState;
use super::QueryCallback;

mod block_machine;
mod double_sorted_witness_machine;
mod fixed_lookup_machine;
pub mod machine_extractor;
mod sorted_witness_machine;

/// A machine is a set of witness columns and identities where the columns
/// are used on the right-hand-side of lookups. It can process plookups.
pub trait Machine<'a, T: FieldElement>: Send + Sync {
    /// Process a plookup. Not all values on the LHS need to be available.
    /// Can update internal data.
    /// Only return an error if this machine is able to handle the query and
    /// it results in a constraint failure.
    /// If this is not the right machine for the query, return `None`.
    fn process_plookup<'b, Q: QueryCallback<T>>(
        &mut self,
        mutable_state: &'b mut MutableState<'a, 'b, T, Q>,
        kind: IdentityKind,
        left: &[AffineExpression<&'a AlgebraicReference, T>],
        right: &'a SelectedExpressions<Expression<T>>,
    ) -> Option<EvalResult<'a, T>>;

    /// Returns the final values of the witness columns.
    fn take_witness_col_values(&mut self) -> HashMap<String, Vec<T>>;
}

/// All known implementations of [Machine].
/// This allows us to treat machines uniformly without putting them into a `Box`,
/// which requires that all lifetime parameters are 'static.
pub enum KnownMachine<'a, T: FieldElement> {
    SortedWitnesses(SortedWitnesses<'a, T>),
    DoubleSortedWitnesses(DoubleSortedWitnesses<T>),
    BlockMachine(BlockMachine<'a, T>),
    Vm(Generator<'a, T>),
}

impl<'a, T: FieldElement> Machine<'a, T> for KnownMachine<'a, T> {
    fn process_plookup<'b, Q: QueryCallback<T>>(
        &mut self,
        mutable_state: &'b mut MutableState<'a, 'b, T, Q>,
        kind: IdentityKind,
        left: &[AffineExpression<&'a AlgebraicReference, T>],
        right: &'a SelectedExpressions<Expression<T>>,
    ) -> Option<crate::witgen::EvalResult<'a, T>> {
        match self {
            KnownMachine::SortedWitnesses(m) => m.process_plookup(mutable_state, kind, left, right),
            KnownMachine::DoubleSortedWitnesses(m) => {
                m.process_plookup(mutable_state, kind, left, right)
            }
            KnownMachine::BlockMachine(m) => m.process_plookup(mutable_state, kind, left, right),
            KnownMachine::Vm(m) => m.process_plookup(mutable_state, kind, left, right),
        }
    }

    fn take_witness_col_values(&mut self) -> std::collections::HashMap<String, Vec<T>> {
        match self {
            KnownMachine::SortedWitnesses(m) => m.take_witness_col_values(),
            KnownMachine::DoubleSortedWitnesses(m) => m.take_witness_col_values(),
            KnownMachine::BlockMachine(m) => m.take_witness_col_values(),
            KnownMachine::Vm(m) => m.take_witness_col_values(),
        }
    }
}
