use std::collections::HashMap;

use ast::analyzed::IdentityKind;
use ast::analyzed::PolynomialReference;
use ast::analyzed::SelectedExpressions;
use number::FieldElement;

use self::block_machine::BlockMachine;
use self::double_sorted_witness_machine::DoubleSortedWitnesses;
pub use self::fixed_lookup_machine::FixedLookup;
use self::sorted_witness_machine::SortedWitnesses;

use super::affine_expression::AffineResult;
use super::EvalResult;
use super::FixedData;

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
    fn process_plookup(
        &mut self,
        fixed_data: &'a FixedData<T>,
        fixed_lookup: &mut FixedLookup<T>,
        kind: IdentityKind,
        left: &[AffineResult<&'a PolynomialReference, T>],
        right: &'a SelectedExpressions<T>,
        machines: Vec<&mut KnownMachine<'a, T>>,
    ) -> Option<EvalResult<'a, T>>;

    /// Returns the final values of the witness columns.
    fn take_witness_col_values(
        &mut self,
        fixed_data: &FixedData<T>,
        fixed_lookup: &mut FixedLookup<T>,
    ) -> HashMap<String, Vec<T>>;
}

/// All known implementations of [Machine].
/// This allows us to treat machines uniformly without putting them into a `Box`,
/// which requires that all lifetime parameters are 'static.
pub enum KnownMachine<'a, T: FieldElement> {
    SortedWitnesses(SortedWitnesses<T>),
    DoubleSortedWitnesses(DoubleSortedWitnesses<T>),
    BlockMachine(BlockMachine<'a, T>),
}

impl<'a, T: FieldElement> KnownMachine<'a, T> {
    fn get(&mut self) -> &mut dyn Machine<'a, T> {
        match self {
            KnownMachine::SortedWitnesses(m) => m,
            KnownMachine::DoubleSortedWitnesses(m) => m,
            KnownMachine::BlockMachine(m) => m,
        }
    }
}

impl<'a, T: FieldElement> Machine<'a, T> for KnownMachine<'a, T> {
    fn process_plookup(
        &mut self,
        fixed_data: &'a FixedData<T>,
        fixed_lookup: &mut FixedLookup<T>,
        kind: IdentityKind,
        left: &[AffineResult<&'a PolynomialReference, T>],
        right: &'a SelectedExpressions<T>,
        machines: Vec<&mut KnownMachine<'a, T>>,
    ) -> Option<crate::witgen::EvalResult<'a, T>> {
        self.get()
            .process_plookup(fixed_data, fixed_lookup, kind, left, right, machines)
    }

    fn take_witness_col_values(
        &mut self,
        fixed_data: &FixedData<T>,
        fixed_lookup: &mut FixedLookup<T>,
    ) -> std::collections::HashMap<String, Vec<T>> {
        self.get().take_witness_col_values(fixed_data, fixed_lookup)
    }
}
