use std::collections::HashMap;

use ast::analyzed::{IdentityKind, PolynomialReference, SelectedExpressions};
use number::FieldElement;

pub use self::fixed_lookup_machine::FixedLookup;

use super::affine_expression::AffineResult;
use super::EvalError;
use super::EvalResult;
use super::EvalValue;
use super::FixedData;

mod block_machine;
mod double_sorted_witness_machine;
mod fixed_lookup_machine;
pub mod machine_extractor;
mod sorted_witness_machine;

/// A machine is a set of witness columns and identities where the columns
/// are used on the right-hand-side of lookups. It can process plookups.
pub trait Machine<T: FieldElement>: Send + Sync {
    // /// Tries to construct a new machine with the given subset of
    // /// witness columns and identities. If the identities do not
    // /// fit the pattern of the machine type, it can return None.
    // fn try_new(
    //     fixed_data: &'a FixedData<'a>,
    //     identities: Vec<&'a Identity>,
    //     witness_names: HashSet<&'a str>,
    // ) -> Option<Box<Self>>;

    /// Process a plookup. Not all values on the LHS need to be available.
    /// Can update internal data.
    /// Only return an error if this machine is able to handle the query and
    /// it results in a constraint failure.
    /// If this is not the right machine for the query, return `None`.
    fn process_plookup<'a>(
        &mut self,
        fixed_data: &FixedData<T>,
        fixed_lookup: &mut FixedLookup<T>,
        kind: IdentityKind,
        left: &[AffineResult<&'a PolynomialReference, T>],
        right: &SelectedExpressions<T>,
    ) -> Option<PlookupResult<'a, T>>;

    /// Returns the final values of the witness columns.
    fn witness_col_values(&mut self, fixed_data: &FixedData<T>) -> HashMap<String, Vec<T>>;
}

/// Represents an assignment together with a number of new rows generated for performance measurement purposes.
pub type PlookupResult<'a, T, K = &'a PolynomialReference> =
    Result<(EvalValue<K, T>, usize), EvalError>;
