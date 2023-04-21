use std::collections::HashMap;

use crate::analyzer::{IdentityKind, SelectedExpressions};
use number::FieldElement;

pub use self::fixed_lookup_machine::FixedLookup;

use super::EvalResult;
use super::{affine_expression::AffineExpression, eval_error::EvalError, FixedData};

mod block_machine;
mod double_sorted_witness_machine;
mod fixed_lookup_machine;
pub mod machine_extractor;
mod sorted_witness_machine;

/// A machine is a set of witness columns and identities where the columns
/// are used on the right-hand-side of lookups. It can process plookups.
pub trait Machine {
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
    fn process_plookup(
        &mut self,
        fixed_data: &FixedData,
        fixed_lookup: &mut FixedLookup,
        kind: IdentityKind,
        left: &[Result<AffineExpression, EvalError>],
        right: &SelectedExpressions,
    ) -> Option<EvalResult>;

    /// Returns the final values of the witness columns.
    fn witness_col_values(&mut self, fixed_data: &FixedData) -> HashMap<String, Vec<FieldElement>>;
}
