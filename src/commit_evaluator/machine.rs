use std::collections::HashMap;

use crate::{analyzer::SelectedExpressions, number::AbstractNumberType};

use super::{affine_expression::AffineExpression, eval_error::EvalError, FixedData};

/// A machine is a set of witness columns and identities where the columns
/// are used on the righ-hand-side of lookups. It can process plookups.
pub trait Machine {
    // /// Tries to construct a new machine with the given subset of
    // /// witness columns and identities. If the identities do not
    // /// fit the pattern of the machine type, it can return None.
    // fn try_new(
    //     fixed_data: &'a FixedData<'a>,
    //     identities: Vec<&'a Identity>,
    //     witness_names: HashSet<&'a String>,
    // ) -> Option<Box<Self>>;

    /// Process o plookup. Not all values on the LHS need to be available.
    /// Can update internal data.
    fn process_plookup(
        &mut self,
        fixed_data: &FixedData,
        left: &[Option<AffineExpression>],
        right: &SelectedExpressions,
    ) -> LookupResult;

    /// Returns the final values of the witness columns.
    fn witness_col_values(
        &mut self,
        fixed_data: &FixedData,
    ) -> HashMap<String, Vec<AbstractNumberType>>;
}

pub type LookupResult = Result<LookupReturn, EvalError>;

pub enum LookupReturn {
    /// The query is not applicable to this machine type.
    NotApplicable,
    /// The machne type can fully handle this query and it is
    /// (partially) satisfied with the given assignments.
    Assignments(Vec<(usize, AbstractNumberType)>),
}
