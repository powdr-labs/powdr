use powdr_number::FieldElement;

use crate::witgen::{identity_processor::Machines, QueryCallback};

/// Everything [Generator] needs to mutate in order to compute a new row.
pub struct MutableState<'a, 'b, T: FieldElement, Q: QueryCallback<T>> {
    pub machines: Machines<'a, 'b, T>,
    pub query_callback: &'b mut Q,
}
