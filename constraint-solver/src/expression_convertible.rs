use std::ops::{Add, Mul, Neg, Sub};

pub trait ExpressionConvertible<T, V> {
    /// Converts `self` into an algebraic expression over its variables.
    ///
    /// The leafs of the algebraic expression are "variables" of type `V`
    /// and numbers of type `T`.
    ///
    /// Fails in case a non-algebraic operation is used.
    fn try_to_expression<
        E: From<T>
            + Add<E, Output = E>
            + Sub<E, Output = E>
            + Mul<E, Output = E>
            + Neg<Output = E>
            + TryInto<T>,
    >(
        &self,
        var_converter: &impl Fn(&V) -> E,
    ) -> Option<E>;
}
