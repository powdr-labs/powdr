use std::ops::{Add, Mul, Neg, Sub};

pub trait ExpressionConvertible<T, V> {
    /// Converts `self` into an algebraic expression over its variables.
    ///
    /// The leafs of the algebraic expression are "variables" of type `V`
    /// and numbers of type `T`.
    ///
    /// Fails in case a non-algebraic operation is used.
    ///
    /// The `try_to_number` function is used to check if some conversions can be simplified.
    fn try_to_expression<
        E: Add<E, Output = E> + Sub<E, Output = E> + Mul<E, Output = E> + Neg<Output = E>,
    >(
        &self,
        number_converter: &impl Fn(&T) -> E,
        var_converter: &impl Fn(&V) -> E,
        try_to_number: &impl Fn(&E) -> Option<T>,
    ) -> Option<E>;
}
