use std::ops::{Add, Mul, Neg, Sub};

use crate::FieldElement;

pub trait ExpressionConvertible<T, V> {
    /// Converts `self` into a structure that supports algebraic operations.
    ///
    /// Fails in case a non-algebraic operation is used.
    ///
    /// The `try_to_number` function is used to check if some conversions can be simplified.
    ///
    /// This or `to_expression` must be implemented.
    fn try_to_expression<
        E: Add<E, Output = E> + Sub<E, Output = E> + Mul<E, Output = E> + Neg<Output = E>,
    >(
        &self,
        number_converter: &impl Fn(&T) -> E,
        var_converter: &impl Fn(&V) -> E,
        _try_to_number: &impl Fn(&E) -> Option<T>,
    ) -> Option<E> {
        Some(self.to_expression(number_converter, var_converter))
    }

    /// Converts `self` into a structure that supports algebraic operations.
    ///
    /// This or `try_to_expression` must be implemented.
    fn to_expression<
        E: Add<E, Output = E> + Sub<E, Output = E> + Mul<E, Output = E> + Neg<Output = E>,
    >(
        &self,
        number_converter: &impl Fn(&T) -> E,
        var_converter: &impl Fn(&V) -> E,
    ) -> E {
        self.try_to_expression(number_converter, var_converter, &|_| unreachable!())
            .unwrap()
    }
}

impl<V, T: FieldElement> ExpressionConvertible<T, V> for T {
    fn to_expression<
        E: Add<E, Output = E> + Sub<E, Output = E> + Mul<E, Output = E> + Neg<Output = E>,
    >(
        &self,
        number_converter: &impl Fn(&T) -> E,
        _var_converter: &impl Fn(&V) -> E,
    ) -> E {
        number_converter(self)
    }
}
