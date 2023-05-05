use num_traits::Zero;
use std::{
    fmt,
    ops::{Add, AddAssign, Div, Mul, Neg, Sub},
};

use crate::{AbstractNumberType, DegreeType};
pub trait FieldElementTrait:
    Zero
    + Add
    + for<'a> Add<&'a Self>
    + AddAssign
    + Sub
    + for<'a> Sub<&'a Self>
    + Mul
    + for<'a> Mul<&'a Self>
    + Div
    + for<'a> Div<&'a Self>
    + Neg
    + fmt::Display
{
    fn to_degree(&self) -> DegreeType;

    fn to_integer(&self) -> AbstractNumberType;

    fn modulus() -> AbstractNumberType;

    fn pow(self, exponent: AbstractNumberType) -> Self;

    fn integer_div(self, other: Self) -> Self;

    fn to_bytes_le(&self) -> Vec<u8>;
}
