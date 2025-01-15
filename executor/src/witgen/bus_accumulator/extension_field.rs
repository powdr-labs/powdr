use std::{
    collections::BTreeMap,
    iter::Sum,
    ops::{Add, Mul, Sub},
};

use num_traits::{One, Zero};

pub trait ExtensionField<T>:
    Add<Output = Self> + Sub<Output = Self> + Mul<T, Output = Self> + Zero + One + Copy + Sum
{
    fn get_challenge(challenges: &BTreeMap<u64, T>, index: u64) -> Self;
    fn size() -> usize;
    fn inverse(self) -> Self;
    fn to_vec(self) -> Vec<T>;
}
