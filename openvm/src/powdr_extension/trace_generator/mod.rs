pub mod cpu;
#[cfg(feature = "cuda")]
pub mod cuda;

mod common;

pub use cpu::{DummyChipComplex, SharedPeripheryChipsCpu};

#[cfg(feature = "cuda")]
pub use cuda::{};
