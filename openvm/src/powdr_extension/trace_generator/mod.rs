pub mod cpu;
#[cfg(feature = "cuda")]
pub mod cuda;
pub mod jit_mapping;

mod common;

pub use cpu::{DummyChipComplex, SharedPeripheryChipsCpu};

#[cfg(feature = "cuda")]
pub use cuda::{GpuDummyChipComplex, SharedPeripheryChipsGpu};
