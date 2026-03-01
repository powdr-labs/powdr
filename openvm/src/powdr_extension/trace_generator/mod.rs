pub mod cpu;
#[cfg(feature = "cuda")]
pub mod cuda;

mod common;
pub use common::create_dummy_airs;
