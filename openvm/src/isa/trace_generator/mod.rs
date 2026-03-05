mod cpu;
#[cfg(feature = "cuda")]
mod cuda;

mod common;
pub use common::create_dummy_airs;
pub use cpu::create_dummy_chip_complex_cpu;
#[cfg(feature = "cuda")]
pub use cuda::create_dummy_chip_complex_gpu;
