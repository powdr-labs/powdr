#[cfg(feature = "cuda")]
use openvm_cuda_builder::{cuda_available, CudaBuilder};
#[cfg(feature = "cuda")]
use std::{env, path::Path};

#[cfg(feature = "cuda")]
fn is_valid_cuda_arch(value: &str) -> bool {
    !value.is_empty() && value.split(',').all(|v| v.parse::<u32>().is_ok())
}

fn main() {
    #[cfg(feature = "cuda")]
    {
        if !cuda_available() {
            return; // Skip CUDA compilation
        }

        // The upstream autodetection can return an NVML error string in some
        // environments; default to a stable arch so `cargo check --features cuda` works.
        let cuda_arch = env::var("CUDA_ARCH")
            .ok()
            .filter(|v| is_valid_cuda_arch(v))
            .unwrap_or_else(|| "80".to_string());
        env::set_var("CUDA_ARCH", cuda_arch);

        let mut builder: CudaBuilder = CudaBuilder::new()
            .include_from_dep("DEP_CIRCUIT_PRIMITIVES_CUDA_INCLUDE") // Point to header file folder of crate with path `DEP_CIRCUIT_PRIMITIVES_CUDA_INCLUDE`
            .include_from_dep("DEP_CUDA_COMMON_INCLUDE") // The only dependency of our dependency `DEP_CIRCUIT_PRIMITIVES_CUDA_INCLUDE`
            .watch("cuda") // Watch file changes of this crate for recompilation
            .library_name("powdr_gpu") // Library name of this crate; doesn't affect import name
            .files_from_glob("cuda/src/**/*.cu"); // Import all `.cu` files with zero or more nested sub-folders under `cuda/src`of this crate

        if Path::new("cuda/include").exists() {
            builder = builder.include("cuda/include");
        }

        builder.emit_link_directives();
        builder.build();
    }
}
