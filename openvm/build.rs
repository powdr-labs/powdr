#[cfg(feature = "cuda")]
use openvm_cuda_builder::{cuda_available, CudaBuilder};

fn main() {
    #[cfg(feature = "cuda")]
    {
        if !cuda_available() {
            return; // Skip CUDA compilation
        }

        let builder: CudaBuilder = CudaBuilder::new()
            .include_from_dep("DEP_CIRCUIT_PRIMITIVES_CUDA_INCLUDE") // Point to header file folder of crate with path `DEP_CIRCUIT_PRIMITIVES_CUDA_INCLUDE`
            .include_from_dep("DEP_CUDA_COMMON_INCLUDE") // The only dependency of our dependency `DEP_CIRCUIT_PRIMITIVES_CUDA_INCLUDE`
            .include("cuda/include") // Point to header file folder of this crate
            .watch("cuda") // Watch file changes of this crate for recompilation
            .library_name("powdr_gpu") // Library name of this crate; doesn't affect import name
            .files_from_glob("cuda/src/**/*.cu"); // Import all `.cu` files with zero or more nested sub-folders under `cuda/src`of this crate

        builder.emit_link_directives();
        builder.build();
    }
}
