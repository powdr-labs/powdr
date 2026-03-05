use openvm_circuit::{
    arch::{DenseRecordArena, VmChipComplex},
    system::cuda::SystemChipInventoryGPU,
};
use openvm_cuda_backend::prover_backend::GpuBackend;

pub type GpuDummyChipComplex<SC> =
    VmChipComplex<SC, DenseRecordArena, GpuBackend, SystemChipInventoryGPU>;
