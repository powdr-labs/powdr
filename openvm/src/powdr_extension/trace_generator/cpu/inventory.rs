use openvm_circuit::{
    arch::{MatrixRecordArena, VmChipComplex},
    system::SystemChipInventory,
};
use openvm_stark_backend::{config::Val, prover::cpu::CpuBackend};

/// A dummy inventory used for execution of autoprecompiles
/// It extends the `SdkVmConfigExecutor` and `SdkVmConfigPeriphery`, providing them with shared, pre-loaded periphery chips to avoid memory allocations by each SDK chip
pub type DummyChipComplex<SC> =
    VmChipComplex<SC, MatrixRecordArena<Val<SC>>, CpuBackend<SC>, SystemChipInventory<SC>>;
