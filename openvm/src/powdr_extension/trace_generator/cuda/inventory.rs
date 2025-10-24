use openvm_circuit::arch::{
    AirInventory, ChipInventoryError, VmBuilder, VmChipComplex, VmProverExtension,
};
use openvm_pairing_circuit::PairingProverExt;
use openvm_sdk::config::SdkVmConfig;

use crate::{
    powdr_extension::trace_generator::cuda::periphery::SharedPeripheryChipsGpuProverExt, BabyBearSC,
};

use crate::powdr_extension::trace_generator::cuda::periphery::SharedPeripheryChipsGpu;
use crate::DenseRecordArena;
use crate::GpuBabyBearPoseidon2Engine;
use crate::GpuBackend;
use crate::SystemGpuBuilder;
use openvm_circuit::system::cuda::SystemChipInventoryGPU;
pub type GpuDummyChipComplex<SC> =
    VmChipComplex<SC, DenseRecordArena, GpuBackend, SystemChipInventoryGPU>;

pub fn create_dummy_chip_complex(
    config: &SdkVmConfig,
    circuit: AirInventory<BabyBearSC>,
    shared_chips: SharedPeripheryChipsGpu,
) -> Result<GpuDummyChipComplex<BabyBearSC>, ChipInventoryError> {
    use openvm_algebra_circuit::AlgebraProverExt;
    use openvm_bigint_circuit::Int256GpuProverExt;
    use openvm_ecc_circuit::EccProverExt;
    use openvm_keccak256_circuit::Keccak256GpuProverExt;
    use openvm_native_circuit::NativeGpuProverExt;
    use openvm_rv32im_circuit::Rv32ImGpuProverExt;
    use openvm_sha256_circuit::Sha256GpuProverExt;

    type E = GpuBabyBearPoseidon2Engine;

    let config = config.to_inner();
    let mut chip_complex =
        VmBuilder::<E>::create_chip_complex(&SystemGpuBuilder, &config.system, circuit)?;
    let inventory = &mut chip_complex.inventory;

    // CHANGE: inject the periphery chips so that they are not created by the extensions. This is done for memory footprint: the dummy periphery chips are thrown away anyway, so we reuse a single one for all APCs.
    VmProverExtension::<E, _, _>::extend_prover(
        &SharedPeripheryChipsGpuProverExt,
        &shared_chips,
        inventory,
    )?;
    // END CHANGE

    if let Some(rv32i) = &config.rv32i {
        VmProverExtension::<E, _, _>::extend_prover(&Rv32ImGpuProverExt, rv32i, inventory)?;
    }
    if let Some(io) = &config.io {
        VmProverExtension::<E, _, _>::extend_prover(&Rv32ImGpuProverExt, io, inventory)?;
    }
    if let Some(keccak) = &config.keccak {
        VmProverExtension::<E, _, _>::extend_prover(&Keccak256GpuProverExt, keccak, inventory)?;
    }
    if let Some(sha256) = &config.sha256 {
        VmProverExtension::<E, _, _>::extend_prover(&Sha256GpuProverExt, sha256, inventory)?;
    }
    if let Some(native) = &config.native {
        VmProverExtension::<E, _, _>::extend_prover(&NativeGpuProverExt, native, inventory)?;
    }
    if let Some(castf) = &config.castf {
        VmProverExtension::<E, _, _>::extend_prover(&NativeGpuProverExt, castf, inventory)?;
    }
    if let Some(rv32m) = &config.rv32m {
        VmProverExtension::<E, _, _>::extend_prover(&Rv32ImGpuProverExt, rv32m, inventory)?;
    }
    if let Some(bigint) = &config.bigint {
        VmProverExtension::<E, _, _>::extend_prover(&Int256GpuProverExt, bigint, inventory)?;
    }
    if let Some(modular) = &config.modular {
        VmProverExtension::<E, _, _>::extend_prover(&AlgebraProverExt, modular, inventory)?;
    }
    if let Some(fp2) = &config.fp2 {
        VmProverExtension::<E, _, _>::extend_prover(&AlgebraProverExt, fp2, inventory)?;
    }
    if let Some(pairing) = &config.pairing {
        VmProverExtension::<E, _, _>::extend_prover(&PairingProverExt, pairing, inventory)?;
    }
    if let Some(ecc) = &config.ecc {
        VmProverExtension::<E, _, _>::extend_prover(&EccProverExt, ecc, inventory)?;
    }
    Ok(chip_complex)
}
