use openvm_algebra_circuit::AlgebraCpuProverExt;
use openvm_bigint_circuit::Int256CpuProverExt;
use openvm_circuit::arch::{
    AirInventory, ChipInventoryError, VmBuilder, VmChipComplex, VmProverExtension,
};
use openvm_circuit::system::SystemCpuBuilder;
use openvm_keccak256_circuit::Keccak256CpuProverExt;
use openvm_native_circuit::NativeCpuProverExt;
use openvm_pairing_circuit::PairingProverExt;
use openvm_rv32im_circuit::Rv32ImCpuProverExt;
use openvm_sdk::config::SdkVmConfig;
use openvm_sha256_circuit::Sha2CpuProverExt;

use super::periphery::SharedPeripheryChipsCpu;
use crate::BabyBearSC;

use super::periphery::SharedPeripheryChipsCpuProverExt;
use openvm_circuit::arch::MatrixRecordArena;
use openvm_circuit::system::SystemChipInventory;
use openvm_stark_backend::config::Val;
use openvm_stark_backend::prover::cpu::CpuBackend;
/// A dummy inventory used for execution of autoprecompiles
/// It extends the `SdkVmConfigExecutor` and `SdkVmConfigPeriphery`, providing them with shared, pre-loaded periphery chips to avoid memory allocations by each SDK chip
pub type DummyChipComplex<SC> =
    VmChipComplex<SC, MatrixRecordArena<Val<SC>>, CpuBackend<SC>, SystemChipInventory<SC>>;
use openvm_ecc_circuit::EccCpuProverExt;
use openvm_stark_sdk::config::baby_bear_poseidon2::BabyBearPoseidon2Engine;
pub fn create_dummy_chip_complex(
    config: &SdkVmConfig,
    circuit: AirInventory<BabyBearSC>,
    shared_chips: SharedPeripheryChipsCpu,
) -> Result<DummyChipComplex<BabyBearSC>, ChipInventoryError> {
    let config = config.to_inner();
    let mut chip_complex = VmBuilder::<BabyBearPoseidon2Engine>::create_chip_complex(
        &SystemCpuBuilder,
        &config.system,
        circuit,
    )?;
    let inventory = &mut chip_complex.inventory;

    // CHANGE: inject the periphery chips so that they are not created by the extensions. This is done for memory footprint: the dummy periphery chips are thrown away anyway, so we reuse a single one for all APCs.
    VmProverExtension::<BabyBearPoseidon2Engine, _, _>::extend_prover(
        &SharedPeripheryChipsCpuProverExt,
        &shared_chips,
        inventory,
    )?;
    // END CHANGE

    if let Some(rv32i) = &config.rv32i {
        VmProverExtension::<BabyBearPoseidon2Engine, _, _>::extend_prover(
            &Rv32ImCpuProverExt,
            rv32i,
            inventory,
        )?;
    }
    if let Some(io) = &config.io {
        VmProverExtension::<BabyBearPoseidon2Engine, _, _>::extend_prover(
            &Rv32ImCpuProverExt,
            io,
            inventory,
        )?;
    }
    if let Some(keccak) = &config.keccak {
        VmProverExtension::<BabyBearPoseidon2Engine, _, _>::extend_prover(
            &Keccak256CpuProverExt,
            keccak,
            inventory,
        )?;
    }
    if let Some(sha256) = &config.sha256 {
        VmProverExtension::<BabyBearPoseidon2Engine, _, _>::extend_prover(
            &Sha2CpuProverExt,
            sha256,
            inventory,
        )?;
    }
    if let Some(native) = &config.native {
        VmProverExtension::<BabyBearPoseidon2Engine, _, _>::extend_prover(
            &NativeCpuProverExt,
            native,
            inventory,
        )?;
    }
    if let Some(castf) = &config.castf {
        VmProverExtension::<BabyBearPoseidon2Engine, _, _>::extend_prover(
            &NativeCpuProverExt,
            castf,
            inventory,
        )?;
    }
    if let Some(rv32m) = &config.rv32m {
        VmProverExtension::<BabyBearPoseidon2Engine, _, _>::extend_prover(
            &Rv32ImCpuProverExt,
            rv32m,
            inventory,
        )?;
    }
    if let Some(bigint) = &config.bigint {
        VmProverExtension::<BabyBearPoseidon2Engine, _, _>::extend_prover(
            &Int256CpuProverExt,
            bigint,
            inventory,
        )?;
    }
    if let Some(modular) = &config.modular {
        VmProverExtension::<BabyBearPoseidon2Engine, _, _>::extend_prover(
            &AlgebraCpuProverExt,
            modular,
            inventory,
        )?;
    }
    if let Some(fp2) = &config.fp2 {
        VmProverExtension::<BabyBearPoseidon2Engine, _, _>::extend_prover(
            &AlgebraCpuProverExt,
            fp2,
            inventory,
        )?;
    }
    if let Some(pairing) = &config.pairing {
        VmProverExtension::<BabyBearPoseidon2Engine, _, _>::extend_prover(
            &PairingProverExt,
            pairing,
            inventory,
        )?;
    }
    if let Some(ecc) = &config.ecc {
        VmProverExtension::<BabyBearPoseidon2Engine, _, _>::extend_prover(
            &EccCpuProverExt,
            ecc,
            inventory,
        )?;
    }

    Ok(chip_complex)
}
