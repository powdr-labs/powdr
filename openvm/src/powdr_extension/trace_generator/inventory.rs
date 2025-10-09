use derive_more::From;
use openvm_algebra_circuit::AlgebraCpuProverExt;
use openvm_bigint_circuit::Int256CpuProverExt;
use openvm_circuit::{
    arch::{
        AirInventory, AirInventoryError, ChipInventoryError, MatrixRecordArena, VmBuilder,
        VmChipComplex, VmCircuitConfig, VmCircuitExtension, VmProverExtension,
    },
    system::{phantom::PhantomExecutor, SystemChipInventory, SystemCpuBuilder, SystemExecutor},
};
use openvm_circuit_derive::{AnyEnum, Executor, MeteredExecutor, PreflightExecutor};
use openvm_circuit_primitives::Chip;
use openvm_ecc_circuit::EccCpuProverExt;
use openvm_keccak256_circuit::Keccak256CpuProverExt;
use openvm_native_circuit::NativeCpuProverExt;
use openvm_pairing_circuit::PairingProverExt;
use openvm_rv32im_circuit::Rv32ImCpuProverExt;
use openvm_sdk::config::SdkVmConfig;
use openvm_sha256_circuit::Sha2CpuProverExt;
use openvm_stark_backend::{config::Val, p3_field::PrimeField32, prover::cpu::CpuBackend};

use crate::{
    powdr_extension::trace_generator::periphery::{
        SharedPeripheryChips, SharedPeripheryChipsCpuProverExt,
    },
    BabyBearSC, ExtendedVmConfigExecutor,
};

use openvm_stark_sdk::config::baby_bear_poseidon2::BabyBearPoseidon2Engine;

/// A dummy inventory used for execution of autoprecompiles
/// It extends the `SdkVmConfigExecutor` and `SdkVmConfigPeriphery`, providing them with shared, pre-loaded periphery chips to avoid memory allocations by each SDK chip
pub type DummyChipComplex<SC> =
    VmChipComplex<SC, MatrixRecordArena<Val<SC>>, CpuBackend<SC>, SystemChipInventory<SC>>;

#[allow(clippy::large_enum_variant)]
#[derive(Chip, PreflightExecutor, Executor, MeteredExecutor, From, AnyEnum)]
pub enum DummyExecutor<F: PrimeField32> {
    #[any_enum]
    Sdk(ExtendedVmConfigExecutor<F>),
    #[any_enum]
    Shared(SharedExecutor<F>),
    #[any_enum]
    /// We keep the `SystemExecutor` variant to allow for system-level operations. Failing to do this compiles, but at runtime since the `PhantomChip` cannot be found
    /// This seems like a bug in openvm, as it breaks abstraction: wrapping an executor should not require the user to know about the system executor.
    System(SystemExecutor<F>),
}

#[derive(Chip, PreflightExecutor, Executor, MeteredExecutor, From, AnyEnum)]
pub enum SharedExecutor<F: PrimeField32> {
    Phantom(PhantomExecutor<F>),
}

mod from_implementations {

    use super::DummyExecutor;
    use openvm_sdk::config::SdkVmConfigExecutor;
    use openvm_stark_backend::p3_field::PrimeField32;

    // Import all the relevant executor types
    use openvm_algebra_circuit::{Fp2ExtensionExecutor, ModularExtensionExecutor};

    use openvm_bigint_circuit::Int256Executor;
    use openvm_ecc_circuit::WeierstrassExtensionExecutor;
    use openvm_keccak256_circuit::Keccak256Executor;
    use openvm_native_circuit::{CastFExtensionExecutor, NativeExecutor};
    use openvm_pairing_circuit::PairingExtensionExecutor;
    use openvm_rv32im_circuit::{Rv32IExecutor, Rv32IoExecutor, Rv32MExecutor};
    use openvm_sha256_circuit::Sha256Executor;
    use powdr_openvm_hints_circuit::HintsExtensionExecutor;

    use crate::ExtendedVmConfigExecutor;

    /// Defines `From<T> for DummyExecutor` and `From<T> for DummyPeriphery`
    /// by mapping to the appropriate `SdkVmConfigExecutor` and `SdkVmConfigPeriphery` variant.
    /// This cannot be derived because we have a custom implementation of this conversion for the `SystemExecutor`, avoiding this wrapping.
    macro_rules! impl_zero_cost_conversions {
        ($(($variant:ident, $executor_ty:ty)),* $(,)?) => {
            $(
                impl<F: PrimeField32> From<$executor_ty> for DummyExecutor<F> {
                    fn from(executor: $executor_ty) -> Self {
                        DummyExecutor::Sdk(ExtendedVmConfigExecutor::Sdk(SdkVmConfigExecutor::$variant(executor)))
                    }
                }
            )*
        };
    }

    impl<F: PrimeField32> From<HintsExtensionExecutor<F>> for DummyExecutor<F> {
        fn from(executor: HintsExtensionExecutor<F>) -> Self {
            DummyExecutor::Sdk(ExtendedVmConfigExecutor::Hints(executor))
        }
    }

    impl_zero_cost_conversions!(
        (Rv32i, Rv32IExecutor),
        (Io, Rv32IoExecutor),
        (Keccak, Keccak256Executor),
        (Sha256, Sha256Executor),
        (Native, NativeExecutor<F>),
        (Rv32m, Rv32MExecutor),
        (Bigint, Int256Executor),
        (Modular, ModularExtensionExecutor),
        (Fp2, Fp2ExtensionExecutor),
        (Pairing, PairingExtensionExecutor<F>),
        (Ecc, WeierstrassExtensionExecutor),
        (Castf, CastFExtensionExecutor),
    );
}

pub fn create_dummy_airs(
    config: &SdkVmConfig,
    shared_chips: SharedPeripheryChips,
) -> Result<AirInventory<BabyBearSC>, AirInventoryError> {
    let config = config.to_inner();
    let mut inventory = config.system.create_airs()?;

    // CHANGE: add dummy periphery
    inventory.start_new_extension();
    VmCircuitExtension::extend_circuit(&shared_chips, &mut inventory)?;
    // END CHANGE

    if let Some(rv32i) = &config.rv32i {
        VmCircuitExtension::extend_circuit(rv32i, &mut inventory)?;
    }
    if let Some(io) = &config.io {
        VmCircuitExtension::extend_circuit(io, &mut inventory)?;
    }
    if let Some(keccak) = &config.keccak {
        VmCircuitExtension::extend_circuit(keccak, &mut inventory)?;
    }
    if let Some(sha256) = &config.sha256 {
        VmCircuitExtension::extend_circuit(sha256, &mut inventory)?;
    }
    if let Some(native) = &config.native {
        VmCircuitExtension::extend_circuit(native, &mut inventory)?;
    }
    if let Some(castf) = &config.castf {
        VmCircuitExtension::extend_circuit(castf, &mut inventory)?;
    }
    if let Some(rv32m) = &config.rv32m {
        VmCircuitExtension::extend_circuit(rv32m, &mut inventory)?;
    }
    if let Some(bigint) = &config.bigint {
        VmCircuitExtension::extend_circuit(bigint, &mut inventory)?;
    }
    if let Some(modular) = &config.modular {
        VmCircuitExtension::extend_circuit(modular, &mut inventory)?;
    }
    if let Some(fp2) = &config.fp2 {
        VmCircuitExtension::extend_circuit(fp2, &mut inventory)?;
    }
    if let Some(pairing) = &config.pairing {
        VmCircuitExtension::extend_circuit(pairing, &mut inventory)?;
    }
    if let Some(ecc) = &config.ecc {
        VmCircuitExtension::extend_circuit(ecc, &mut inventory)?;
    }
    Ok(inventory)
}

pub fn create_dummy_chip_complex(
    config: &SdkVmConfig,
    circuit: AirInventory<BabyBearSC>,
    shared_chips: SharedPeripheryChips,
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
