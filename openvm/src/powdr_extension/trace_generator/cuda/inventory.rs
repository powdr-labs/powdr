use derive_more::From;
use openvm_circuit::{
    arch::{
        AirInventory, ChipInventoryError, VmBuilder, VmChipComplex, VmProverExtension,
    },
    system::{phantom::PhantomExecutor, SystemExecutor},
};
use openvm_circuit_derive::{AnyEnum, Executor, MeteredExecutor, PreflightExecutor};
use openvm_circuit_primitives::Chip;
use openvm_pairing_circuit::PairingProverExt;
use openvm_sdk::config::SdkVmConfig;
use openvm_stark_backend::p3_field::PrimeField32;

use crate::{powdr_extension::trace_generator::cuda::periphery::SharedPeripheryChipsGpuProverExt, BabyBearSC, ExtendedVmConfigExecutor
};

use crate::GpuBackend;
use crate::DenseRecordArena;
use crate::GpuBabyBearPoseidon2Engine;
use crate::SystemGpuBuilder;
use openvm_circuit::system::cuda::SystemChipInventoryGPU;
use crate::powdr_extension::trace_generator::cuda::periphery::SharedPeripheryChipsGpu;
pub type GpuDummyChipComplex<SC> =
    VmChipComplex<SC, DenseRecordArena, GpuBackend, SystemChipInventoryGPU>;

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