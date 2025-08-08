use derive_more::From;
use openvm_circuit::{
    arch::{SystemExecutor, SystemPeriphery, VmChipComplex, VmInventory},
    system::phantom::PhantomChip,
};
use openvm_circuit_derive::{AnyEnum, InstructionExecutor};
use openvm_circuit_primitives::{
    bitwise_op_lookup::SharedBitwiseOperationLookupChip, range_tuple::SharedRangeTupleCheckerChip,
    var_range::SharedVariableRangeCheckerChip, Chip, ChipUsageGetter,
};
use openvm_stark_backend::p3_field::PrimeField32;

use crate::{ExtendedVmConfigExecutor, ExtendedVmConfigPeriphery};

/// A dummy inventory used for execution of autoprecompiles
/// It extends the `SdkVmConfigExecutor` and `SdkVmConfigPeriphery`, providing them with shared, pre-loaded periphery chips to avoid memory allocations by each SDK chip
pub type DummyInventory<F> = VmInventory<DummyExecutor<F>, DummyPeriphery<F>>;
pub type DummyChipComplex<F> = VmChipComplex<F, DummyExecutor<F>, DummyPeriphery<F>>;

#[allow(clippy::large_enum_variant)]
#[derive(ChipUsageGetter, Chip, InstructionExecutor, AnyEnum, From)]
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

#[derive(ChipUsageGetter, Chip, AnyEnum, From)]
pub enum DummyPeriphery<F: PrimeField32> {
    #[any_enum]
    Sdk(ExtendedVmConfigPeriphery<F>),
    #[any_enum]
    Shared(SharedPeriphery<F>),
    #[any_enum]
    System(SystemPeriphery<F>),
}

#[derive(ChipUsageGetter, Chip, InstructionExecutor, From, AnyEnum)]
pub enum SharedExecutor<F: PrimeField32> {
    Phantom(PhantomChip<F>),
}

#[derive(From, ChipUsageGetter, Chip, AnyEnum)]
pub enum SharedPeriphery<F: PrimeField32> {
    BitwiseLookup8(SharedBitwiseOperationLookupChip<8>),
    RangeChecker(SharedRangeTupleCheckerChip<2>),
    VariableRangeChecker(SharedVariableRangeCheckerChip),
    Phantom(PhantomChip<F>),
}

mod from_implementations {

    use super::{DummyExecutor, DummyPeriphery};
    use openvm_sdk::config::{SdkVmConfigExecutor, SdkVmConfigPeriphery};
    use openvm_stark_backend::p3_field::PrimeField32;

    // Import all the relevant executor and periphery types
    use openvm_algebra_circuit::{
        Fp2ExtensionExecutor, Fp2ExtensionPeriphery, ModularExtensionExecutor,
        ModularExtensionPeriphery,
    };
    use openvm_bigint_circuit::{Int256Executor, Int256Periphery};
    use openvm_ecc_circuit::{WeierstrassExtensionExecutor, WeierstrassExtensionPeriphery};
    use openvm_keccak256_circuit::{Keccak256Executor, Keccak256Periphery};
    use openvm_native_circuit::{
        CastFExtensionExecutor, CastFExtensionPeriphery, NativeExecutor, NativePeriphery,
    };
    use openvm_pairing_circuit::{PairingExtensionExecutor, PairingExtensionPeriphery};
    use openvm_rv32im_circuit::{
        Rv32IExecutor, Rv32IPeriphery, Rv32IoExecutor, Rv32IoPeriphery, Rv32MExecutor,
        Rv32MPeriphery,
    };
    use openvm_sha256_circuit::{Sha256Executor, Sha256Periphery};
    use powdr_openvm_hints_circuit::HintsExecutor;
    use powdr_openvm_hints_circuit::HintsPeriphery;

    use crate::ExtendedVmConfigExecutor;
    use crate::ExtendedVmConfigPeriphery;

    /// Defines `From<T> for DummyExecutor` and `From<T> for DummyPeriphery`
    /// by mapping to the appropriate `SdkVmConfigExecutor` and `SdkVmConfigPeriphery` variant.
    /// This cannot be derived because we have a custom implementation of this conversion for the `SystemExecutor`, avoiding this wrapping.
    macro_rules! impl_zero_cost_conversions {
        ($(($variant:ident, $executor_ty:ty, $periphery_ty:ty)),* $(,)?) => {
            $(
                impl<F: PrimeField32> From<$executor_ty> for DummyExecutor<F> {
                    fn from(executor: $executor_ty) -> Self {
                        DummyExecutor::Sdk(ExtendedVmConfigExecutor::Sdk(SdkVmConfigExecutor::$variant(executor)))
                    }
                }

                impl<F: PrimeField32> From<$periphery_ty> for DummyPeriphery<F> {
                    fn from(periphery: $periphery_ty) -> Self {
                        DummyPeriphery::Sdk(ExtendedVmConfigPeriphery::Sdk(SdkVmConfigPeriphery::$variant(periphery)))
                    }
                }
            )*
        };
    }

    impl<F: PrimeField32> From<HintsExecutor<F>> for DummyExecutor<F> {
        fn from(executor: HintsExecutor<F>) -> Self {
            DummyExecutor::Sdk(ExtendedVmConfigExecutor::Hints(executor))
        }
    }

    impl<F: PrimeField32> From<HintsPeriphery<F>> for DummyPeriphery<F> {
        fn from(executor: HintsPeriphery<F>) -> Self {
            DummyPeriphery::Sdk(ExtendedVmConfigPeriphery::Hints(executor))
        }
    }

    impl_zero_cost_conversions!(
        (Rv32i, Rv32IExecutor<F>, Rv32IPeriphery<F>),
        (Io, Rv32IoExecutor<F>, Rv32IoPeriphery<F>),
        (Keccak, Keccak256Executor<F>, Keccak256Periphery<F>),
        (Sha256, Sha256Executor<F>, Sha256Periphery<F>),
        (Native, NativeExecutor<F>, NativePeriphery<F>),
        (Rv32m, Rv32MExecutor<F>, Rv32MPeriphery<F>),
        (BigInt, Int256Executor<F>, Int256Periphery<F>),
        (
            Modular,
            ModularExtensionExecutor<F>,
            ModularExtensionPeriphery<F>
        ),
        (Fp2, Fp2ExtensionExecutor<F>, Fp2ExtensionPeriphery<F>),
        (
            Pairing,
            PairingExtensionExecutor<F>,
            PairingExtensionPeriphery<F>
        ),
        (
            Ecc,
            WeierstrassExtensionExecutor<F>,
            WeierstrassExtensionPeriphery<F>
        ),
        (CastF, CastFExtensionExecutor<F>, CastFExtensionPeriphery<F>),
    );
}
