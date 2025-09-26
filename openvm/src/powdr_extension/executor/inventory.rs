use derive_more::From;
use openvm_circuit::{
    arch::{MatrixRecordArena, VmChipComplex},
    system::{
        phantom::{PhantomChip, PhantomExecutor},
        SystemChipInventory, SystemExecutor,
    },
};
use openvm_circuit_derive::{AnyEnum, Executor, MeteredExecutor, PreflightExecutor};
use openvm_circuit_primitives::{
    bitwise_op_lookup::SharedBitwiseOperationLookupChip, range_tuple::SharedRangeTupleCheckerChip,
    var_range::SharedVariableRangeCheckerChip, Chip,
};
use openvm_stark_backend::{config::Val, p3_field::PrimeField32, prover::cpu::CpuBackend};

use crate::ExtendedVmConfigExecutor;

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

#[derive(From, Chip, AnyEnum)]
pub enum SharedPeriphery<F: PrimeField32> {
    BitwiseLookup8(SharedBitwiseOperationLookupChip<8>),
    RangeChecker(SharedRangeTupleCheckerChip<2>),
    VariableRangeChecker(SharedVariableRangeCheckerChip),
    Phantom(PhantomChip<F>),
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
