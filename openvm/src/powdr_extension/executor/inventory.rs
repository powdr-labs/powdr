use derive_more::From;
use openvm_circuit::{
    arch::{ChipInventory, ExecutorInventory, MatrixRecordArena, PreflightExecutor},
    system::{phantom::PhantomChip, SystemExecutor},
};
use openvm_circuit_derive::{AnyEnum, PreflightExecutor};
use openvm_circuit_primitives::{
    bitwise_op_lookup::SharedBitwiseOperationLookupChip, range_tuple::SharedRangeTupleCheckerChip,
    var_range::SharedVariableRangeCheckerChip, Chip,
};
use openvm_stark_backend::{config::Val, p3_field::PrimeField32, prover::cpu::CpuBackend};

use crate::ExtendedVmConfigExecutor;

/// A dummy inventory used for execution of autoprecompiles
/// It extends the `SdkVmConfigExecutor` and `SdkVmConfigPeriphery`, providing them with shared, pre-loaded periphery chips to avoid memory allocations by each SDK chip
pub type DummyChipInventory<SC> = ChipInventory<SC, MatrixRecordArena<Val<SC>>, CpuBackend<SC>>;
pub type DummyExecutorInventory<F> = ExecutorInventory<DummyExecutor<F>>;
// pub type DummyChipComplex<SC, RA, PB> = VmChipComplex<SC, RA, PB, F, DummyExecutor<F>>;

#[allow(clippy::large_enum_variant)]
#[derive(Chip, AnyEnum, From)]
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

impl<F: PrimeField32, RA> PreflightExecutor<F, RA> for DummyExecutor<F> {
    fn execute(
        &self,
        state: openvm_circuit::arch::VmStateMut<
            F,
            openvm_circuit::system::memory::online::TracingMemory,
            RA,
        >,
        instruction: &openvm_instructions::instruction::Instruction<F>,
    ) -> Result<(), openvm_circuit::arch::ExecutionError> {
        todo!()
    }

    fn get_opcode_name(&self, opcode: usize) -> String {
        todo!()
    }
}

#[derive(Chip, PreflightExecutor, From, AnyEnum)]
pub enum SharedExecutor<F: PrimeField32> {
    Phantom(PhantomChip<F>),
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

    // Import all the relevant executor and periphery types
    use openvm_algebra_circuit::{Fp2ExtensionExecutor, ModularExtensionExecutor};
    
    use openvm_ecc_circuit::WeierstrassExtensionExecutor;
    use openvm_keccak256_circuit::Keccak256Executor;
    use openvm_native_circuit::NativeExecutor;
    use openvm_pairing_circuit::PairingExtensionExecutor;
    use openvm_rv32im_circuit::{Rv32IExecutor, Rv32IoExecutor, Rv32MExecutor};
    use openvm_sha256_circuit::Sha256Executor;
    use powdr_openvm_hints_circuit::HintsExecutor;

    use crate::ExtendedVmConfigExecutor;

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
            )*
        };
    }

    impl<F: PrimeField32> From<HintsExecutor<F>> for DummyExecutor<F> {
        fn from(executor: HintsExecutor<F>) -> Self {
            DummyExecutor::Sdk(ExtendedVmConfigExecutor::Hints(executor))
        }
    }

    impl_zero_cost_conversions!(
        (Rv32i, Rv32IExecutor, Rv32IPeriphery<F>),
        (Io, Rv32IoExecutor, Rv32IoPeriphery<F>),
        (Keccak, Keccak256Executor, Keccak256Periphery<F>),
        (Sha256, Sha256Executor, Sha256Periphery<F>),
        (Native, NativeExecutor<F>, NativePeriphery<F>),
        (Rv32m, Rv32MExecutor, Rv32MPeriphery<F>),
        // (BigInt, Int256Executor, Int256Periphery<F>),
        (
            Modular,
            ModularExtensionExecutor,
            ModularExtensionPeriphery<F>
        ),
        (Fp2, Fp2ExtensionExecutor, Fp2ExtensionPeriphery<F>),
        (
            Pairing,
            PairingExtensionExecutor<F>,
            PairingExtensionPeriphery<F>
        ),
        (
            Ecc,
            WeierstrassExtensionExecutor,
            WeierstrassExtensionPeriphery<F>
        ),
        // (CastF, CastFExtensionExecutor, CastFExtensionPeriphery<F>),
    );
}
