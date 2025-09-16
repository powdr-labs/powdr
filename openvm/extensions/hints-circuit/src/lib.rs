use openvm_circuit::arch::VmExecutionExtension;
use openvm_circuit::derive::{AnyEnum, PreflightExecutor};
use openvm_circuit::system::phantom::PhantomChip;
use openvm_instructions::PhantomDiscriminant;
use openvm_stark_backend::p3_field::PrimeField32;
use powdr_openvm_hints_transpiler::HintsPhantom;

// this module is mostly copy/pasted code from k256 for the field element representation in 32-bit architectures
mod executors;
mod field10x26_k256;

/// OpenVM extension with miscellaneous hint implementations.
pub struct HintsExtension;

#[derive(AnyEnum, PreflightExecutor)]
pub enum HintsExecutor<F: PrimeField32> {
    Phantom(PhantomChip<F>),
}

impl<F: PrimeField32> VmExecutionExtension<F> for HintsExtension {
    type Executor = HintsExecutor<F>;

    fn extend_execution(
        &self,
        inventory: &mut openvm_circuit::arch::ExecutorInventoryBuilder<F, Self::Executor>,
    ) -> Result<(), openvm_circuit::arch::ExecutorInventoryError> {
        inventory.add_phantom_sub_executor(
            executors::ReverseBytesSubEx,
            PhantomDiscriminant(HintsPhantom::HintReverseBytes as u16),
        )?;
        inventory.add_phantom_sub_executor(
            executors::K256InverseFieldSubEx,
            PhantomDiscriminant(HintsPhantom::HintK256InverseField as u16),
        )?;
        inventory.add_phantom_sub_executor(
            executors::K256InverseField10x26SubEx,
            PhantomDiscriminant(HintsPhantom::HintK256InverseField10x26 as u16),
        )?;
        inventory.add_phantom_sub_executor(
            executors::K256SqrtField10x26SubEx,
            PhantomDiscriminant(HintsPhantom::HintK256SqrtField10x26 as u16),
        )?;
        Ok(())
    }
}
