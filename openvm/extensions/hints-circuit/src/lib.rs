use openvm_circuit::arch::{
    AirInventory, AirInventoryError, ExecutorInventoryBuilder, ExecutorInventoryError, VmCircuitExtension, VmExecutionExtension
};
use openvm_circuit::derive::{AnyEnum, Executor, MeteredExecutor, PreflightExecutor};
use openvm_circuit::system::phantom::PhantomExecutor;
use openvm_instructions::PhantomDiscriminant;
use openvm_stark_backend::config::StarkGenericConfig;
use openvm_stark_backend::p3_field::{Field, PrimeField32};
use powdr_openvm_hints_transpiler::HintsPhantom;
use serde::{Deserialize, Serialize};

// this module is mostly copy/pasted code from k256 for the field element representation in 32-bit architectures
mod executors;
mod field10x26_k256;

/// OpenVM extension with miscellaneous hint implementations.
#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct HintsExtension;

#[derive(AnyEnum, PreflightExecutor, Executor, MeteredExecutor, Clone)]
pub enum HintsExtensionExecutor<F: Field> {
    Phantom(PhantomExecutor<F>),
}

impl<F: PrimeField32> VmExecutionExtension<F> for HintsExtension {
    type Executor = HintsExtensionExecutor<F>;

    fn extend_execution(
        &self,
        inventory: &mut ExecutorInventoryBuilder<F, Self::Executor>,
    ) -> Result<(), ExecutorInventoryError> {
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

impl<SC: StarkGenericConfig> VmCircuitExtension<SC> for HintsExtension {
    fn extend_circuit(&self, _: &mut AirInventory<SC>) -> Result<(), AirInventoryError> {
        Ok(())
    }
}
