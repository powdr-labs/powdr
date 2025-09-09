use openvm_circuit::arch::{VmExtension, VmInventory};
use openvm_circuit::circuit_derive::{Chip, ChipUsageGetter};
use openvm_circuit::derive::{AnyEnum, InstructionExecutor};
use openvm_circuit::system::phantom::PhantomChip;
use openvm_instructions::PhantomDiscriminant;
use openvm_stark_backend::p3_field::PrimeField32;
use powdr_openvm_hints_transpiler::HintsPhantom;

// this module is mostly copy/pasted code from k256 for the field element representation in 32-bit architectures
mod executors;
mod field10x26_k256;

/// OpenVM extension with miscellaneous hint implementations.
pub struct HintsExtension;

#[derive(ChipUsageGetter, Chip, InstructionExecutor, AnyEnum)]
pub enum HintsExecutor<F: PrimeField32> {
    Phantom(PhantomChip<F>),
}

#[derive(ChipUsageGetter, Chip, AnyEnum)]
pub enum HintsPeriphery<F: PrimeField32> {
    Phantom(PhantomChip<F>),
}

impl<F: PrimeField32> VmExtension<F> for HintsExtension {
    type Executor = HintsExecutor<F>;
    type Periphery = HintsPeriphery<F>;

    fn build(
        &self,
        builder: &mut openvm_circuit::arch::VmInventoryBuilder<F>,
    ) -> Result<
        openvm_circuit::arch::VmInventory<Self::Executor, Self::Periphery>,
        openvm_circuit::arch::VmInventoryError,
    > {
        let inventory = VmInventory::new();
        builder.add_phantom_sub_executor(
            executors::ReverseBytesSubEx,
            PhantomDiscriminant(HintsPhantom::HintReverseBytes as u16),
        )?;
        builder.add_phantom_sub_executor(
            executors::K256InverseFieldSubEx,
            PhantomDiscriminant(HintsPhantom::HintK256InverseField as u16),
        )?;
        builder.add_phantom_sub_executor(
            executors::K256InverseField10x26SubEx,
            PhantomDiscriminant(HintsPhantom::HintK256InverseField10x26 as u16),
        )?;
        builder.add_phantom_sub_executor(
            executors::K256SqrtField10x26SubEx,
            PhantomDiscriminant(HintsPhantom::HintK256SqrtField10x26 as u16),
        )?;
        Ok(inventory)
    }
}
