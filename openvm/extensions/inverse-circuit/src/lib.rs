use openvm_circuit::arch::{PhantomSubExecutor, Streams, VmExtension, VmInventory};
use openvm_circuit::circuit_derive::{Chip, ChipUsageGetter};
use openvm_circuit::derive::{AnyEnum, InstructionExecutor};
use openvm_circuit::system::memory::MemoryController;
use openvm_circuit::system::phantom::PhantomChip;
use openvm_stark_backend::p3_field::PrimeField32;
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use openvm_instructions::PhantomDiscriminant;
use openvm_instructions::riscv::RV32_MEMORY_AS;
use openvm_rv32im_circuit::adapters::unsafe_read_rv32_register;
use powdr_openvm_inverse_transpiler::InversePhantom;

pub struct InverseExtension;

#[derive(ChipUsageGetter, Chip, InstructionExecutor, AnyEnum)]
pub enum InverseExecutor<F: PrimeField32> {
    // TODO: not sure what to do when there's no "real" instruction in the extension
    Phantom(PhantomChip<F>),
}

#[derive(ChipUsageGetter, Chip, AnyEnum)]
pub enum InversePeriphery<F: PrimeField32> {
    Phantom(PhantomChip<F>),
}

impl VmExtension<BabyBear> for InverseExtension {
    type Executor = InverseExecutor<BabyBear>;
    type Periphery = InversePeriphery<BabyBear>;

    fn build(
        &self,
        builder: &mut openvm_circuit::arch::VmInventoryBuilder<BabyBear>,
    ) -> Result<openvm_circuit::arch::VmInventory<Self::Executor, Self::Periphery>, openvm_circuit::arch::VmInventoryError> {
        let inventory = VmInventory::new();
        builder.add_phantom_sub_executor(InverseSubEx, PhantomDiscriminant(InversePhantom::HintInverse as u16))?;
        Ok(inventory)
    }
}


pub struct InverseSubEx;

impl<F: PrimeField32> PhantomSubExecutor<F> for InverseSubEx {
    fn phantom_execute(
        &mut self,
        memory: &MemoryController<F>,
        streams: &mut Streams<F>,
        _discriminant: PhantomDiscriminant,
        a: F,
        _b: F,
        c_upper: u16,
    ) -> eyre::Result<()> {
        assert_eq!(c_upper, 0);
        // read register
        let rs1 = unsafe_read_rv32_register(memory, a);
        // read memory
        let bytes = memory.unsafe_read::<4>(
            F::from_canonical_u32(RV32_MEMORY_AS),
            F::from_canonical_u32(rs1),
        );
        println!("InverseSubEx: read bytes: {:?}", bytes);
        // write hint as bytes in reverse
        let hint_bytes = bytes
            .into_iter()
            .rev()
            .collect();
        // let hint_bytes = std::iter::once(F::from_canonical_u32(42)).collect();
        streams.hint_stream = hint_bytes;
        Ok(())
    }
}
