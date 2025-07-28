use openvm_circuit::arch::{PhantomSubExecutor, Streams, VmExtension, VmInventory};
use openvm_circuit::circuit_derive::{Chip, ChipUsageGetter};
use openvm_circuit::derive::{AnyEnum, InstructionExecutor};
use openvm_circuit::system::memory::MemoryController;
use openvm_circuit::system::phantom::PhantomChip;
use openvm_instructions::riscv::RV32_MEMORY_AS;
use openvm_instructions::PhantomDiscriminant;
use openvm_rv32im_circuit::adapters::unsafe_read_rv32_register;
use openvm_stark_backend::p3_field::PrimeField32;
use powdr_openvm_hints_transpiler::HintsPhantom;

// this module is mostly copy/pasted code from k256 for the field element representation in 32-bit architectures
mod field_k256;

pub struct HintsExtension;

#[derive(ChipUsageGetter, Chip, InstructionExecutor, AnyEnum)]
pub enum HintsExecutor<F: PrimeField32> {
    // TODO: not sure what to do when there's no "real" instruction in the extension
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
            ReverseBytesSubEx,
            PhantomDiscriminant(HintsPhantom::HintReverseBytes as u16),
        )?;
        builder.add_phantom_sub_executor(
            K256InverseFieldSubEx,
            PhantomDiscriminant(HintsPhantom::HintK256InverseField as u16),
        )?;
        builder.add_phantom_sub_executor(
            K256InverseField10x26SubEx,
            PhantomDiscriminant(HintsPhantom::HintK256InverseField10x26 as u16),
        )?;
        builder.add_phantom_sub_executor(
            K256SqrtField10x26SubEx,
            PhantomDiscriminant(HintsPhantom::HintK256SqrtField10x26 as u16),
        )?;
        Ok(inventory)
    }
}

pub struct ReverseBytesSubEx;

impl<F: PrimeField32> PhantomSubExecutor<F> for ReverseBytesSubEx {
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
        // write hint as bytes in reverse
        let hint_bytes = bytes.into_iter().rev().collect();
        // let hint_bytes = std::iter::once(F::from_canonical_u32(42)).collect();
        streams.hint_stream = hint_bytes;
        Ok(())
    }
}

pub struct K256InverseFieldSubEx;

use crypto_bigint::const_monty_form;
use crypto_bigint::impl_modulus;
use crypto_bigint::modular::ConstMontyParams;
use crypto_bigint::Encoding;
use crypto_bigint::Zero;
use crypto_bigint::U256;
impl_modulus!(
    K256Mod,
    U256,
    "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F"
);

impl<F: PrimeField32> PhantomSubExecutor<F> for K256InverseFieldSubEx {
    fn phantom_execute(
        &mut self,
        memory: &MemoryController<F>,
        streams: &mut Streams<F>,
        _: PhantomDiscriminant,
        a: F,
        _b: F,
        c_upper: u16,
    ) -> eyre::Result<()> {
        assert_eq!(c_upper, 0);
        // read register
        let rs1 = unsafe_read_rv32_register(memory, a);
        // read the field element
        let bytes: [u8; 32] = memory
            .unsafe_read::<32>(
                F::from_canonical_u32(RV32_MEMORY_AS),
                F::from_canonical_u32(rs1),
            )
            .into_iter()
            .map(|f| u8::try_from(f.as_canonical_u32()).expect("value not a byte"))
            .collect::<Vec<_>>()
            .try_into()
            .unwrap();

        let n = U256::from_be_bytes(bytes);

        // perform the inverse.
        let n_mod = const_monty_form!(n, K256Mod);
        // TODO: do we care about constant time here?
        let n_inv = if !(bool::from(n_mod.is_zero())) {
            n_mod.inv().unwrap().retrieve()
        } else {
            U256::ZERO
        };
        let inv_bytes = n_inv
            .to_be_bytes()
            .into_iter()
            .map(|b| F::from_canonical_u8(b))
            .collect();
        streams.hint_stream = inv_bytes;

        Ok(())
    }
}

pub struct K256InverseField10x26SubEx;

impl<F: PrimeField32> PhantomSubExecutor<F> for K256InverseField10x26SubEx {
    fn phantom_execute(
        &mut self,
        memory: &MemoryController<F>,
        streams: &mut Streams<F>,
        _: PhantomDiscriminant,
        a: F,
        _b: F,
        c_upper: u16,
    ) -> eyre::Result<()> {
        assert_eq!(c_upper, 0);
        // read register
        let rs1 = unsafe_read_rv32_register(memory, a);
        // read the k256 field_10x26 as raw bytes
        let bytes: [u8; 40] = memory
            .unsafe_read::<40>(
                F::from_canonical_u32(RV32_MEMORY_AS),
                F::from_canonical_u32(rs1),
            )
            .into_iter()
            .map(|f| u8::try_from(f.as_canonical_u32()).expect("value not a byte"))
            .collect::<Vec<_>>()
            .try_into()
            .unwrap();
        // we just reinterpret the bytes as a k256 field element. We don't use mem::transmute to avoid alignment issues
        let mut elem = [0u32; 10];
        unsafe {
            std::ptr::copy_nonoverlapping(bytes.as_ptr(), elem.as_mut_ptr() as *mut u8, 40);
        }
        let elem = field_k256::FieldElement10x26(elem);
        let inv = elem.invert();
        // okay to transmute in the opposite direction
        let inv_bytes: [u8; 40] = unsafe { std::mem::transmute(inv.0) };
        streams.hint_stream = inv_bytes
            .into_iter()
            .map(|b| F::from_canonical_u8(b))
            .collect();

        Ok(())
    }
}

pub struct K256SqrtField10x26SubEx;

impl<F: PrimeField32> PhantomSubExecutor<F> for K256SqrtField10x26SubEx {
    fn phantom_execute(
        &mut self,
        memory: &MemoryController<F>,
        streams: &mut Streams<F>,
        _: PhantomDiscriminant,
        a: F,
        _b: F,
        c_upper: u16,
    ) -> eyre::Result<()> {
        assert_eq!(c_upper, 0);
        // read register
        let rs1 = unsafe_read_rv32_register(memory, a);
        // read the k256 field_10x26 as raw bytes
        let bytes: [u8; 40] = memory
            .unsafe_read::<40>(
                F::from_canonical_u32(RV32_MEMORY_AS),
                F::from_canonical_u32(rs1),
            )
            .into_iter()
            .map(|f| u8::try_from(f.as_canonical_u32()).expect("value not a byte"))
            .collect::<Vec<_>>()
            .try_into()
            .unwrap();
        // we just reinterpret the bytes as a k256 field element. Can't use mem::transmute due to alighment requirements
        let mut elem = [0u32; 10];
        unsafe {
            std::ptr::copy_nonoverlapping(bytes.as_ptr(), elem.as_mut_ptr() as *mut u8, 40);
        }
        let elem = field_k256::FieldElement10x26(elem);
        let res = elem.sqrt();
        if res.is_some().into() {
            // return 1 followed by the result
            let bytes: [u8; 40] = unsafe {
                // safe to transmute into u8 array
                std::mem::transmute(res.unwrap().0)
            };
            streams.hint_stream = 1u32
                .to_le_bytes() // indicates that a square root exists
                .into_iter()
                .chain(bytes.into_iter())
                .map(|b| F::from_canonical_u8(b))
                .collect();
        } else {
            // no square root, return a 0
            streams.hint_stream = 0u32
                .to_le_bytes()
                .map(|b| F::from_canonical_u8(b))
                .into_iter()
                .collect();
        }

        Ok(())
    }
}
