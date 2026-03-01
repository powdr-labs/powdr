use std::{fmt::Display, iter::once, marker::PhantomData};

use openvm_instructions::instruction::Instruction as OpenVmInstruction;
use openvm_stark_backend::p3_field::PrimeField32;
use powdr_autoprecompiles::blocks::{Instruction, PcStep};
use serde::{Deserialize, Serialize};

use crate::isa::OpenVmISA;

/// A newtype wrapper around `OpenVmInstruction` to implement the `Instruction` trait.
/// This is necessary because we cannot implement a foreign trait for a foreign type.
#[derive(Serialize, Deserialize)]
pub struct Instr<F, ISA> {
    pub inner: OpenVmInstruction<F>,
    _marker: PhantomData<ISA>,
}

impl<F, ISA> From<OpenVmInstruction<F>> for Instr<F, ISA> {
    fn from(value: OpenVmInstruction<F>) -> Self {
        Self {
            inner: value,
            _marker: PhantomData,
        }
    }
}

// TODO: derive, probably the compiler being too conservative here
impl<F, ISA> Clone for Instr<F, ISA>
where
    OpenVmInstruction<F>: Clone,
{
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
            _marker: PhantomData,
        }
    }
}

impl<F: PrimeField32, ISA: OpenVmISA> Display for Instr<F, ISA> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", ISA::format(&self.inner))
    }
}

impl<F, ISA: OpenVmISA> PcStep for Instr<F, ISA> {
    fn pc_step() -> u32 {
        ISA::DEFAULT_PC_STEP
    }
}

impl<F: PrimeField32, ISA: OpenVmISA> Instruction<F> for Instr<F, ISA> {
    fn pc_lookup_row(&self, pc: u64) -> Vec<F> {
        let args = [
            self.inner.opcode.to_field(),
            self.inner.a,
            self.inner.b,
            self.inner.c,
            self.inner.d,
            self.inner.e,
            self.inner.f,
            self.inner.g,
        ];
        // The PC lookup row has the format:
        // [pc, opcode, a, b, c, d, e, f, g]
        let pc = F::from_canonical_u32(pc.try_into().unwrap());
        once(pc).chain(args).collect()
    }
}
