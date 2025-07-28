use openvm_instructions::{
    instruction::Instruction, riscv::RV32_REGISTER_NUM_LIMBS, LocalOpcode, PhantomDiscriminant,
};
use openvm_instructions_derive::LocalOpcode;
use openvm_stark_backend::p3_field::PrimeField32;
use openvm_transpiler::{TranspilerExtension, TranspilerOutput};
use powdr_openvm_hints_guest::{HintsFunct7, HINTS_FUNCT3, OPCODE};
use rrs_lib::instruction_formats::RType;
use strum::{EnumCount, EnumIter, FromRepr};

#[derive(
    Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, EnumCount, EnumIter, FromRepr, LocalOpcode,
)]
#[opcode_offset = 0x800]
#[repr(usize)]
pub enum HintsOpcode {
    HINTS,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, FromRepr)]
#[repr(u16)]
pub enum HintsPhantom {
    // idk if there is a "proper" way for avoiding conflicts in this number,
    // just looked at ovm code and picked the next range that didn't seem to be
    // used
    HintReverseBytes = 0x60,
    HintK256InverseField = 0x61,
    HintK256InverseField10x26 = 0x62,
    HintK256SqrtField10x26 = 0x63,
}

#[derive(Default)]
pub struct HintsTranspilerExtension;

impl<F: PrimeField32> TranspilerExtension<F> for HintsTranspilerExtension {
    fn process_custom(&self, instruction_stream: &[u32]) -> Option<TranspilerOutput<F>> {
        if instruction_stream.is_empty() {
            return None;
        }
        let instruction_u32 = instruction_stream[0];
        let opcode = (instruction_u32 & 0x7f) as u8;
        if opcode != OPCODE {
            return None;
        }

        let insn = RType::new(instruction_u32);
        if insn.funct3 as u8 != HINTS_FUNCT3 {
            return None;
        }

        let funct7 = HintsFunct7::from_repr(insn.funct7 as u8)?;
        let disc = match funct7 {
            HintsFunct7::ReverseBytes => HintsPhantom::HintReverseBytes,
            HintsFunct7::K256InverseField => HintsPhantom::HintK256InverseField,
            HintsFunct7::K256InverseField10x26 => HintsPhantom::HintK256InverseField10x26,
            HintsFunct7::K256SqrtField10x26 => HintsPhantom::HintK256SqrtField10x26,
        };

        let instruction = Instruction::phantom(
            PhantomDiscriminant(disc as u16),
            F::from_canonical_usize(RV32_REGISTER_NUM_LIMBS * insn.rs1),
            F::ZERO,
            0,
        );

        Some(TranspilerOutput::one_to_one(instruction))
    }
}
