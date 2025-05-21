use powdr_number::FieldSize;
use powdr_riscv_types::RiscVProgram;

use crate::CompilerOptions;

use crate::large_field;
use crate::small_field;

/// Translates a RISC-V program to POWDR ASM.
///
/// Will call each of the methods in the `RiscVProgram` just once.
pub fn translate_program(program: impl RiscVProgram, options: CompilerOptions) -> String {
    match options.field.field_size() {
        FieldSize::Small => small_field::code_gen::translate_program(program, options),
        FieldSize::Large => large_field::code_gen::translate_program(program, options),
    }
}
