mod common;

mod instruction_tests {
    use std::path::Path;

    use crate::common::verify_riscv_asm_file;
    use powdr_number::KnownField;
    use powdr_riscv::{CompilerOptions, RuntimeEnum};
    use test_log::test;

    fn run_instruction_test(path: &Path) {
        let options_16 = CompilerOptions {
            field: KnownField::BabyBearField,
            runtime: RuntimeEnum::base_16(),
        };
        run_instruction_test_with_options(path, options_16);

        let options_32 = CompilerOptions {
            field: KnownField::GoldilocksField,
            runtime: RuntimeEnum::base_32(),
        };
        run_instruction_test_with_options(path, options_32);
    }

    fn run_instruction_test_with_options(path: &Path, options: CompilerOptions) {
        // Test from ELF path:
        verify_riscv_asm_file(path, options, false);
    }

    include!(concat!(env!("OUT_DIR"), "/instruction_tests.rs"));
}
