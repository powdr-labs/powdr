mod common;

mod instruction_tests {
    use std::path::Path;

    use crate::common::verify_riscv_asm_file;
    use powdr_riscv::CompilerOptions;
    use test_log::test;

    fn run_instruction_test(path: &Path) {
        let options_16 = CompilerOptions::new_16();
        run_instruction_test_with_options(path, options_16);

        let options_32 = CompilerOptions::new_32();
        run_instruction_test_with_options(path, options_32);
    }

    fn run_instruction_test_with_options(path: &Path, options: CompilerOptions) {
        // Test from ELF path:
        verify_riscv_asm_file(path, options, false);
    }

    include!(concat!(env!("OUT_DIR"), "/instruction_tests.rs"));
}
