mod common;

mod instruction_tests {
    use std::path::Path;

    use crate::common::verify_riscv_asm_file;
    use powdr_riscv::Runtime;
    use test_log::test;

    fn run_instruction_test(path: &Path) {
        // Test from ELF path:
        verify_riscv_asm_file(path, &Runtime::base(), false);
    }

    include!(concat!(env!("OUT_DIR"), "/instruction_tests.rs"));
}
