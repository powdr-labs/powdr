mod common;

mod instruction_tests {
    use std::path::Path;

    use crate::common::{verify_riscv_asm_file, verify_riscv_asm_string};
    use powdr_backend::BackendType;
    use powdr_number::GoldilocksField;
    use powdr_riscv::asm::compile;
    use powdr_riscv::Runtime;
    use test_log::test;

    fn run_instruction_test(path: &Path, name: &str) {
        // Test from ELF path:
        verify_riscv_asm_file(path, &Runtime::base(), false);

        if name == "rvc" {
            // "rvc" test is not supported via assembly path
            return;
        }

        // Test from assembly path:
        // TODO Should we create one powdr-asm from all tests or keep them separate?
        let assembly = std::fs::read_to_string(path).unwrap();
        let powdr_asm = compile::<GoldilocksField>(
            [(name.to_string(), assembly)].into(),
            &Runtime::base(),
            false,
        );

        verify_riscv_asm_string::<()>(
            &format!("{name}.asm"),
            &powdr_asm,
            Default::default(),
            None,
            BackendType::EStarkDumpComposite,
        );
    }

    include!(concat!(env!("OUT_DIR"), "/instruction_tests.rs"));
}
