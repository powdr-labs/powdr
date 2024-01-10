mod common;

mod instruction_tests {
    use crate::common::verify_riscv_asm_string;
    use riscv::compiler::compile;
    use riscv::CoProcessors;
    use test_log::test;

    fn run_instruction_test(assembly: &str, name: &str) {
        // TODO Should we create one powdr-asm from all tests or keep them separate?
        let powdr_asm = compile(
            [(name.to_string(), assembly.to_string())].into(),
            &CoProcessors::base(),
            false,
        );

        verify_riscv_asm_string(&format!("{name}.asm"), &powdr_asm, Default::default());
    }

    include!(concat!(env!("OUT_DIR"), "/instruction_tests.rs"));
}
