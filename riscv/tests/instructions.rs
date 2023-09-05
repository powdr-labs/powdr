mod instruction_tests {
    use asm_utils::compiler::Compiler;
    use compiler::verify_asm_string;
    use number::GoldilocksField;
    use riscv::compiler::Risc;
    use test_log::test;

    fn run_instruction_test(assembly: &str, name: &str) {
        // TODO Should we create one powdr-asm from all tests or keep them separate?
        let powdr_asm = Risc::compile([(name.to_string(), assembly.to_string())].into());

        verify_asm_string::<GoldilocksField>(&format!("{name}.asm"), &powdr_asm, vec![]);
    }

    include!(concat!(env!("OUT_DIR"), "/instruction_tests.rs"));
}
