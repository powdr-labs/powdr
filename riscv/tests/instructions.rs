mod instruction_tests {

    use riscv::compiler::compile_riscv_asm;
    use test_log::test;

    fn run_instruction_test(assembly: &str, name: &str) {
        // TODO Should we create one powdr asm from all tests or keep them separate?
        let _powdr_asm = compile_riscv_asm([(name.to_string(), assembly.to_string())].into());

        // verify_asm_string::<GoldilocksField>(&format!("{name}.asm"), &powdr_asm, vec![]);
        unimplemented!()
    }

    include!(concat!(env!("OUT_DIR"), "/instruction_tests.rs"));
}
