mod instruction_tests {
    use compiler::compile_asm_string_temp;
    use number::GoldilocksField;
    use riscv::compiler::compile_riscv_asm;

    fn run_instruction_test(assembly: &str, name: &str) {
        // TODO Should we create one powdr asm from all tests or keep them separate?
        let powdr_asm = compile_riscv_asm([(name.to_string(), assembly.to_string())].into());

        compile_asm_string_temp::<GoldilocksField>(&format!("{name}.asm"), &powdr_asm, vec![]);
    }

    include!(concat!(env!("OUT_DIR"), "/instruction_tests.rs"));
}
