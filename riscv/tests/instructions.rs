mod common;

mod instruction_tests {
    use std::path::Path;

    use crate::common::{verify_riscv_asm_file, verify_riscv_asm_string};
    use powdr_number::BabyBearField;
    use powdr_number::GoldilocksField;
    use powdr_number::KnownField;
    use powdr_riscv::asm::compile;
    use powdr_riscv::{CompilerOptions, RuntimeEnum};
    use test_log::test;

    fn run_instruction_test(path: &Path, name: &str) {
        /*
                let options_16 = CompilerOptions {
                    field: KnownField::BabyBearField,
                    runtime: RuntimeEnum::base_16(),
                };
                run_instruction_test_with_options(path, name, options_16);
        */

        let options_32 = CompilerOptions {
            field: KnownField::GoldilocksField,
            runtime: RuntimeEnum::base_32(),
        };
        run_instruction_test_with_options(path, name, options_32);
    }

    fn run_instruction_test_with_options(path: &Path, name: &str, options: CompilerOptions) {
        // Test from ELF path:
        verify_riscv_asm_file(path, options.clone(), false);

        if name == "rvc" {
            // "rvc" test is not supported via assembly path
            return;
        }

        // Test from assembly path:
        // TODO Should we create one powdr-asm from all tests or keep them separate?
        let assembly = std::fs::read_to_string(path).unwrap();
        let powdr_asm = compile(
            [(name.to_string(), assembly)].into(),
            options.clone(),
            false,
        );

        match options.field {
            KnownField::BabyBearField => {
                verify_riscv_asm_string::<BabyBearField, ()>(
                    &format!("{name}.asm"),
                    &powdr_asm,
                    Default::default(),
                    None,
                );
            }
            KnownField::Mersenne31Field => todo!(),
            KnownField::GoldilocksField => {
                verify_riscv_asm_string::<GoldilocksField, ()>(
                    &format!("{name}.asm"),
                    &powdr_asm,
                    Default::default(),
                    None,
                );
            }
            KnownField::Bn254Field => todo!(),
        }
    }

    include!(concat!(env!("OUT_DIR"), "/instruction_tests.rs"));
}
