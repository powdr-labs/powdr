use std::collections::BTreeMap;

use powdr_riscv_syscalls::Syscall;

use itertools::Itertools;

use crate::code_gen::Register;

use crate::runtime::{parse_instruction_declaration, SubMachine, SyscallImpl};
use crate::RuntimeLibs;

/// RISCV powdr assembly runtime.
/// Determines submachines, instructions and syscalls available to the main machine.
#[derive(Clone)]
pub struct Runtime {
    submachines: BTreeMap<String, SubMachine>,
    syscalls: BTreeMap<&'static str, SyscallImpl>,
}

impl Runtime {
    pub fn new(libs: RuntimeLibs, continuations: bool) -> Self {
        let mut runtime = Runtime::base(continuations);
        if libs.poseidon2 {
            runtime = runtime.with_poseidon2();
        }
        if libs.keccak {
            runtime = runtime.with_keccak();
        }
        if libs.arith {
            runtime = runtime.with_arith();
        }
        if libs.splt_vec {
            runtime = runtime.with_split_vec();
        }
        runtime
    }

    pub fn base(continuations: bool) -> Self {
        let mut r = Runtime {
            submachines: Default::default(),
            syscalls: Default::default(),
        };

        // Base submachines
        // TODO: can/should the memory machine be part of the runtime also?
        r.add_submachine(
            "std::machines::small_field::binary::Binary",
            None,
            "binary",
            vec!["byte_binary", "MIN_DEGREE", "LARGE_SUBMACHINES_MAX_DEGREE"],
            [
                r#"instr and XL, YL, ZH, ZL, WL
                            link ~> (tmp1_h, tmp1_l) = regs.mload(0, XL, STEP)
                            link ~> (tmp2_h, tmp2_l) = regs.mload(0, YL, STEP + 1)
                            link ~> (tmp3_h, tmp3_l) = add_sub.add(tmp2_h, tmp2_l, ZH, ZL)
                            link ~> (tmp4_h, tmp4_l) = binary.and(tmp1_h, tmp1_l, tmp3_h, tmp3_l)
                            link ~> regs.mstore(0, WL, STEP + 3, tmp4_h, tmp4_l);"#,
                r#"instr or XL, YL, ZH, ZL, WL
                            link ~> (tmp1_h, tmp1_l) = regs.mload(0, XL, STEP)
                            link ~> (tmp2_h, tmp2_l) = regs.mload(0, YL, STEP + 1)
                            link ~> (tmp3_h, tmp3_l) = add_sub.add(tmp2_h, tmp2_l, ZH, ZL)
                            link ~> (tmp4_h, tmp4_l) = binary.or(tmp1_h, tmp1_l, tmp3_h, tmp3_l)
                            link ~> regs.mstore(0, WL, STEP + 3, tmp4_h, tmp4_l);"#,
                r#"instr xor XL, YL, ZH, ZL, WL
                            link ~> (tmp1_h, tmp1_l) = regs.mload(0, XL, STEP)
                            link ~> (tmp2_h, tmp2_l) = regs.mload(0, YL, STEP + 1)
                            link ~> (tmp3_h, tmp3_l) = add_sub.add(tmp2_h, tmp2_l, ZH, ZL)
                            link ~> (tmp4_h, tmp4_l) = binary.xor(tmp1_h, tmp1_l, tmp3_h, tmp3_l)
                            link ~> regs.mstore(0, WL, STEP + 3, tmp4_h, tmp4_l);"#,
            ],
        );

        r.add_submachine(
            "std::machines::small_field::shift::Shift",
            None,
            "shift",
            vec!["byte_shift", "MIN_DEGREE", "LARGE_SUBMACHINES_MAX_DEGREE"],
            [
                r#"instr shl XL, YL, ZH, ZL, WL
                    link ~> (tmp1_h, tmp1_l) = regs.mload(0, XL, STEP)
                    link ~> (tmp2_h, tmp2_l) = regs.mload(0, YL, STEP + 1)
                    link ~> (tmp3_h, tmp3_l) = add_sub.add(tmp2_h, tmp2_l, ZH, ZL)
                    link ~> (tmp4_l, tmp4_h) = shift.shl(tmp1_l, tmp1_h, tmp3_l)
                    link ~> regs.mstore(0, WL, STEP + 3, tmp4_h, tmp4_l);
"#,
                r#"instr shr XL, YL, ZH, ZL, WL
                    link ~> (tmp1_h, tmp1_l) = regs.mload(0, XL, STEP)
                    link ~> (tmp2_h, tmp2_l) = regs.mload(0, YL, STEP + 1)
                    link ~> (tmp3_h, tmp3_l) = add_sub.add(tmp2_h, tmp2_l, ZH, ZL)
                    link ~> (tmp4_l, tmp4_h) = shift.shr(tmp1_l, tmp1_h, tmp3_l)
                    link ~> regs.mstore(0, WL, STEP + 3, tmp4_h, tmp4_l);
"#,
            ],
        );

        r.add_submachine::<&str, _>("std::machines::range::Bit2", None, "bit2", vec![], []);

        r.add_submachine::<&str, _>("std::machines::range::Bit6", None, "bit6", vec![], []);

        r.add_submachine::<&str, _>("std::machines::range::Bit7", None, "bit7", vec![], []);

        r.add_submachine::<&str, _>("std::machines::range::Byte", None, "byte", vec![], []);

        r.add_submachine::<&str, _>("std::machines::range::Bit12", None, "bit12", vec![], []);

        r.add_submachine::<&str, _>("std::machines::range::Byte2", None, "byte2", vec![], []);

        r.add_submachine::<&str, _>(
            "std::machines::binary::ByteBinary",
            None,
            "byte_binary",
            vec![],
            [],
        );

        r.add_submachine::<&str, _>(
            "std::machines::small_field::shift::ByteShift",
            None,
            "byte_shift",
            vec![],
            [],
        );

        r.add_submachine::<&str, _>(
            "std::machines::split::ByteCompare",
            None,
            "byte_compare",
            vec![],
            [],
        );

        // Base syscalls
        r.add_syscall(
            // TODO this is a quite inefficient way of getting prover inputs.
            // We need to be able to access the register memory within PIL functions.
            Syscall::Input,
            [
                "query_arg_1_h, query_arg_1_l <== get_reg(10);",
                "query_arg_2_h, query_arg_2_l <== get_reg(11);",
                // TODO fix for BabyBear
                "set_reg 10, 0, ${ std::prelude::Query::Input(std::convert::int(std::prover::eval(query_arg_1_l)), std::convert::int(std::prover::eval(query_arg_2_l))) };",
            ]
        );

        r.add_syscall(
            Syscall::Output,
            // This is using x0 on purpose, because we do not want to introduce
            // nondeterminism with this.
            [
                "query_arg_1_h, query_arg_1_l <== get_reg(10);",
                "query_arg_2_h, query_arg_2_l <== get_reg(11);",
                "set_reg 0, 0, ${ std::prelude::Query::Output(std::convert::int(std::prover::eval(query_arg_1_l)), std::prover::eval(query_arg_2_l)) };"
            ]
        );

        r.add_syscall(Syscall::Halt, ["return;"]);

        r.add_syscall(Syscall::CommitPublic, ["commit_public 10, 11;"]);

        r.with_poseidon(continuations)
    }

    fn with_keccak(self) -> Self {
        todo!()
    }

    #[allow(clippy::too_many_arguments)]
    fn add_submachine<S: AsRef<str>, I1: IntoIterator<Item = S>>(
        &mut self,
        path: &str,
        alias: Option<&str>,
        instance_name: &str,
        arguments: Vec<&str>,
        instructions: I1,
    ) {
        let subm = SubMachine {
            path: str::parse(path).expect("invalid submachine path"),
            alias: alias.map(|s| s.to_string()),
            instance_name: instance_name.to_string(),
            arguments: arguments.into_iter().map(|s| s.to_string()).collect(),
            instructions: instructions
                .into_iter()
                .map(|s| parse_instruction_declaration(s.as_ref()))
                .collect(),
        };
        assert!(
            self.submachines
                .insert(instance_name.to_string(), subm)
                .is_none(),
            "submachine {instance_name} already present"
        );
    }

    fn add_syscall<S: AsRef<str>, I: IntoIterator<Item = S>>(
        &mut self,
        syscall: Syscall,
        implementation: I,
    ) {
        let implementation = SyscallImpl {
            syscall,
            statements: implementation
                .into_iter()
                .map(|s| s.as_ref().to_string())
                .collect(),
        };

        if self
            .syscalls
            .insert(syscall.name(), implementation)
            .is_some()
        {
            panic!("duplicate syscall {syscall}");
        }
    }

    // TODO This implementation is just a placeholder,
    // needed by the runtime "finalize" code.
    fn with_poseidon(mut self, continuations: bool) -> Self {
        assert!(!continuations);

        let implementation = std::iter::once("affine 0, 0, 0, 0, 0, 0;".to_string());

        self.add_syscall(Syscall::NativeHash, implementation);
        self
    }

    fn with_poseidon2(self) -> Self {
        todo!()
    }

    pub fn has_submachine(&self, name: &str) -> bool {
        self.submachines.contains_key(name)
    }

    fn with_arith(self) -> Self {
        todo!()
    }

    fn with_split_vec(self) -> Self {
        todo!()
    }

    pub fn submachines_import(&self) -> String {
        self.submachines.values().map(|m| m.import()).join("\n")
    }

    pub fn submachines_declare(&self) -> String {
        self.submachines
            .values()
            .map(|m| m.declaration())
            .join("\n")
    }

    pub fn submachines_instructions(&self) -> Vec<String> {
        self.submachines
            .values()
            .flat_map(|m| m.instructions.iter())
            .map(|s| s.to_string())
            .collect()
    }

    pub fn ecall_handler(&self) -> Vec<String> {
        let ecall = [
            "// ecall handler".to_string(),
            "__ecall_handler:".to_string(),
        ]
        .into_iter();

        let jump_table = self.syscalls.values().map(|s| {
            let opcode = s.syscall as u8;
            format!(
                "branch_if_diff_equal 5, 0, 0, {opcode}, __ecall_handler_{};",
                s.syscall
            )
        });

        let invalid_handler = ["__invalid_syscall:".to_string(), "fail;".to_string()].into_iter();

        let handlers = self.syscalls.iter().flat_map(|(syscall, implementation)| {
            std::iter::once(format!("__ecall_handler_{syscall}:"))
                .chain(implementation.statements.iter().cloned())
                .chain([format!("jump_dyn 1, {};", Register::from("tmp1").addr())])
        });

        ecall
            .chain(jump_table)
            .chain(invalid_handler)
            .chain(handlers)
            .chain(std::iter::once("// end of ecall handler".to_string()))
            .collect()
    }

    pub fn get_syscall_impl(&self, syscall_name: &str) -> Option<&SyscallImpl> {
        self.syscalls.get(syscall_name)
    }
}
