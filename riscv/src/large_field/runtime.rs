use std::collections::BTreeMap;

use powdr_syscalls::Syscall;

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
    pub fn new(libs: RuntimeLibs) -> Self {
        let mut runtime = Runtime::base();
        if libs.poseidon2 {
            runtime = runtime.with_poseidon2();
        }
        if libs.keccak {
            runtime = runtime.with_keccak();
        }
        if libs.arith {
            runtime = runtime.with_arith();
        }
        runtime
    }

    pub fn base() -> Self {
        let mut r = Runtime {
            submachines: Default::default(),
            syscalls: Default::default(),
        };

        // Base submachines
        // TODO: can/should the memory machine be part of the runtime also?
        r.add_submachine(
            "std::machines::large_field::binary::Binary",
            None,
            "binary",
            vec!["byte_binary", "MIN_DEGREE", "LARGE_SUBMACHINES_MAX_DEGREE"],
            [
                r#"instr and X, Y, Z, W
                    link ~> tmp1_col = regs.mload(X, STEP)
                    link ~> tmp2_col = regs.mload(Y, STEP + 1)
                    link ~> tmp3_col = binary.and(tmp1_col, tmp2_col + Z)
                    link ~> regs.mstore(W, STEP + 3, tmp3_col);"#,
                r#"instr or X, Y, Z, W
                    link ~> tmp1_col = regs.mload(X, STEP)
                    link ~> tmp2_col = regs.mload(Y, STEP + 1)
                    link ~> tmp3_col = binary.or(tmp1_col, tmp2_col + Z)
                    link ~> regs.mstore(W, STEP + 3, tmp3_col);"#,
                r#"instr xor X, Y, Z, W
                    link ~> tmp1_col = regs.mload(X, STEP)
                    link ~> tmp2_col = regs.mload(Y, STEP + 1)
                    link ~> tmp3_col = binary.xor(tmp1_col, tmp2_col + Z)
                    link ~> regs.mstore(W, STEP + 3, tmp3_col);"#,
            ],
        );

        r.add_submachine(
            "std::machines::large_field::shift::Shift",
            None,
            "shift",
            vec!["byte_shift", "MIN_DEGREE", "LARGE_SUBMACHINES_MAX_DEGREE"],
            [
                r#"instr shl X, Y, Z, W
                    link ~> tmp1_col = regs.mload(X, STEP)
                    link ~> tmp2_col = regs.mload(Y, STEP + 1)
                    link ~> tmp3_col = shift.shl(tmp1_col, tmp2_col + Z)
                    link ~> regs.mstore(W, STEP + 3, tmp3_col);"#,
                r#"instr shr X, Y, Z, W
                    link ~> tmp1_col = regs.mload(X, STEP)
                    link ~> tmp2_col = regs.mload(Y, STEP + 1)
                    link ~> tmp3_col = shift.shr(tmp1_col, tmp2_col + Z)
                    link ~> regs.mstore(W, STEP + 3, tmp3_col);"#,
            ],
        );

        r.add_submachine(
            "std::machines::split::split_gl::SplitGL",
            None,
            "split_gl",
            vec!["byte_compare", "MIN_DEGREE", "LARGE_SUBMACHINES_MAX_DEGREE"],
            [r#"instr split_gl X, Z, W
                    link ~> tmp1_col = regs.mload(X, STEP)
                    link ~> (tmp3_col, tmp4_col) = split_gl.split(tmp1_col)
                    link ~> regs.mstore(Z, STEP + 2, tmp3_col)
                    link ~> regs.mstore(W, STEP + 3, tmp4_col);"#],
        );

        r.add_submachine::<&str, _>("std::machines::range::Bit2", None, "bit2", vec![], []);

        r.add_submachine::<&str, _>("std::machines::range::Bit6", None, "bit6", vec![], []);

        r.add_submachine::<&str, _>("std::machines::range::Bit7", None, "bit7", vec![], []);

        r.add_submachine::<&str, _>("std::machines::range::Byte", None, "byte", vec![], []);

        r.add_submachine::<&str, _>("std::machines::range::Byte2", None, "byte2", vec![], []);

        r.add_submachine::<&str, _>(
            "std::machines::binary::ByteBinary",
            None,
            "byte_binary",
            vec![],
            [],
        );

        r.add_submachine::<&str, _>(
            "std::machines::large_field::shift::ByteShift",
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
                "query_arg_1 <== get_reg(10);",
                "query_arg_2 <== get_reg(11);",
                "set_reg 10, ${ std::prelude::Query::Input(std::convert::int(std::prover::eval(query_arg_1)), std::convert::int(std::prover::eval(query_arg_2))) };",
            ]
        );

        r.add_syscall(
            Syscall::Output,
            // This is using x0 on purpose, because we do not want to introduce
            // nondeterminism with this.
            [
                "query_arg_1 <== get_reg(10);",
                "query_arg_2 <== get_reg(11);",
                "set_reg 0, ${ std::prelude::Query::Output(std::convert::int(std::prover::eval(query_arg_1)), std::prover::eval(query_arg_2)) };"
            ]
        );

        r.add_syscall(Syscall::Halt, ["return;"]);

        r.add_syscall(Syscall::CommitPublic, ["commit_public 10, 11;"]);

        r.add_syscall(Syscall::InvertGL, ["invert_gl 10, 11;"]);

        r.with_poseidon()
    }

    fn with_keccak(mut self) -> Self {
        self.add_submachine(
            "std::machines::hash::keccakf32_memory::Keccakf32Memory",
            None,
            "keccakf",
            vec!["memory", "MIN_DEGREE", "MAIN_MAX_DEGREE"],
            [r#"instr keccakf X, Y
                link ~> tmp1_col = regs.mload(X, STEP)
                link ~> tmp2_col = regs.mload(Y, STEP + 1)
                link ~> keccakf.keccakf32_memory(tmp1_col, tmp2_col, STEP)
            {
                // make sure tmp1_col and tmp2_col are 4-byte aligned memory addresses
                tmp1_col = 4 * (X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000),
                tmp2_col = 4 * (Y_b5 + Y_b6 * 0x100 + Y_b7 * 0x10000 + Y_b8 * 0x1000000)
            }
            "#
            .to_string()],
        );

        // The keccakf syscall has a two arguments passed on x10 and x11,
        // the memory address of the 25 field element input array
        // and the memory address of the 25 field element output array to store results to.
        let implementation = std::iter::once("keccakf 10, 11;".to_string());

        self.add_syscall(Syscall::KeccakF, implementation);
        self
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

    fn with_poseidon(mut self) -> Self {
        self.add_submachine(
            "std::machines::hash::poseidon_gl_memory::PoseidonGLMemory",
            None,
            "poseidon_gl",
            vec![
                "memory",
                "split_gl",
                "MIN_DEGREE",
                "LARGE_SUBMACHINES_MAX_DEGREE",
            ],
            [r#"instr poseidon_gl X, Y
                    link ~> tmp1_col = regs.mload(X, STEP)
                    link ~> tmp2_col = regs.mload(Y, STEP + 1)
                    link ~> poseidon_gl.poseidon_permutation(tmp1_col, tmp2_col, STEP)
                {
                    // make sure tmp1_col and tmp2_col are 4-byte aligned memory addresses
                    tmp1_col = 4 * (X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000),
                    tmp2_col = 4 * (Y_b5 + Y_b6 * 0x100 + Y_b7 * 0x10000 + Y_b8 * 0x1000000)
                }
            "#],
        );

        // The poseidon syscall has a single argument passed on x10, the
        // memory address of the 12 field element input array. Since the memory
        // offset is chosen by LLVM, we assume it's properly aligned.
        let implementation = std::iter::once("poseidon_gl 10, 10;".to_string());

        self.add_syscall(Syscall::PoseidonGL, implementation.clone());
        self.add_syscall(Syscall::NativeHash, implementation);
        self
    }

    fn with_poseidon2(mut self) -> Self {
        self.add_submachine(
            "std::machines::hash::poseidon2_gl::Poseidon2GL",
            None,
            "poseidon2_gl",
            vec!["memory", "MIN_DEGREE", "LARGE_SUBMACHINES_MAX_DEGREE"],
            [r#"instr poseidon2_gl X, Y
                    link ~> tmp1_col = regs.mload(X, STEP)
                    link ~> tmp2_col = regs.mload(Y, STEP + 1)
                    link ~> poseidon2_gl.poseidon2_permutation(tmp1_col, tmp2_col, STEP)
                {
                    // make sure tmp1_col and tmp2_col are 4-byte aligned memory addresses
                    tmp1_col = 4 * (X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000),
                    tmp2_col = 4 * (Y_b5 + Y_b6 * 0x100 + Y_b7 * 0x10000 + Y_b8 * 0x1000000)
                }
            "#],
        );

        self.add_submachine(
            "std::machines::split::split_gl_vec::SplitGLVec8",
            None,
            "split_gl_vec",
            vec!["memory", "split_gl", "MIN_DEGREE", "MAIN_MAX_DEGREE"],
            [
                r#"instr split_gl_vec X, Y
                    link ~> tmp1_col = regs.mload(X, STEP)
                    link ~> tmp2_col = regs.mload(Y, STEP + 1)
                    link ~> split_gl_vec.split(tmp1_col, tmp2_col, STEP + 2)
                {
                    // make sure tmp1_col and tmp2_col are 4-byte aligned memory addresses
                    tmp1_col = 4 * (X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000),
                    tmp2_col = 4 * (Y_b5 + Y_b6 * 0x100 + Y_b7 * 0x10000 + Y_b8 * 0x1000000)
                }
            "#,
                r#"
                // Computes val(X) * 2**32 + val(Y) as a field operation and stores the result
                // at memory address val(Z)
                // val(Z) can be between in range [0, 2**32).
                // val(Z) should be a multiple of 4, but this instruction does not enforce it.
                instr merge_gl X, Y, Z
                    // load lower limb
                    link ~> tmp1_col = regs.mload(X, STEP)
                    // load higher limb
                    link ~> tmp2_col = regs.mload(Y, STEP + 1)
                    // load the result address
                    link ~> tmp3_col = regs.mload(Z, STEP + 2)
                    // store the result at the result address
                    link ~> memory.mstore(X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000, STEP + 3, tmp1_col + tmp2_col * 0x100000000)
                {
                    // Byte decompose the address
                    tmp3_col = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000
                }
            "#,
            ],
        );

        // The poseidon2 syscall has input address passed on x10 and output address passed on x11,
        // they can overlap.
        self.add_syscall(
            Syscall::Poseidon2GL,
            std::iter::once("poseidon2_gl 10, 11;".to_string()),
        );

        self.add_syscall(
            Syscall::SplitGLVec,
            std::iter::once("split_gl_vec 10, 11;".to_string()),
        );
        self.add_syscall(
            Syscall::MergeGL,
            std::iter::once("merge_gl 10, 11, 12;".to_string()),
        );

        self
    }

    pub fn has_submachine(&self, name: &str) -> bool {
        self.submachines.contains_key(name)
    }

    fn with_arith(mut self) -> Self {
        self.add_submachine(
            "std::machines::large_field::arith256_memory::Arith256Memory",
            None,
            "arith",
            vec!["memory", "MIN_DEGREE", "MAIN_MAX_DEGREE"],
            [
                r#"instr affine_256 X, Y, Z, W
                    link ~> tmp1_col = regs.mload(X, STEP)
                    link ~> tmp2_col = regs.mload(Y, STEP)
                    link ~> tmp3_col = regs.mload(Z, STEP)
                    link ~> tmp4_col = regs.mload(W, STEP)
                    link ~> arith.affine_256(STEP, tmp1_col, tmp2_col, tmp3_col, tmp4_col);
            "#,
                r#"instr ec_add X, Y, W
                    link ~> tmp1_col = regs.mload(X, STEP)
                    link ~> tmp2_col = regs.mload(Y, STEP)
                    link ~> tmp4_col = regs.mload(W, STEP)
                    link ~> arith.ec_add(STEP, tmp1_col, tmp2_col, tmp4_col);
            "#,
                r#"instr ec_double X, W
                    link ~> tmp1_col = regs.mload(X, STEP)
                    link ~> tmp4_col = regs.mload(W, STEP)
                    link ~> arith.ec_double(STEP, tmp1_col, tmp4_col);
            "#,
                r#"instr mod_256 X, Y, W
                    link ~> tmp1_col = regs.mload(X, STEP)
                    link ~> tmp2_col = regs.mload(Y, STEP)
                    link ~> tmp4_col = regs.mload(W, STEP)
                    link ~> arith.mod_256(STEP, tmp1_col, tmp2_col, tmp4_col);
            "#,
            ],
        );

        let affine256 = std::iter::once("affine_256 10, 11, 12, 13;".to_string());
        self.add_syscall(Syscall::Affine256, affine256);

        let mod256 = std::iter::once("mod_256 10, 11, 12;".to_string());
        self.add_syscall(Syscall::Mod256, mod256);

        let ec_add = std::iter::once("ec_add 10, 11, 12;".to_string());
        self.add_syscall(Syscall::EcAdd, ec_add);

        let ec_double = std::iter::once("ec_double 10, 11;".to_string());
        self.add_syscall(Syscall::EcDouble, ec_double);

        self
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
            format!(
                "branch_if_diff_equal 5, 0, {}, __ecall_handler_{};",
                s.syscall as u8, s.syscall
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
