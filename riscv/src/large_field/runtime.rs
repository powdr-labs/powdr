use std::collections::BTreeMap;

use powdr_riscv_syscalls::Syscall;

use itertools::Itertools;

use crate::code_gen::Register;

use crate::runtime::{parse_instruction_declaration, SubMachine, SyscallImpl, EXTRA_REG_PREFIX};
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
            0,
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
            0,
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
            0,
        );

        r.add_submachine::<&str, _>("std::machines::range::Bit2", None, "bit2", vec![], [], 0);

        r.add_submachine::<&str, _>("std::machines::range::Bit6", None, "bit6", vec![], [], 0);

        r.add_submachine::<&str, _>("std::machines::range::Bit7", None, "bit7", vec![], [], 0);

        r.add_submachine::<&str, _>("std::machines::range::Byte", None, "byte", vec![], [], 0);

        r.add_submachine::<&str, _>("std::machines::range::Byte2", None, "byte2", vec![], [], 0);

        r.add_submachine::<&str, _>(
            "std::machines::binary::ByteBinary",
            None,
            "byte_binary",
            vec![],
            [],
            0,
        );

        r.add_submachine::<&str, _>(
            "std::machines::large_field::shift::ByteShift",
            None,
            "byte_shift",
            vec![],
            [],
            0,
        );

        r.add_submachine::<&str, _>(
            "std::machines::split::ByteCompare",
            None,
            "byte_compare",
            vec![],
            [],
            0,
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

        let tmp1 = Register::from("tmp1");
        let tmp2 = Register::from("tmp2");

        r.add_syscall(
            Syscall::InvertGL,
            [
                format!("invert_gl 10, {};", tmp1.addr()),
                format!(
                    "split_gl {}, {}, {};",
                    tmp1.addr(),
                    tmp1.addr(),
                    tmp2.addr()
                ),
                format!("mstore 11, 0, 0, {};", tmp1.addr()),
                format!("mstore 11, 0, 4, {};", tmp2.addr()),
            ],
        );

        r.with_poseidon()
    }

    fn with_keccak(mut self) -> Self {
        self.add_submachine(
            "std::machines::hash::keccakf::KeccakF",
            None,
            "keccakf",
            vec!["memory"],
            [r#"instr keccakf X, Y
                    link ~> tmp1_col = regs.mload(X, STEP),
                    link ~> tmp2_col = regs.mload(Y, STEP + 1)
                    link ~> keccakf.keccakf(tmp1_col, tmp2_col, STEP)
                {
                    // make sure tmp1_col and tmp2_col are aligned memory addresses
                    tmp3_col * 4 = tmp1_col,
                    tmp4_col * 4 = tmp2_col,
                    // make sure the factors fit in 32 bits
                    tmp3_col = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000,
                    tmp4_col = Y_b5 + Y_b6 * 0x100 + Y_b7 * 0x10000 + Y_b8 * 0x1000000
                }
            "#
            .to_string()],
            0,
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
        extra_registers: u8,
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
            extra_registers,
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
                    // make sure tmp1_col and tmp2_col are aligned memory addresses
                    tmp3_col * 4 = tmp1_col,
                    tmp4_col * 4 = tmp2_col,
                    // make sure the factors fit in 32 bits
                    tmp3_col = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000,
                    tmp4_col = Y_b5 + Y_b6 * 0x100 + Y_b7 * 0x10000 + Y_b8 * 0x1000000
                }
            "#],
            0,
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
            vec![
                "memory",
                "split_gl",
                "MIN_DEGREE",
                "LARGE_SUBMACHINES_MAX_DEGREE",
            ],
            [r#"instr poseidon2_gl X, Y
                    link ~> tmp1_col = regs.mload(X, STEP)
                    link ~> tmp2_col = regs.mload(Y, STEP + 1)
                    link ~> poseidon2_gl.poseidon2_permutation(tmp1_col, tmp2_col, STEP)
                {
                    // make sure tmp1_col and tmp2_col are aligned memory addresses
                    tmp3_col * 4 = tmp1_col,
                    tmp4_col * 4 = tmp2_col,
                    // make sure the factors fit in 32 bits
                    tmp3_col = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000,
                    tmp4_col = Y_b5 + Y_b6 * 0x100 + Y_b7 * 0x10000 + Y_b8 * 0x1000000
                }
            "#],
            0,
        );

        // The poseidon2 syscall has input address passed on x10 and output address passed on x11,
        // they can overlap.
        let implementation = std::iter::once("poseidon2_gl 10, 11;".to_string());

        self.add_syscall(Syscall::Poseidon2GL, implementation);
        self
    }

    pub fn has_submachine(&self, name: &str) -> bool {
        self.submachines.contains_key(name)
    }

    fn with_arith(mut self) -> Self {
        self.add_submachine(
            "std::machines::large_field::arith::Arith",
            None,
            "arith",
            vec![],
            [
                format!(
                    "instr affine_256 link ~> {};",
                    instr_link("arith.affine_256", 24, 16)
                ),
                format!(
                    "instr ec_add link ~> {};",
                    instr_link("arith.ec_add", 32, 16)
                ),
                format!(
                    "instr ec_double link ~> {};",
                    instr_link("arith.ec_double", 16, 16)
                ),
                format!(
                    "instr mod_256 link ~> {};",
                    instr_link("arith.mod_256", 24, 8)
                ),
            ],
            32,
        );

        // The affine_256 syscall takes as input the addresses of x1, y1 and x2.
        let affine256 =
            // Load x1 in 0..8
            (0..8).flat_map(|i| load_word(10, i as u32 *4 , &reg(i)))
            // Load y1 in 8..16
            .chain((0..8).flat_map(|i| load_word(11, i as u32 *4 , &reg(i + 8))))
            // Load x2 in 16..24
            .chain((0..8).flat_map(|i| load_word(12, i as u32 *4 , &reg(i + 16))))
            // Call instruction
            .chain(std::iter::once("affine_256;".to_string()))
            // Store result y2 in x1's memory
            .chain((0..8).flat_map(|i| store_word(10, i as u32 *4 , &reg(i))))
            // Store result y3 in y1's memory
            .chain((0..8).flat_map(|i| store_word(11, i as u32 *4 , &reg(i + 8))));

        self.add_syscall(Syscall::Affine256, affine256);

        // The mod_256 syscall takes as input the addresses of y2, y3, and x1.
        let mod256 =
            // Load y2 in 0..8
            (0..8).flat_map(|i| load_word(10, i as u32 *4 , &reg(i)))
            // Load y3 in 8..16
            .chain((0..8).flat_map(|i| load_word(11, i as u32 *4 , &reg(i + 8))))
            // Load x1 in 16..24
            .chain((0..8).flat_map(|i| load_word(12, i as u32 *4 , &reg(i + 16))))
            // Call instruction
            .chain(std::iter::once("mod_256;".to_string()))
            // Store result x2 in y2's memory
            .chain((0..8).flat_map(|i| store_word(10, i as u32 *4 , &reg(i))));

        self.add_syscall(Syscall::Mod256, mod256);

        // The ec_add syscall takes as input the four addresses of x1, y1, x2, y2.
        let ec_add =
            // Load x1 in 0..8
            (0..8).flat_map(|i| load_word(10, i as u32 * 4, &reg(i)))
            // Load y1 in 8..16
            .chain((0..8).flat_map(|i| load_word(11, i as u32 * 4, &reg(i + 8))))
            // Load x2 in 16..24
            .chain((0..8).flat_map(|i| load_word(12, i as u32 * 4, &reg(i + 16))))
            // Load y2 in 24..32
            .chain((0..8).flat_map(|i| load_word(13, i as u32 * 4, &reg(i + 24))))
            // Call instruction
            .chain(std::iter::once("ec_add;".to_string()))
            // Save result x3 in x1
            .chain((0..8).flat_map(|i| store_word(10, i as u32 * 4, &reg(i))))
            // Save result y3 in y1
            .chain((0..8).flat_map(|i| store_word(11, i as u32 * 4, &reg(i + 8))));

        self.add_syscall(Syscall::EcAdd, ec_add);

        // The ec_double syscall takes as input the addresses of x and y in x10 and x11 respectively.
        // We load x and y from memory into registers 0..8 and registers 8..16 respectively.
        // We then store the result from those registers into the same addresses (x10 and x11).
        let ec_double =
            // Load x in 0..8
            (0..8).flat_map(|i| load_word(10, i as u32 * 4, &reg(i)))
            // Load y in 8..16
            .chain((0..8).flat_map(|i| load_word(11, i as u32 * 4, &reg(i + 8))))
            // Call instruction
            .chain(std::iter::once("ec_double;".to_string()))
            // Store result in x
            .chain((0..8).flat_map(|i| store_word(10, i as u32 * 4, &reg(i))))
            // Store result in y
            .chain((0..8).flat_map(|i| store_word(11, i as u32 * 4, &reg(i + 8))));

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

    pub fn submachines_extra_registers(&self) -> Vec<String> {
        let count = self
            .submachines
            .values()
            .map(|m| m.extra_registers)
            .max()
            .unwrap_or(0);

        (0..count)
            .map(|i| format!("reg {EXTRA_REG_PREFIX}{i};"))
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

/// Helper function for register names used in instruction params
fn reg(idx: usize) -> String {
    format!("{EXTRA_REG_PREFIX}{idx}")
}

/// Helper function to generate instr link for large number input/output registers
fn instr_link(call: &str, inputs: usize, outputs: usize) -> String {
    format!(
        "{}{}({})",
        if outputs > 0 {
            format!(
                "({}) = ",
                (0..outputs).map(|i| format!("{}'", reg(i))).join(", ")
            )
        } else {
            "".to_string()
        },
        call,
        (0..inputs).map(reg).join(", ")
    )
}

/// Load word from addr+offset into register
fn load_word(addr_reg_id: u32, offset: u32, reg: &str) -> [String; 2] {
    let tmp1 = Register::from("tmp1");
    let tmp2 = Register::from("tmp2");
    [
        format!(
            "mload {addr_reg_id}, {offset}, {}, {};",
            tmp1.addr(),
            tmp2.addr()
        ),
        format!("{reg} <=X= get_reg({});", tmp1.addr()),
    ]
}

/// Store word from register into addr+offset
fn store_word(addr_reg_id: u32, offset: u32, reg: &str) -> [String; 3] {
    let tmp1 = Register::from("tmp1");
    let tmp2 = Register::from("tmp2");
    [
        // split_gl ensures we store a 32-bit value
        format!("set_reg {}, {reg};", tmp1.addr()),
        format!(
            "split_gl {}, {}, {};",
            tmp1.addr(),
            tmp1.addr(),
            tmp2.addr()
        ),
        format!("mstore {addr_reg_id}, 0, {offset}, {};", tmp1.addr()),
    ]
}
