use std::{collections::BTreeMap, convert::TryFrom};

use powdr_riscv_syscalls::Syscall;

use powdr_ast::parsed::asm::{FunctionStatement, MachineStatement, SymbolPath};

use itertools::Itertools;
use powdr_parser::ParserContext;

use crate::code_gen::Register;

static EXTRA_REG_PREFIX: &str = "xtra";

lazy_static::lazy_static! {
    static ref INSTRUCTION_DECLARATION_PARSER: powdr_parser::powdr::InstructionDeclarationParser
        = powdr_parser::powdr::InstructionDeclarationParser::new();
    static ref FUNCTION_STATEMENT_PARSER: powdr_parser::powdr::FunctionStatementParser
        = powdr_parser::powdr::FunctionStatementParser::new();
}

pub fn parse_instruction_declaration(input: &str) -> MachineStatement {
    let ctx = ParserContext::new(None, input);
    INSTRUCTION_DECLARATION_PARSER
        .parse(&ctx, input)
        .expect("invalid instruction declaration")
}

pub fn parse_function_statement(input: &str) -> FunctionStatement {
    let ctx = ParserContext::new(None, input);
    FUNCTION_STATEMENT_PARSER
        .parse(&ctx, input)
        .expect("invalid function statement")
}

struct SubMachine {
    /// Full path to machine (e.g, `path::to::Machine`)
    path: SymbolPath,
    /// Optional alias (`use path::to::Machine as TheAlias;`)
    alias: Option<String>,
    /// Instance declaration name,
    instance_name: String,
    /// Arguments
    arguments: Vec<String>,
    /// Instruction declarations
    instructions: Vec<MachineStatement>,
    /// Number of registers needed by this machine's instruction declarations if > 4.
    extra_registers: u8,
    /// TODO: only needed because of witgen requiring that each machine be called at least once
    init_call: Vec<FunctionStatement>,
}

impl SubMachine {
    fn import(&self) -> String {
        format!(
            "use {}{}{};",
            self.path,
            self.alias.as_deref().map(|_| " as ").unwrap_or_default(),
            self.alias.as_deref().unwrap_or_default()
        )
    }

    fn declaration(&self) -> String {
        let ty = self.alias.as_deref().unwrap_or(self.path.name());
        let args = if self.arguments.is_empty() {
            "".to_string()
        } else {
            format!("({})", self.arguments.join(", "))
        };
        format!("{} {}{};", ty, self.instance_name, args)
    }
}

/// Sequence of asm function statements.
/// Any of the registers used as input/output to the syscall should be usable without issue.
/// Other registers should be saved/restored from memory, as LLVM doesn't know about their usage here.
struct SyscallImpl(Vec<FunctionStatement>);

/// RISCV powdr assembly runtime.
/// Determines submachines, instructions and syscalls available to the main machine.
pub struct Runtime {
    submachines: BTreeMap<String, SubMachine>,
    syscalls: BTreeMap<Syscall, SyscallImpl>,
}

impl Runtime {
    pub fn base() -> Self {
        let mut r = Runtime {
            submachines: Default::default(),
            syscalls: Default::default(),
        };

        // Base submachines
        // TODO: can/should the memory machine be part of the runtime also?
        r.add_submachine(
            "std::machines::binary::Binary",
            None,
            "binary",
            vec!["byte_binary"],
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
            ["and 0, 0, 0, 0;"],
        );

        r.add_submachine(
            "std::machines::shift::Shift",
            None,
            "shift",
            vec!["byte_shift"],
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
            ["shl 0, 0, 0, 0;"],
        );

        r.add_submachine(
            "std::machines::split::split_gl::SplitGL",
            None,
            "split_gl",
            vec!["byte_compare"],
            [r#"instr split_gl X, Z, W
                    link ~> tmp1_col = regs.mload(X, STEP)
                    link ~> (tmp3_col, tmp4_col) = split_gl.split(tmp1_col)
                    link ~> regs.mstore(Z, STEP + 2, tmp3_col)
                    link ~> regs.mstore(W, STEP + 3, tmp4_col);"#],
            0,
            ["split_gl 0, 0, 0;"],
        );

        r.add_submachine::<&str, _, _>(
            "std::machines::range::Bit2",
            None,
            "bit2",
            vec![],
            [],
            0,
            [],
        );

        r.add_submachine::<&str, _, _>(
            "std::machines::range::Bit6",
            None,
            "bit6",
            vec![],
            [],
            0,
            [],
        );

        r.add_submachine::<&str, _, _>(
            "std::machines::range::Bit7",
            None,
            "bit7",
            vec![],
            [],
            0,
            [],
        );

        r.add_submachine::<&str, _, _>(
            "std::machines::range::Byte",
            None,
            "byte",
            vec![],
            [],
            0,
            [],
        );

        r.add_submachine::<&str, _, _>(
            "std::machines::range::Byte2",
            None,
            "byte2",
            vec![],
            [],
            0,
            [],
        );

        r.add_submachine::<&str, _, _>(
            "std::machines::binary::ByteBinary",
            None,
            "byte_binary",
            vec![],
            [],
            0,
            [],
        );

        r.add_submachine::<&str, _, _>(
            "std::machines::shift::ByteShift",
            None,
            "byte_shift",
            vec![],
            [],
            0,
            [],
        );

        r.add_submachine::<&str, _, _>(
            "std::machines::split::ByteCompare",
            None,
            "byte_compare",
            vec![],
            [],
            0,
            [],
        );

        // Base syscalls
        r.add_syscall(
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
                "set_reg 0, ${ std::prelude::Query::Output(std::convert::int(std::prover::eval(query_arg_1)), std::convert::int(std::prover::eval(query_arg_2))) };"
            ]
        );

        r.add_syscall(Syscall::Halt, ["return;"]);

        r
    }

    pub fn has_submachine(&self, name: &str) -> bool {
        self.submachines.contains_key(name)
    }

    pub fn has_syscall(&self, s: Syscall) -> bool {
        self.syscalls.contains_key(&s)
    }

    pub fn with_keccak(mut self) -> Self {
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
            std::iter::once("set_reg 10, 0x100;".to_string()) // filler value for input pointer
                .chain(std::iter::once("set_reg 11, 0x300;".to_string())) // filler value for output pointer (at least 200 bytes away)
                .chain(std::iter::once("keccakf 10, 11;".to_string())) // must be called at least once
                .chain((0..50).flat_map(|i| store_word(11, i as u32 * 4, "x0"))), // zero out 200 bytes following output pointer
        );

        // The keccakf syscall has a two arguments passed on x10 and x11,
        // the memory address of the 25 field element input array
        // and the memory address of the 25 field element output array to store results to.
        let implementation = std::iter::once("keccakf 10, 11;".to_string());

        self.add_syscall(Syscall::KeccakF, implementation);
        self
    }

    fn with_poseidon(mut self, continuations: bool) -> Self {
        let init_call = if continuations {
            vec![
                "mstore_bootloader 0, 0, 0, 0;",
                "mstore_bootloader 0, 0, 4, 0;",
                "mstore_bootloader 0, 0, 8, 0;",
                "mstore_bootloader 0, 0, 12, 0;",
                "mstore_bootloader 0, 0, 16, 0;",
                "mstore_bootloader 0, 0, 20, 0;",
                "mstore_bootloader 0, 0, 24, 0;",
                "mstore_bootloader 0, 0, 28, 0;",
                "mstore_bootloader 0, 0, 32, 0;",
                "mstore_bootloader 0, 0, 36, 0;",
                "mstore_bootloader 0, 0, 40, 0;",
                "mstore_bootloader 0, 0, 44, 0;",
                "mstore_bootloader 0, 0, 48, 0;",
                "mstore_bootloader 0, 0, 52, 0;",
                "mstore_bootloader 0, 0, 56, 0;",
                "mstore_bootloader 0, 0, 60, 0;",
                "mstore_bootloader 0, 0, 64, 0;",
                "mstore_bootloader 0, 0, 68, 0;",
                "mstore_bootloader 0, 0, 72, 0;",
                "mstore_bootloader 0, 0, 76, 0;",
                "mstore_bootloader 0, 0, 80, 0;",
                "mstore_bootloader 0, 0, 84, 0;",
                "mstore_bootloader 0, 0, 88, 0;",
                "mstore_bootloader 0, 0, 92, 0;",
                "poseidon_gl 0, 0;",
            ]
        } else {
            vec!["poseidon_gl 0, 0;"]
        };
        self.add_submachine(
            "std::machines::hash::poseidon_gl_memory::PoseidonGLMemory",
            None,
            "poseidon_gl",
            vec!["memory", "split_gl"],
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
            init_call,
        );

        // The poseidon syscall has a single argument passed on x10, the
        // memory address of the 12 field element input array. Since the memory
        // offset is chosen by LLVM, we assume it's properly aligned.
        let implementation = std::iter::once("poseidon_gl 10, 10;".to_string());

        self.add_syscall(Syscall::PoseidonGL, implementation);
        self
    }

    pub fn with_poseidon_no_continuations(self) -> Self {
        self.with_poseidon(false)
    }

    pub fn with_poseidon_for_continuations(self) -> Self {
        self.with_poseidon(true)
    }
    pub fn with_arith(mut self) -> Self {
        self.add_submachine(
            "std::machines::arith::Arith",
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
            // calling ec_double for machine initialization.
            // store x in registers 0..8
            [
                0x60297556u32,
                0x2f057a14,
                0x8568a18b,
                0x82f6472f,
                0x355235d3,
                0x20453a14,
                0x755eeea4,
                0xfff97bd5,
            ]
            .into_iter()
            .enumerate()
            .map(|(i, fe)| format!("{} <=X= {fe};", reg(i)))
            // store y in registers 8..16
            .chain(
                [
                    0xb075f297u32,
                    0x3c870c36,
                    0x518fe4a0,
                    0xde80f0f6,
                    0x7f45c560,
                    0xf3be9601,
                    0xacfbb620,
                    0xae12777a,
                ]
                .into_iter()
                .enumerate()
                .map(|(i, fe)| format!("{} <=X= {fe};", reg(i + 8))),
            )
            // call machine instruction
            .chain(std::iter::once("ec_double;".to_string()))
            // set output registers to zero
            .chain((0..16).map(|i| format!("{} <=X= 0;", reg(i)))),
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

    #[allow(clippy::too_many_arguments)]
    pub fn add_submachine<S: AsRef<str>, I1: IntoIterator<Item = S>, I2: IntoIterator<Item = S>>(
        &mut self,
        path: &str,
        alias: Option<&str>,
        instance_name: &str,
        arguments: Vec<&str>,
        instructions: I1,
        extra_registers: u8,
        init_call: I2,
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
            init_call: init_call
                .into_iter()
                .map(|s| parse_function_statement(s.as_ref()))
                .collect(),
        };
        assert!(
            self.submachines
                .insert(instance_name.to_string(), subm)
                .is_none(),
            "submachine {instance_name} already present"
        );
    }

    pub fn add_syscall<S: AsRef<str>, I: IntoIterator<Item = S>>(
        &mut self,
        syscall: Syscall,
        implementation: I,
    ) {
        let implementation = SyscallImpl(
            implementation
                .into_iter()
                .map(|s| parse_function_statement(s.as_ref()))
                .collect(),
        );

        if self.syscalls.insert(syscall, implementation).is_some() {
            panic!("duplicate syscall {syscall}");
        }
    }

    pub fn submachines_init(&self) -> Vec<String> {
        self.submachines
            .values()
            .flat_map(|m| m.init_call.iter())
            .map(|s| s.to_string())
            .collect()
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

        let jump_table = self.syscalls.keys().map(|s| {
            format!(
                "branch_if_diff_equal 5, 0, {}, __ecall_handler_{};",
                *s as u32, s
            )
        });

        let invalid_handler = ["__invalid_syscall:".to_string(), "fail;".to_string()].into_iter();

        let handlers = self.syscalls.iter().flat_map(|(syscall, implementation)| {
            std::iter::once(format!("__ecall_handler_{syscall}:"))
                .chain(implementation.0.iter().map(|i| i.to_string()))
                .chain([format!("jump_dyn 1, {};", Register::from("tmp1").addr())])
        });

        ecall
            .chain(jump_table)
            .chain(invalid_handler)
            .chain(handlers)
            .chain(std::iter::once("// end of ecall handler".to_string()))
            .collect()
    }

    pub fn submachine_names(&self) -> String {
        self.submachines.keys().join("\n")
    }
}

impl TryFrom<&[&str]> for Runtime {
    type Error = String;

    fn try_from(names: &[&str]) -> Result<Self, Self::Error> {
        let mut runtime = Runtime::base();
        for name in names {
            if runtime.has_submachine(name) {
                continue;
            }
            match *name {
                "poseidon_gl" => runtime = runtime.with_poseidon_no_continuations(),
                "keccakf" => runtime = runtime.with_keccak(),
                "arith" => runtime = runtime.with_arith(),
                _ => return Err(format!("Invalid co-processor specified: {name}")),
            }
        }
        Ok(runtime)
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
