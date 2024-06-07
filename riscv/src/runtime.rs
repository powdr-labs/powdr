use std::{collections::BTreeMap, convert::TryFrom};

use powdr_riscv_syscalls::Syscall;

use powdr_ast::parsed::asm::{FunctionStatement, MachineStatement, SymbolPath};

use itertools::Itertools;
use powdr_parser::ParserContext;

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
        format!("{} {};", ty, self.instance_name)
    }
}

/// Sequence of asm function statements.
/// Any of the registers used as input/output to the syscall should be usable without issue.
/// Other registers should be saved/restored from memory, as LLVM doesn't know about their usage here.
struct SyscallImpl(Vec<FunctionStatement>);

/// RISCV powdr assembly runtime.
/// Determines submachines, instructions and syscalls avaiable to the main machine.
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
            [
                "instr and Y ~ binary.and val1, (val2 + Y) -> val3';",
                "instr or Y ~ binary.or val1, (val2 + Y) -> val3';",
                "instr xor Y ~ binary.xor val1, (val2 + Y) -> val3';",
            ],
            0,
            ["and 0;"],
        );

        r.add_submachine(
            "std::machines::shift::Shift",
            None,
            "shift",
            [
                "instr shl ~ shift.shl val1, val2 -> val3';",
                "instr shr ~ shift.shr val1, val2 -> val3';",
            ],
            0,
            ["shl;"],
        );

        r.add_submachine(
            "std::machines::split::split_gl::SplitGL",
            None,
            "split_gl",
            ["instr split_gl ~ split_gl.split val1 -> val3', val4';"],
            0,
            ["split_gl;", "val1 <=X= 0;", "val2 <=X= 0;"],
        );

        // Base syscalls
        r.add_syscall(
            Syscall::Input,
            [
                "val1 <== get_reg(10);",
                "val3 <=X= ${ std::prover::Query::Input(std::convert::int(std::prover::eval(val1))) };",
                "set_reg 10, val3;"
            ],
        );

        r.add_syscall(
            Syscall::DataIdentifier,
            [
                "val1 <== get_reg(10);",
                "val2 <== get_reg(11);",
                "val3 <=X= ${ std::prover::Query::DataIdentifier(std::convert::int(std::prover::eval(val2)), std::convert::int(std::prover::eval(val1))) };",
                "set_reg 10, val3;"
            ]
        );

        r.add_syscall(
            Syscall::Output,
            // This is using x0 on purpose, because we do not want to introduce
            // nondeterminism with this.
            [
                "val1 <== get_reg(10);",
                "val2 <== get_reg(11);",
                "set_reg 0, ${ std::prover::Query::Output(std::convert::int(std::prover::eval(val1)), std::convert::int(std::prover::eval(val2))) };"
            ]
        );

        r
    }

    pub fn has_submachine(&self, name: &str) -> bool {
        self.submachines.contains_key(name)
    }

    pub fn has_syscall(&self, s: Syscall) -> bool {
        self.syscalls.contains_key(&s)
    }

    pub fn with_poseidon(mut self) -> Self {
        self.add_submachine(
            "std::machines::hash::poseidon_gl::PoseidonGL",
            None,
            "poseidon_gl",
            [format!(
                "instr poseidon_gl ~ poseidon_gl.poseidon_permutation {};",
                instr_register_params(0, 12, 4)
            )],
            12,
            // init call
            std::iter::once("poseidon_gl;".to_string())
                // zero out output registers
                .chain((0..4).map(|i| format!("{} <== get_reg(0);", reg(i)))),
        );

        // The poseidon syscall has a single argument passed on x10, the
        // memory address of the 12 field element input array. Since the memory
        // offset is chosen by LLVM, we assume it's properly aligned.
        let implementation =
            // The poseidon syscall uses x10 for input, we store it in tmp3 and
            // reuse x10 as input to the poseidon machine instruction.
            std::iter::once("tmp3 <== get_reg(10);".to_string())
            // The poseidon instruction uses registers 0..12 as input/output.
            // The memory field elements are loaded into these registers before calling the instruction.
            // They might be in use by the riscv machine, so we save the registers on the stack.
            .chain((0..12).flat_map(|i| load_gl_fe("tmp3", i as u32 * 8, &reg(i))))
            .chain(std::iter::once("poseidon_gl;".to_string()))
            .chain((0..4).flat_map(|i| store_gl_fe("tmp3", i as u32 * 8, &reg(i))));

        self.add_syscall(Syscall::PoseidonGL, implementation);
        self
    }

    pub fn with_arith(mut self) -> Self {
        self.add_submachine(
            "std::machines::arith::Arith",
            None,
            "arith",
            [
                format!(
                    "instr affine_256 ~ arith.affine_256 {};",
                    instr_register_params(0, 24, 16) // will use registers 0..24
                ),
                format!(
                    "instr ec_add ~ arith.ec_add {};",
                    instr_register_params(0, 32, 16) // will use registers 0..32
                ),
                format!(
                    "instr ec_double ~ arith.ec_double {};",
                    instr_register_params(0, 16, 16) // will use registers 0..16
                ),
                format!(
                    "instr mod_256 ~ arith.mod_256 {};",
                    instr_register_params(0, 24, 8) // will use registers 0..24
                ),
            ],
            // machine needs 32 registers
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
            (0..8).flat_map(|i| load_word(10, i as u32 *4 , &reg(i + 0)))
            // Load y1 in 8..16
            .chain((0..8).flat_map(|i| load_word(11, i as u32 *4 , &reg(i + 8))))
            // Load x2 in 16..24
            .chain((0..8).flat_map(|i| load_word(12, i as u32 *4 , &reg(i + 16))))
            // Call instruction
            .chain(std::iter::once("affine_256;".to_string()))
            // Store result y2 in x1's memory
            .chain((0..8).flat_map(|i| store_word(10, i as u32 *4 , &reg(i + 0))))
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

    pub fn add_submachine<S: AsRef<str>, I1: IntoIterator<Item = S>, I2: IntoIterator<Item = S>>(
        &mut self,
        path: &str,
        alias: Option<&str>,
        instance_name: &str,
        instructions: I1,
        extra_registers: u8,
        init_call: I2,
    ) {
        let subm = SubMachine {
            path: str::parse(path).expect("invalid submachine path"),
            alias: alias.map(|s| s.to_string()),
            instance_name: instance_name.to_string(),
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

    pub fn global_declarations(&self) -> String {
        [
            "__divdi3",
            "__udivdi3",
            "__udivti3",
            "__divdf3",
            "__muldf3",
            "__moddi3",
            "__umoddi3",
            "__umodti3",
            "__eqdf2",
            "__ltdf2",
            "__nedf2",
            "__unorddf2",
            "__floatundidf",
            "__extendsfdf2",
            "memcpy",
            "memmove",
            "memset",
            "memcmp",
            "bcmp",
            "strlen",
        ]
        .map(|n| format!(".globl {n}@plt\n.globl {n}\n.set {n}@plt, {n}\n"))
        .join("\n\n")
            + &[("__rust_alloc_error_handler", "__rg_oom")]
                .map(|(n, m)| format!(".globl {n}\n.set {n}, {m}\n"))
                .join("\n\n")
            +
            // some extra symbols expected by rust code:
            // - __rust_no_alloc_shim_is_unstable: compilation time acknowledgment that this feature is unstable.
            // - __rust_alloc_error_handler_should_panic: needed by the default alloc error handler,
            //   not sure why it's not present in the asm.
            //   https://github.com/rust-lang/rust/blob/ae9d7b0c6434b27e4e2effe8f05b16d37e7ef33f/library/alloc/src/alloc.rs#L415
            r".data
.globl __rust_alloc_error_handler_should_panic
__rust_alloc_error_handler_should_panic: .byte 0
.globl __rust_no_alloc_shim_is_unstable
__rust_no_alloc_shim_is_unstable: .byte 0
.text
"
    }

    pub fn ecall_handler(&self) -> Vec<String> {
        let ecall = [
            "// ecall handler".to_string(),
            "__ecall_handler:".to_string(),
            "val1 <== get_reg(5);".to_string(),
        ]
        .into_iter();

        let jump_table = self.syscalls.keys().map(|s| {
            format!(
                "val2 <=X= {};branch_if_zero __ecall_handler_{};",
                *s as u32, s
            )
        });

        let invalid_handler = ["__invalid_syscall:".to_string(), "fail;".to_string()].into_iter();

        let handlers = self.syscalls.iter().flat_map(|(syscall, implementation)| {
            std::iter::once(format!("__ecall_handler_{syscall}:"))
                .chain(implementation.0.iter().map(|i| i.to_string()))
                .chain(["val1 <== get_reg(1);".to_string(), "jump_dyn;".to_string()])
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
                "poseidon_gl" => runtime = runtime.with_poseidon(),
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

/// Helper function to generate params (i.e., "A, B -> C, D") for instruction declarations using registers
fn instr_register_params(start_idx: usize, inputs: usize, outputs: usize) -> String {
    format!(
        "{} -> {}",
        (start_idx..start_idx + inputs).map(reg).join(", "),
        (start_idx..start_idx + outputs)
            .map(|i| format!("{}'", reg(i)))
            .join(", "),
    )
}

/// Load gl field element from addr+offset into register
fn load_gl_fe(addr: &str, offset: u32, reg: &str) -> [String; 6] {
    let lo = offset;
    let hi = offset + 4;
    [
        format!("val1 <=X= {addr};"),
        format!("mload {lo};"),
        format!("{reg} <=X= val3;"),
        format!("val1 <=X= {addr};"),
        format!("mload {hi};"),
        format!("{reg} <=X= {reg} + val3 * 2**32;"),
    ]
}

/// Store gl field element from register into addr+offset
fn store_gl_fe(addr: &str, offset: u32, reg: &str) -> [String; 7] {
    let lo = offset;
    let hi = offset + 4;
    [
        format!("val1 <=X= {reg};"),
        format!("split_gl;"),
        format!("val1 <=X= {addr};"),
        format!("val2 <=X= val3;"),
        format!("mstore {lo};"),
        format!("val2 <=X= val4;"),
        format!("mstore {hi};"),
    ]
}

/// Load word from addr+offset into register
fn load_word(addr_reg_id: u32, offset: u32, reg: &str) -> [String; 3] {
    [
        format!("val1 <== get_reg({addr_reg_id});"),
        format!("mload {offset};"),
        format!("{reg} <=X= val3;"),
    ]
}

/// Store word from register into addr+offset
fn store_word(addr_reg_id: u32, offset: u32, reg: &str) -> [String; 5] {
    [
        // split_gl ensures we store a 32-bit value
        format!("val1 <=X= {reg};"),
        format!("split_gl;"),
        format!("val1 <== get_reg({addr_reg_id});"),
        format!("val2 <=X= val3;"),
        format!("mstore {offset};"),
    ]
}
