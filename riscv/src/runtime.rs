use std::{collections::BTreeMap, convert::TryFrom};

use powdr_riscv_syscalls::{Syscall, SYSCALL_REGISTERS};

use powdr_ast::parsed::asm::{FunctionStatement, MachineStatement, SymbolPath};

use itertools::Itertools;
use powdr_parser::ParserContext;

use crate::compiler::{pop_register, push_register};

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
    /// Number of extra registers needed by this machine's instruction declarations.
    /// 26 of the RISC-V registers are available for use, these are added to that number.
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
                "instr and Y, Z -> X ~ binary.and;",
                "instr or Y, Z -> X ~ binary.or;",
                "instr xor Y, Z -> X ~ binary.xor;",
            ],
            0,
            ["x10 <== and(x10, x10);"],
        );

        r.add_submachine(
            "std::machines::shift::Shift",
            None,
            "shift",
            [
                "instr shl Y, Z -> X ~ shift.shl;",
                "instr shr Y, Z -> X ~ shift.shr;",
            ],
            0,
            ["x10 <== shl(x10, x10);"],
        );

        r.add_submachine(
            "std::machines::split::split_gl::SplitGL",
            None,
            "split_gl",
            ["instr split_gl Z -> X, Y ~ split_gl.split;"],
            0,
            ["x10, x11 <== split_gl(x10);", "x10 <=X= 0;", "x11 <=X= 0;"],
        );

        // Base syscalls
        r.add_syscall(
            Syscall::Input,
            ["x10 <=X= ${ std::prover::Query::Input(std::convert::int(std::prover::eval(x10))) };"],
        );

        r.add_syscall(
            Syscall::DataIdentifier,
            ["x10 <=X= ${ std::prover::Query::DataIdentifier(std::convert::int(std::prover::eval(x11)), std::convert::int(std::prover::eval(x10))) };"]
        );

        r.add_syscall(
            Syscall::Output,
            // This is using x0 on purpose, because we do not want to introduce
            // nondeterminism with this.
            ["x0 <=X= ${ std::prover::Query::Output(std::convert::int(std::prover::eval(x10)), std::convert::int(std::prover::eval(x11))) };"]
        );

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
            [format!(
                "instr keccakf {} ~ keccakf.keccakf {}, {}, STEP ->;", // instr keccakf X, Y -> ~ keccakf X, Y, STEP ->; (sub calls asm)
                instr_register_params(0, 2, 0), // one pointer for input and one pointer for output (both as input arguments)
                reg(0),
                reg(1),
            )],
            0,
            std::iter::once(format!("{} <=X= 0x100;", reg(0))) // filler value for input pointer
                .chain(std::iter::once(format!("{} <=X= 0x300;", reg(1)))) // filler value for output pointer (at least 200 bytes away)
                .chain(std::iter::once("keccakf;".to_string())) // must be called at least once
                .chain((0..50).flat_map(|i| store_word(&reg(1), i as u32 * 4, "x0"))), // zero out 200 bytes following output pointer
        );

        // The keccakf syscall has a two arguments passed on x10 and x11,
        // the memory address of the 25 field element input array
        // and the memory address of the 25 field element output array to store results to.
        let implementation = std::iter::once("keccakf;".to_string());

        self.add_syscall(Syscall::KeccakF, implementation);
        self
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
            0,
            // init call
            std::iter::once("poseidon_gl;".to_string())
                // zero out output registers
                .chain((0..4).map(|i| format!("{} <=X= 0;", reg(i)))),
        );

        // The poseidon syscall has a single argument passed on x10, the
        // memory address of the 12 field element input array. Since the memory
        // offset is chosen by LLVM, we assume it's properly aligned.
        let implementation =
            // The poseidon syscall uses x10 for input, we store it in tmp3 and
            // reuse x10 as input to the poseidon machine instruction.
            std::iter::once("tmp3 <=X= x10;".to_string())
            // The poseidon instruction uses registers 0..12 as input/output.
            // The memory field elements are loaded into these registers before calling the instruction.
            // They might be in use by the riscv machine, so we save the registers on the stack.
            .chain((0..12).flat_map(|i| push_register(&reg(i))))
            .chain((0..12).flat_map(|i| load_gl_fe("tmp3", i as u32 * 8, &reg(i))))
            .chain(std::iter::once("poseidon_gl;".to_string()))
            .chain((0..4).flat_map(|i| store_gl_fe("tmp3", i as u32 * 8, &reg(i))))
            // After copying the result back into memory, we restore the original register values.
            .chain(
                (0..12)
                    .rev()
                    .flat_map(|i| pop_register(SYSCALL_REGISTERS[i])),
            );

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
                    instr_register_params(3, 24, 16) // will use registers 3..27
                ),
                format!(
                    "instr ec_add ~ arith.ec_add {};",
                    instr_register_params(4, 32, 16) // will use registers 4..36
                ),
                format!(
                    "instr ec_double ~ arith.ec_double {};",
                    instr_register_params(2, 16, 16) // will use registers 2..18
                ),
                format!(
                    "instr mod_256 ~ arith.mod_256 {};",
                    instr_register_params(3, 24, 8) // will use registers 3..27
                ),
            ],
            // machine uses the 26 registers from risc-v plus 10 extra registers
            10,
            // calling ec_double for machine initialization.
            // store x in registers 2..10
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
            .map(|(i, fe)| format!("{} <=X= {fe};", reg(i + 2)))
            // store y in registers 10..18
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
                .map(|(i, fe)| format!("{} <=X= {fe};", reg(i + 10))),
            )
            // call machine instruction
            .chain(std::iter::once("ec_double;".to_string()))
            // set output registers to zero
            .chain((2..18).map(|i| format!("{} <=X= 0;", reg(i)))),
        );

        // TODO: we're also saving the "extra registers", but those don't have to be saved

        // The affine_256 syscall takes as input the addresses of x1, y1 and x2.
        let affine256 =
            // Save instruction registers
            (3..27).flat_map(|i| push_register(&reg(i)))
            // Load x1 in 3..11
            .chain((0..8).flat_map(|i| load_word(&reg(0), i as u32 *4 , &reg(i + 3))))
            // Load y1 in 11..19
            .chain((0..8).flat_map(|i| load_word(&reg(1), i as u32 *4 , &reg(i + 11))))
            // Load x2 in 19..27
            .chain((0..8).flat_map(|i| load_word(&reg(2), i as u32 *4 , &reg(i + 19))))
            // Call instruction
            .chain(std::iter::once("affine_256;".to_string()))
            // Store result y2 in x1's memory
            .chain((0..8).flat_map(|i| store_word(&reg(0), i as u32 *4 , &reg(i + 3))))
            // Store result y3 in y1's memory
            .chain((0..8).flat_map(|i| store_word(&reg(1), i as u32 *4 , &reg(i + 11))))
            // Restore instruction registers
            .chain(
                (3..27)
                    .rev()
                    .flat_map(|i| pop_register(&reg(i))));
        self.add_syscall(Syscall::Affine256, affine256);

        // The mod_256 syscall takes as input the addresses of y2, y3, and x1.
        let mod256 =
            // Save instruction registers
            (3..27).flat_map(|i| push_register(&reg(i)))
            // Load y2 in 3..11
            .chain((0..8).flat_map(|i| load_word(&reg(0), i as u32 *4 , &reg(i + 3))))
            // Load y3 in 11..19
            .chain((0..8).flat_map(|i| load_word(&reg(1), i as u32 *4 , &reg(i + 11))))
            // Load x1 in 19..27
            .chain((0..8).flat_map(|i| load_word(&reg(2), i as u32 *4 , &reg(i + 19))))
            // Call instruction
            .chain(std::iter::once("mod_256;".to_string()))
            // Store result x2 in y2's memory
            .chain((0..8).flat_map(|i| store_word(&reg(0), i as u32 *4 , &reg(i + 3))))
            // Restore instruction registers
            .chain(
                (3..27)
                    .rev()
                    .flat_map(|i| pop_register(&reg(i))));
        self.add_syscall(Syscall::Mod256, mod256);

        // The ec_add syscall takes as input the four addresses of x1, y1, x2, y2.
        let ec_add =
            // Save instruction registers.
            (4..36).flat_map(|i| push_register(&reg(i)))
            // Load x1 in 4..12
            .chain((0..8).flat_map(|i| load_word(&reg(0), i as u32 * 4, &reg(i + 4))))
            // Load y1 in 12..20
            .chain((0..8).flat_map(|i| load_word(&reg(1), i as u32 * 4, &reg(i + 12))))
            // Load x2 in 20..28
            .chain((0..8).flat_map(|i| load_word(&reg(2), i as u32 * 4, &reg(i + 20))))
            // Load y2 in 28..36
            .chain((0..8).flat_map(|i| load_word(&reg(3), i as u32 * 4, &reg(i + 28))))
            // Call instruction
            .chain(std::iter::once("ec_add;".to_string()))
            // Save result x3 in x1
            .chain((0..8).flat_map(|i| store_word(&reg(0), i as u32 * 4, &reg(i + 4))))
            // Save result y3 in y1
            .chain((0..8).flat_map(|i| store_word(&reg(1), i as u32 * 4, &reg(i + 12))))
            // Restore instruction registers.
            .chain(
                (4..36)
                    .rev()
                    .flat_map(|i| pop_register(&reg(i))));
        self.add_syscall(Syscall::EcAdd, ec_add);

        // The ec_double syscall takes as input the addresses of x and y in x10 and x11 respectively.
        // We load x and y from memory into registers 2..10 and registers 10..18 respectively.
        // We then store the result from those registers into the same addresses (x10 and x11).
        let ec_double =
            // Save instruction registers.
            (2..18).flat_map(|i| push_register(&reg(i)))
            // Load x in 2..10
            .chain((0..8).flat_map(|i| load_word(&reg(0), i as u32 * 4, &reg(i + 2))))
            // Load y in 10..18
            .chain((0..8).flat_map(|i| load_word(&reg(1), i as u32 * 4, &reg(i + 10))))
            // Call instruction
            .chain(std::iter::once("ec_double;".to_string()))
            // Store result in x
            .chain((0..8).flat_map(|i| store_word(&reg(0), i as u32 * 4, &reg(i + 2))))
            // Store result in y
            .chain((0..8).flat_map(|i| store_word(&reg(1), i as u32 * 4, &reg(i + 10))))
            // Restore instruction registers.
            .chain(
                (2..18)
                    .rev()
                    .flat_map(|i| pop_register(&reg(i))));

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
        ]
        .into_iter();

        let jump_table = self
            .syscalls
            .keys()
            .map(|s| format!("branch_if_zero x5 - {}, __ecall_handler_{};", *s as u32, s));

        let invalid_handler = ["__invalid_syscall:".to_string(), "fail;".to_string()].into_iter();

        let handlers = self.syscalls.iter().flat_map(|(syscall, implementation)| {
            std::iter::once(format!("__ecall_handler_{syscall}:"))
                .chain(implementation.0.iter().map(|i| i.to_string()))
                .chain(std::iter::once("tmp1 <== jump_dyn(x1);".to_string()))
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
                "keccakf" => runtime = runtime.with_keccak(),
                "arith" => runtime = runtime.with_arith(),
                _ => return Err(format!("Invalid co-processor specified: {name}")),
            }
        }
        Ok(runtime)
    }
}

/// Helper function for register names used in instruction params
fn reg(mut idx: usize) -> String {
    // s0..11 callee saved registers
    static SAVED_REGS: [&str; 12] = [
        "x8", "x9", "x18", "x19", "x20", "x21", "x22", "x23", "x24", "x25", "x26", "x27",
    ];

    // first, use syscall_registers
    if idx < SYSCALL_REGISTERS.len() {
        return SYSCALL_REGISTERS[idx].to_string();
    }
    idx -= SYSCALL_REGISTERS.len();
    // second, callee saved registers
    if idx < SAVED_REGS.len() {
        return SAVED_REGS[idx].to_string();
    }
    idx -= SAVED_REGS.len();
    // lastly, use extra submachine registers
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
fn load_gl_fe(addr: &str, offset: u32, reg: &str) -> [String; 3] {
    let lo = offset;
    let hi = offset + 4;
    [
        format!("{reg}, tmp2 <== mload({lo} + {addr});"),
        format!("tmp1, tmp2 <== mload({hi} + {addr});"),
        format!("{reg} <=X= {reg} + tmp1 * 2**32;"),
    ]
}

/// Store gl field element from register into addr+offset
fn store_gl_fe(addr: &str, offset: u32, reg: &str) -> [String; 3] {
    let lo = offset;
    let hi = offset + 4;
    [
        format!("tmp1, tmp2 <== split_gl({reg});"),
        format!("mstore {lo} + {addr}, tmp1;"),
        format!("mstore {hi} + {addr}, tmp2;"),
    ]
}

/// Load word from addr+offset into register
fn load_word(addr: &str, offset: u32, reg: &str) -> [String; 1] {
    [format!("{reg}, tmp2 <== mload({offset} + {addr});")]
}

/// Store word from register into addr+offset
fn store_word(addr: &str, offset: u32, reg: &str) -> [String; 2] {
    [
        // split_gl ensures we store a 32-bit value
        format!("tmp1, tmp2 <== split_gl({reg});"),
        format!("mstore {offset} + {addr}, tmp1;"),
    ]
}
