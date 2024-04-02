use std::{collections::BTreeMap, convert::TryFrom};

use powdr_riscv_syscalls::{Syscall, SYSCALL_REGISTERS};

use powdr_ast::parsed::asm::{FunctionStatement, MachineStatement, SymbolPath};

use itertools::Itertools;
use powdr_parser::ParserContext;

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
    /// Declaration name,
    name: String,
    /// Instruction declarations
    instructions: Vec<MachineStatement>,
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
        format!("{} {};", ty, self.name)
    }
}

/// Sequence of asm function statements.
/// the implementation can use registers in `SYSCALL_REGISTERS`
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
            "std::binary::Binary",
            None,
            "binary",
            [
                "instr and Y, Z -> X ~ binary.and;",
                "instr or Y, Z -> X ~ binary.or;",
                "instr xor Y, Z -> X ~ binary.xor;",
            ],
            ["x10 <== and(x10, x10);"],
        );

        r.add_submachine(
            "std::shift::Shift",
            None,
            "shift",
            [
                "instr shl Y, Z -> X ~ shift.shl;",
                "instr shr Y, Z -> X ~ shift.shr;",
            ],
            ["x10 <== shl(x10, x10);"],
        );

        r.add_submachine(
            "std::split::split_gl::SplitGL",
            None,
            "split_gl",
            ["instr split_gl Z -> X, Y ~ split_gl.split;"],
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
            Syscall::PrintChar,
            // This is using x0 on purpose, because we do not want to introduce
            // nondeterminism with this.
            ["x0 <=X= ${ std::prover::Query::PrintChar(std::convert::int(std::prover::eval(x10))) };"]
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
        self.add_submachine("std::hash::poseidon_gl::PoseidonGL",
                            None,
                            "poseidon_gl",
                            ["instr poseidon_gl ~ poseidon_gl.poseidon_permutation x10, x11, x12, x13, x14, x15, x16, x17, x6, x7, x28, x29 -> x10', x11', x12', x13';"],
                            // init call
                            ["poseidon_gl;",
                             "x10 <=X= 0;",
                             "x11 <=X= 0;",
                             "x12 <=X= 0;",
                             "x13 <=X= 0;",
                            ]);

        // The poseidon syscall has a single argument passed on x10, the
        // memory address of the 12 field element input array. Since the memory
        // offset is chosen by LLVM, we assume it is properly aligned.

        // The poseidon syscall uses x10 for input, we store it in tmp3, as x10 is
        // also used as input to the poseidon machine instruction.
        let setup = std::iter::once("tmp3 <=X= x10;".to_string());

        // The poseidon instruction uses the first 12 SYSCALL_REGISTERS as input/output.
        // The contents of memory are loaded into these registers before calling the instruction.
        // These might be in use by the riscv machine, so we save the registers on the stack.
        let save_register = |i| {
            let reg = SYSCALL_REGISTERS[i];
            [
                // save register in stack
                "x2 <=X= wrap(x2 - 4);".to_string(),
                format!("mstore x2, {reg};"),
            ]
        };

        // After copying the result back into memory, we restore the original register values.
        let restore_register = |i| {
            let reg = SYSCALL_REGISTERS[i];
            [
                // restore register from stack
                format!("{reg}, tmp1 <== mload(x2);"),
                "x2 <=X= wrap(x2 + 4);".to_string(),
            ]
        };

        // load input from memory into register
        let load_word = |i| {
            let reg = SYSCALL_REGISTERS[i];
            let lo = i * 8;
            let hi = i * 8 + 4;
            [
                format!("{reg}, tmp2 <== mload({lo} + tmp3);"),
                format!("tmp1, tmp2 <== mload({hi} + tmp3);"),
                format!("{reg} <=X= {reg} + tmp1 * 2**32;"),
            ]
        };

        // copy output from register into memory
        let store_word = |i| {
            let reg = SYSCALL_REGISTERS[i];
            let lo = i * 8;
            let hi = i * 8 + 4;
            [
                format!("tmp1, tmp2 <== split_gl({reg});"),
                format!("mstore {lo} + tmp3, tmp1;"),
                format!("mstore {hi} + tmp3, tmp2;"),
            ]
        };

        let implementation = setup
            .chain((0..12).flat_map(save_register))
            .chain((0..12).flat_map(load_word))
            .chain(std::iter::once("poseidon_gl;".to_string()))
            .chain((0..4).flat_map(store_word))
            .chain((0..12).rev().flat_map(restore_register));

        self.add_syscall(Syscall::PoseidonGL, implementation);
        self
    }

    pub fn add_submachine<S: AsRef<str>, I1: IntoIterator<Item = S>, I2: IntoIterator<Item = S>>(
        &mut self,
        path: &str,
        alias: Option<&str>,
        name: &str,
        instructions: I1,
        init_call: I2,
    ) {
        let subm = SubMachine {
            path: str::parse(path).expect("invalid submachine path"),
            alias: alias.map(|s| s.to_string()),
            name: name.to_string(),
            instructions: instructions
                .into_iter()
                .map(|s| parse_instruction_declaration(s.as_ref()))
                .collect(),
            init_call: init_call
                .into_iter()
                .map(|s| parse_function_statement(s.as_ref()))
                .collect(),
        };
        assert!(
            self.submachines.insert(name.to_string(), subm).is_none(),
            "submachine {name} already present"
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
            panic!("duplicate syscall {}", syscall);
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
            std::iter::once(format!("__ecall_handler_{}:", syscall))
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
                _ => return Err(format!("Invalid co-processor specified: {name}")),
            }
        }
        Ok(runtime)
    }
}
