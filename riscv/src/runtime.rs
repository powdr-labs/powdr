use powdr_ast::parsed::asm::{FunctionStatement, MachineStatement, SymbolPath};

use powdr_parser::ParserContext;
use powdr_riscv_syscalls::Syscall;

pub static EXTRA_REG_PREFIX: &str = "xtra";

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

#[derive(Clone)]
pub struct SubMachine {
    /// Full path to machine (e.g, `path::to::Machine`)
    pub path: SymbolPath,
    /// Optional alias (`use path::to::Machine as TheAlias;`)
    pub alias: Option<String>,
    /// Instance declaration name,
    pub instance_name: String,
    /// Arguments
    pub arguments: Vec<String>,
    /// Instruction declarations
    pub instructions: Vec<MachineStatement>,
    /// Number of registers needed by this machine's instruction declarations if > 4.
    pub extra_registers: u8,
    /// TODO: only needed because of witgen requiring that each machine be called at least once
    pub init_call: Vec<FunctionStatement>,
}

impl SubMachine {
    pub fn import(&self) -> String {
        format!(
            "use {}{}{};",
            self.path,
            self.alias.as_deref().map(|_| " as ").unwrap_or_default(),
            self.alias.as_deref().unwrap_or_default()
        )
    }

    pub fn declaration(&self) -> String {
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
#[derive(Clone)]
pub struct SyscallImpl(pub Vec<FunctionStatement>);

use crate::runtime_16::Runtime16;
use crate::runtime_32::Runtime32;

#[derive(Clone)]
pub enum RuntimeEnum {
    Runtime16(Runtime16),
    Runtime32(Runtime32),
}

impl RuntimeEnum {
    pub fn base_16() -> Self {
        RuntimeEnum::Runtime16(Runtime16::base())
    }
    pub fn base_32() -> Self {
        RuntimeEnum::Runtime32(Runtime32::base())
    }
    pub fn as_runtime16(&self) -> &Runtime16 {
        match self {
            RuntimeEnum::Runtime16(r) => r,
            _ => panic!("expected Runtime16"),
        }
    }
    pub fn as_runtime32(&self) -> &Runtime32 {
        match self {
            RuntimeEnum::Runtime32(r) => r,
            _ => panic!("expected Runtime32"),
        }
    }
}

impl Runtime for RuntimeEnum {
    fn has_submachine(&self, name: &str) -> bool {
        match self {
            RuntimeEnum::Runtime16(r) => r.has_submachine(name),
            RuntimeEnum::Runtime32(r) => r.has_submachine(name),
        }
    }

    fn has_syscall(&self, s: Syscall) -> bool {
        match self {
            RuntimeEnum::Runtime16(r) => r.has_syscall(s),
            RuntimeEnum::Runtime32(r) => r.has_syscall(s),
        }
    }

    fn with_poseidon_no_continuations(self) -> Self {
        match self {
            RuntimeEnum::Runtime16(r) => RuntimeEnum::Runtime16(r.with_poseidon_no_continuations()),
            RuntimeEnum::Runtime32(r) => RuntimeEnum::Runtime32(r.with_poseidon_no_continuations()),
        }
    }

    fn with_poseidon_for_continuations(self) -> Self {
        match self {
            RuntimeEnum::Runtime16(r) => {
                RuntimeEnum::Runtime16(r.with_poseidon_for_continuations())
            }
            RuntimeEnum::Runtime32(r) => {
                RuntimeEnum::Runtime32(r.with_poseidon_for_continuations())
            }
        }
    }

    fn with_arith(self) -> Self {
        match self {
            RuntimeEnum::Runtime16(r) => RuntimeEnum::Runtime16(r.with_arith()),
            RuntimeEnum::Runtime32(r) => RuntimeEnum::Runtime32(r.with_arith()),
        }
    }

    fn submachines_init(&self) -> Vec<String> {
        match self {
            RuntimeEnum::Runtime16(r) => r.submachines_init(),
            RuntimeEnum::Runtime32(r) => r.submachines_init(),
        }
    }
    fn submachines_import(&self) -> String {
        match self {
            RuntimeEnum::Runtime16(r) => r.submachines_import(),
            RuntimeEnum::Runtime32(r) => r.submachines_import(),
        }
    }
    fn submachines_declare(&self) -> String {
        match self {
            RuntimeEnum::Runtime16(r) => r.submachines_declare(),
            RuntimeEnum::Runtime32(r) => r.submachines_declare(),
        }
    }
    fn submachines_instructions(&self) -> Vec<String> {
        match self {
            RuntimeEnum::Runtime16(r) => r.submachines_instructions(),
            RuntimeEnum::Runtime32(r) => r.submachines_instructions(),
        }
    }
    fn submachines_extra_registers(&self) -> Vec<String> {
        match self {
            RuntimeEnum::Runtime16(r) => r.submachines_extra_registers(),
            RuntimeEnum::Runtime32(r) => r.submachines_extra_registers(),
        }
    }
    fn ecall_handler(&self) -> Vec<String> {
        match self {
            RuntimeEnum::Runtime16(r) => r.ecall_handler(),
            RuntimeEnum::Runtime32(r) => r.ecall_handler(),
        }
    }
    fn submachine_names(&self) -> String {
        match self {
            RuntimeEnum::Runtime16(r) => r.submachine_names(),
            RuntimeEnum::Runtime32(r) => r.submachine_names(),
        }
    }
}

pub trait Runtime {
    fn has_submachine(&self, name: &str) -> bool;
    fn has_syscall(&self, s: Syscall) -> bool;
    fn with_poseidon_no_continuations(self) -> Self;
    fn with_poseidon_for_continuations(self) -> Self;
    fn with_arith(self) -> Self;
    fn submachines_init(&self) -> Vec<String>;
    fn submachines_import(&self) -> String;
    fn submachines_declare(&self) -> String;
    fn submachines_instructions(&self) -> Vec<String>;
    fn submachines_extra_registers(&self) -> Vec<String>;
    fn ecall_handler(&self) -> Vec<String>;
    fn submachine_names(&self) -> String;
}
