use powdr_ast::parsed::asm::{FunctionStatement, MachineStatement, SymbolPath};

use powdr_parser::ParserContext;
use powdr_riscv_syscalls::Syscall;

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
///
/// Any of the registers used as input/output to the syscall should be usable without issue.
/// Other registers should be saved/restored from memory, as LLVM doesn't know about their usage here.
#[derive(Clone)]
pub struct SyscallImpl {
    pub syscall: Syscall,
    pub statements: Vec<String>,
}
