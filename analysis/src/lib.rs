mod block_enforcer;
mod macro_expansion;
mod type_check;
mod vm;

/// expose the macro expander for use in the pil_analyzer
pub use macro_expansion::MacroExpander;

use ast::{asm_analysis::AnalysisASMFile, parsed::asm::ASMFile};
use number::FieldElement;

#[derive(Default)]
/// A monitor of the changes applied to the program as we run through the analysis pipeline
pub struct DiffMonitor {
    previous: Option<String>,
    current: Option<String>,
}

impl DiffMonitor {
    /// push a new program and log::trace! how it differs from the previous one, if any
    fn push<S: ToString>(&mut self, s: S) {
        std::mem::swap(&mut self.previous, &mut self.current);
        self.current = Some(s.to_string());

        if let Some(current) = &self.current {
            if let Some(previous) = &self.previous {
                for diff in diff::lines(previous, current) {
                    match diff {
                        diff::Result::Left(l) => log::trace!("-{}", l),
                        diff::Result::Both(..) => {}
                        diff::Result::Right(r) => log::trace!("+{}", r),
                    }
                }
                log::trace!("");
            }
        }
    }
}

/// a pipeline of analysis steps, with the basic principles that each intermediate output should be as close as possible to a valid program as possible
/// for example, adding PIL constraints should only happen after the columns involved are declared
/// sometimes elements are introduced and mutated later: for example `return` is introduced first to reduce functions to static operations, but only instanciated as an instruction at ROM creation
/// we can try to reduce the occurence of such cases, for example by turning `return` into an embeded keyword rather than an instruction
pub fn analyze<T: FieldElement>(file: ASMFile<T>) -> Result<AnalysisASMFile<T>, Vec<String>> {
    let mut monitor = DiffMonitor::default();

    // expand macros
    log::debug!("Run expand analysis step");
    let file = macro_expansion::expand(file);
    // type check
    log::debug!("Run type-check analysis step");
    let file = type_check::check(file)?;
    monitor.push(&file);

    // run analysis on vm machines, reducing them to block machines
    log::debug!("Start asm analysis");
    let file = vm::analyze(file, &mut monitor)?;
    log::debug!("End asm analysis");

    // from now on we only have static machines!
    // enforce blocks using function_id and latch
    log::debug!("Run enforce_block analysis step");
    let file = block_enforcer::enforce(file);
    monitor.push(&file);

    Ok(file)
}

pub mod utils {
    use ast::{
        asm_analysis::{
            AssignmentStatement, FunctionStatement, InstructionDefinitionStatement,
            InstructionStatement, LabelStatement, RegisterDeclarationStatement,
        },
        parsed::{
            asm::{InstructionBody, MachineStatement},
            PilStatement,
        },
    };
    use number::FieldElement;

    pub fn parse_instruction_definition<T: FieldElement>(
        input: &str,
    ) -> InstructionDefinitionStatement<T> {
        match parser::powdr::InstructionDeclarationParser::new()
            .parse(input)
            .unwrap()
        {
            MachineStatement::InstructionDeclaration(start, name, params, body) => {
                InstructionDefinitionStatement {
                    start,
                    name,
                    params,
                    body,
                }
            }
            _ => panic!(),
        }
    }

    pub fn parse_instruction_body<T: FieldElement>(input: &str) -> InstructionBody<T> {
        parser::powdr::InstructionBodyParser::new()
            .parse(input)
            .unwrap()
    }

    pub fn parse_function_statement<T: FieldElement>(input: &str) -> FunctionStatement<T> {
        match parser::powdr::FunctionStatementParser::new()
            .parse::<T>(input)
            .unwrap()
        {
            ast::parsed::asm::FunctionStatement::Assignment(start, lhs, using_reg, rhs) => {
                AssignmentStatement {
                    start,
                    lhs,
                    using_reg,
                    rhs,
                }
                .into()
            }
            ast::parsed::asm::FunctionStatement::Instruction(start, instruction, inputs) => {
                InstructionStatement {
                    start,
                    instruction,
                    inputs,
                }
                .into()
            }
            ast::parsed::asm::FunctionStatement::Label(start, name) => {
                LabelStatement { start, name }.into()
            }
            ast::parsed::asm::FunctionStatement::DebugDirective(_start, _s) => {
                todo!()
            }
        }
    }

    pub fn parse_pil_statement<T: FieldElement>(input: &str) -> PilStatement<T> {
        parser::powdr::PilStatementParser::new()
            .parse(input)
            .unwrap()
    }

    pub fn parse_register_declaration<T: FieldElement>(
        input: &str,
    ) -> RegisterDeclarationStatement {
        match parser::powdr::RegisterDeclarationParser::new()
            .parse::<T>(input)
            .unwrap()
        {
            MachineStatement::RegisterDeclaration(start, name, flag) => {
                RegisterDeclarationStatement { start, name, flag }
            }
            _ => unreachable!(),
        }
    }
}

#[cfg(test)]
mod test_util {
    use ast::{asm_analysis::AnalysisASMFile, parsed::asm::ASMFile};
    use number::FieldElement;
    use parser::parse_asm;

    use crate::{macro_expansion, type_check};

    /// A test utility to process a source file until after macro expansion
    pub fn expand_str<T: FieldElement>(source: &str) -> ASMFile<T> {
        let file = parse_asm(None, source).unwrap();
        macro_expansion::expand(file)
    }

    /// A test utility to process a source file until after type checking
    pub fn typecheck_str<T: FieldElement>(source: &str) -> Result<AnalysisASMFile<T>, Vec<String>> {
        type_check::check(expand_str(source))
    }
}
