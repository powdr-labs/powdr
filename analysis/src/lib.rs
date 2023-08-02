mod batcher;
mod block_enforcer;
mod function_id_setter;
mod inference;
mod macro_expansion;
mod romgen;
mod type_check;

/// expose the macro expander for use in the pil_analyzer
pub use macro_expansion::MacroExpander;

use ast::{asm_analysis::AnalysisASMFile, parsed::asm::ASMFile};
use number::FieldElement;

#[derive(Default)]
struct DiffMonitor {
    previous: Option<String>,
    current: Option<String>,
}

impl DiffMonitor {
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
            }
        }
    }
}

pub fn analyze<T: FieldElement>(file: ASMFile<T>) -> Result<AnalysisASMFile<T>, Vec<String>> {
    let mut monitor = DiffMonitor::default();

    // expand macros
    log::trace!("Run expand analysis step");
    let file = macro_expansion::expand(file);
    // type check
    log::trace!("Run type-check analysis step");
    let file = type_check::check(file)?;
    monitor.push(&file);
    // infer assignment registers (vm)
    log::trace!("Run inference analysis step");
    let file = inference::infer(file)?;
    monitor.push(&file);
    // generate the rom using a dispatcher (vm)
    log::trace!("Run generate_rom analysis step");
    let file = romgen::generate_rom(file);
    monitor.push(&file);
    // batch statements (vm)
    log::trace!("Run batch analysis step");
    let file = batcher::batch(file);
    monitor.push(&file);
    // generate the function ids from the rom (vm)
    log::trace!("Run function_id_set analysis step");
    let file = function_id_setter::set(file);
    monitor.push(&file);
    // turn asm into pil
    log::trace!("Run asm_to_pil analysis step");
    let file = asm_to_pil::compile(file);
    monitor.push(&file);
    // from now on we only have static machines!
    // enforce blocks using function_id and latch
    log::trace!("Run enforce_block analysis step");
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
        parsed::{asm::MachineStatement, PilStatement},
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

    pub fn parse_operation_statement<T: FieldElement>(input: &str) -> FunctionStatement<T> {
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

    use crate::{inference, macro_expansion, type_check};

    /// A test utility to process a source file until after macro expansion
    pub fn expand_str<T: FieldElement>(source: &str) -> ASMFile<T> {
        let file = parse_asm(None, source).unwrap();
        macro_expansion::expand(file)
    }

    /// A test utility to process a source file until after type checking
    pub fn typecheck_str<T: FieldElement>(source: &str) -> Result<AnalysisASMFile<T>, Vec<String>> {
        type_check::check(expand_str(source))
    }

    /// A test utility to process a source file until after inference
    pub fn infer_str<T: FieldElement>(source: &str) -> Result<AnalysisASMFile<T>, Vec<String>> {
        inference::infer(typecheck_str(source).unwrap())
    }
}
