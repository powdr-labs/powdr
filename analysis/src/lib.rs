mod batcher;
mod macro_expansion;
mod romgen;
mod type_check;

/// expose the macro expander for use in the pil_analyzer
pub use macro_expansion::MacroExpander;

use ast::{asm_analysis::AnalysisASMFile, parsed::asm::ASMFile};
use number::FieldElement;

pub fn analyze<T: FieldElement>(file: ASMFile<T>) -> Result<AnalysisASMFile<T>, Vec<String>> {
    let expanded = macro_expansion::expand(file);
    let checked = type_check::check(expanded)?;
    let rommed = romgen::generate_rom(checked);
    let batched = batcher::batch(rommed);
    Ok(batched)
}

mod utils {
    use ast::{
        asm_analysis::{
            AssignmentStatement, InstructionDefinitionStatement, InstructionStatement,
            LabelStatement, OperationStatement, RegisterDeclarationStatement,
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

    pub fn parse_operation_statement<T: FieldElement>(input: &str) -> OperationStatement<T> {
        match parser::powdr::OperationStatementParser::new()
            .parse::<T>(input)
            .unwrap()
        {
            ast::parsed::asm::OperationStatement::Assignment(start, lhs, using_reg, rhs) => {
                AssignmentStatement {
                    start,
                    lhs,
                    using_reg,
                    rhs,
                }
                .into()
            }
            ast::parsed::asm::OperationStatement::Instruction(start, instruction, inputs) => {
                InstructionStatement {
                    start,
                    instruction,
                    inputs,
                }
                .into()
            }
            ast::parsed::asm::OperationStatement::Label(start, name) => {
                LabelStatement { start, name }.into()
            }
            ast::parsed::asm::OperationStatement::DebugDirective(_start, _s) => {
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
