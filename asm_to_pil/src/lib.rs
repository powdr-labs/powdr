use ast::asm_analysis::AnalysisASMFile;
// use function_desugar::desugar_machine;
use number::FieldElement;
use romgen::generate_machine_rom;

mod common;
mod rom_to_fixed;
mod romgen;

pub fn compile<T: FieldElement>(file: AnalysisASMFile<T>) -> AnalysisASMFile<T> {
    AnalysisASMFile {
        machines: file
            .machines
            .into_iter()
            .map(|(name, m)| {
                (name, {
                    let (m, rom) = generate_machine_rom(m);
                    rom_to_fixed::convert_machine(m, rom)
                })
            })
            .collect(),
    }
}

pub mod utils {
    use ast::{
        asm_analysis::{
            AssignmentStatement, FunctionStatement, Instruction, InstructionDefinitionStatement,
            InstructionStatement, LabelStatement, RegisterDeclarationStatement, RegisterTy,
        },
        parsed::{
            asm::{InstructionBody, MachineStatement, RegisterFlag},
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
                    instruction: Instruction { params, body },
                }
            }
            _ => panic!(),
        }
    }

    pub fn parse_instruction<T: FieldElement>(input: &str) -> Instruction<T> {
        match parser::powdr::InstructionDeclarationParser::new()
            .parse(&format!("instr dummy {input}"))
            .unwrap()
        {
            MachineStatement::InstructionDeclaration(.., params, body) => {
                Instruction { params, body }
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
            _ => unimplemented!(),
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
                let ty = match flag {
                    Some(RegisterFlag::IsAssignment) => RegisterTy::Assignment,
                    Some(RegisterFlag::IsPC) => RegisterTy::Pc,
                    Some(RegisterFlag::IsReadOnly) => RegisterTy::ReadOnly,
                    None => RegisterTy::Write,
                };
                RegisterDeclarationStatement { start, name, ty }
            }
            _ => unreachable!(),
        }
    }
}
