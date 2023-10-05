#![deny(clippy::print_stdout)]

use ast::asm_analysis::AnalysisASMFile;
use number::FieldElement;
use romgen::generate_machine_rom;
mod common;
mod romgen;
mod vm_to_constrained;

/// Remove all ASM from the machine tree. Takes a tree of virtual or constrained machines and returns a tree of constrained machines
pub fn compile<T: FieldElement>(file: AnalysisASMFile<T>) -> AnalysisASMFile<T> {
    AnalysisASMFile {
        machines: file
            .machines
            .into_iter()
            .map(|(name, m)| {
                (name, {
                    let (m, rom) = generate_machine_rom(m);
                    vm_to_constrained::convert_machine(m, rom)
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
            asm::{AssignmentRegister, InstructionBody, MachineStatement, RegisterFlag},
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
            MachineStatement::InstructionDeclaration(start, name, instruction) => {
                InstructionDefinitionStatement {
                    start,
                    name,
                    instruction: Instruction {
                        params: instruction.params,
                        body: instruction.body,
                    },
                }
            }
            _ => panic!(),
        }
    }

    pub fn parse_instruction<T: FieldElement>(input: &str) -> Instruction<T> {
        let instr = parser::powdr::InstructionParser::new()
            .parse(input)
            .unwrap();
        Instruction {
            params: instr.params,
            body: instr.body,
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
            ast::parsed::asm::FunctionStatement::Assignment(start, lhs, reg, rhs) => {
                AssignmentStatement {
                    start,
                    lhs_with_reg: {
                        let lhs_len = lhs.len();
                        lhs.into_iter()
                            .zip(reg.unwrap_or(vec![AssignmentRegister::Wildcard; lhs_len]))
                            .collect()
                    },
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
