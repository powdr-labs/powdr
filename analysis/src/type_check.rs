use ast::{
    analysis::{
        AnalysisASMFile, AssignmentStatement, DegreeStatement, InstructionDefinitionStatement,
        InstructionStatement, LabelStatement, PilBlock, RegisterDeclarationStatement,
    },
    parsed::asm::{ASMFile, ASMStatement},
};
use number::FieldElement;

/// A very stupid type checker. TODO: make it smart
pub fn check<T: FieldElement>(file: ASMFile<T>) -> Result<AnalysisASMFile<T>, String> {
    let mut degree = None;
    let mut registers = vec![];
    let mut pil = vec![];
    let mut instructions = vec![];
    let mut program = vec![];

    for s in file.0 {
        match s {
            ASMStatement::Degree(_, degree_value) => {
                degree = Some(DegreeStatement {
                    degree: degree_value,
                });
            }
            ASMStatement::RegisterDeclaration(start, name, flag) => {
                registers.push(RegisterDeclarationStatement { start, name, flag });
            }
            ASMStatement::InstructionDeclaration(start, name, params, body) => {
                instructions.push(InstructionDefinitionStatement {
                    start,
                    name,
                    params,
                    body,
                });
            }
            ASMStatement::InlinePil(start, statements) => {
                pil.push(PilBlock { start, statements });
            }
            ASMStatement::Assignment(start, lhs, using_reg, rhs) => {
                program.push(
                    AssignmentStatement {
                        start,
                        lhs,
                        using_reg,
                        rhs,
                    }
                    .into(),
                );
            }
            ASMStatement::Instruction(start, instruction, inputs) => {
                program.push(
                    InstructionStatement {
                        start,
                        instruction,
                        inputs,
                    }
                    .into(),
                );
            }
            ASMStatement::Label(start, name) => {
                program.push(LabelStatement { start, name }.into());
            }
        }
    }

    Ok(AnalysisASMFile {
        degree,
        registers,
        pil,
        instructions,
        program,
        batches: None,
    })
}
