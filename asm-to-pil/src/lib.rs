use std::collections::BTreeMap;

use powdr_ast::{
    asm_analysis::{AnalysisASMFile, Module, StatementReference, SubmachineDeclaration},
    parsed::asm::{parse_absolute_path, SymbolPath},
};
use powdr_number::FieldElement;
use romgen::generate_machine_rom;
use vm_to_constrained::ROM_SUBMACHINE_NAME;
mod common;
mod romgen;
mod vm_to_constrained;

pub const ROM_SUFFIX: &str = "ROM";
const MAIN_MACHINE: &str = "::Main";

/// Remove all ASM from the machine tree, leaving only constrained machines
pub fn compile<T: FieldElement>(mut file: AnalysisASMFile) -> AnalysisASMFile {
    let main_machine_path = parse_absolute_path(MAIN_MACHINE);

    for (path, module) in &mut file.modules {
        let mut new_machines = BTreeMap::default();
        let (mut machines, statements, ordering) = std::mem::take(module).into_inner();
        let ordering = ordering
            .into_iter()
            .flat_map(|r| {
                match r {
                    StatementReference::MachineDeclaration(name) => {
                        let m = machines.remove(&name).unwrap();
                        let machine_path =
                            path.clone().join(SymbolPath::from_identifier(name.clone()));
                        // A machine is callable if it is not the main machine
                        // This is used to avoid generating a dispatcher for the main machine, since it's only called once, from the outside, and doesn't return
                        let is_callable = machine_path != main_machine_path;
                        let (m, rom) = generate_machine_rom::<T>(m, is_callable);
                        let (mut m, rom_machine) = vm_to_constrained::convert_machine::<T>(m, rom);

                        match rom_machine {
                            // in the absence of ROM, simply return the machine
                            None => {
                                new_machines.insert(name.clone(), m);
                                vec![name]
                            }
                            Some(rom_machine) => {
                                // introduce a new name for the ROM machine, based on the original name
                                let rom_name = format!("{name}{ROM_SUFFIX}");
                                let mut ty = path.clone();
                                ty.push(rom_name.clone());

                                // add the ROM as a submachine
                                m.submachines.push(SubmachineDeclaration {
                                    name: ROM_SUBMACHINE_NAME.into(),
                                    ty,
                                    args: vec![],
                                });

                                new_machines.insert(name.clone(), m);
                                new_machines.insert(rom_name.clone(), rom_machine);

                                // return both the machine and the rom
                                vec![name, rom_name]
                            }
                        }
                        .into_iter()
                        .map(StatementReference::MachineDeclaration)
                        .collect()
                    }
                    r => vec![r],
                }
            })
            .collect();
        machines.extend(new_machines);
        *module = Module::new(machines, statements, ordering);
    }
    file
}

pub mod utils {
    use powdr_ast::{
        asm_analysis::{
            AssignmentStatement, FunctionStatement, InstructionDefinitionStatement,
            InstructionStatement, LabelStatement, RegisterDeclarationStatement, RegisterTy,
        },
        parsed::{
            asm::{
                AssignmentRegister, Instruction, InstructionBody, MachineStatement, RegisterFlag,
            },
            PilStatement,
        },
    };
    use powdr_number::FieldElement;
    use powdr_parser::{
        powdr::{
            FunctionStatementParser, InstructionBodyParser, InstructionDeclarationParser,
            InstructionParser, PilStatementParser, RegisterDeclarationParser,
        },
        ParserContext,
    };

    lazy_static::lazy_static! {
        static ref INSTRUCTION_DECLARATION_PARSER: InstructionDeclarationParser = InstructionDeclarationParser::new();
        static ref INSTRUCTION_PARSER: InstructionParser = InstructionParser::new();
        static ref INSTRUCTION_BODY_PARSER: InstructionBodyParser = InstructionBodyParser::new();
        static ref FUNCTION_STATEMENT_PARSER: FunctionStatementParser = FunctionStatementParser::new();
        static ref PIL_STATEMENT_PARSER: PilStatementParser = PilStatementParser::new();
        static ref REGISTER_DECLARATION_PARSER: RegisterDeclarationParser = RegisterDeclarationParser::new();

    }

    pub fn parse_instruction_definition(input: &str) -> InstructionDefinitionStatement {
        let ctx = ParserContext::new(None, input);
        match INSTRUCTION_DECLARATION_PARSER.parse(&ctx, input).unwrap() {
            MachineStatement::InstructionDeclaration(source, name, instruction) => {
                InstructionDefinitionStatement {
                    source,
                    name,
                    instruction: Instruction {
                        params: instruction.params,
                        body: instruction.body,
                        links: instruction.links,
                    },
                }
            }
            _ => panic!(),
        }
    }

    pub fn parse_instruction(input: &str) -> Instruction {
        let ctx = ParserContext::new(None, input);
        let instr = INSTRUCTION_PARSER.parse(&ctx, input).unwrap();
        Instruction {
            params: instr.params,
            body: instr.body,
            links: instr.links,
        }
    }

    pub fn parse_instruction_body(input: &str) -> InstructionBody {
        let ctx = ParserContext::new(None, input);
        INSTRUCTION_BODY_PARSER.parse(&ctx, input).unwrap()
    }

    pub fn parse_function_statement(input: &str) -> FunctionStatement {
        let ctx = ParserContext::new(None, input);
        match FUNCTION_STATEMENT_PARSER.parse(&ctx, input).unwrap() {
            powdr_ast::parsed::asm::FunctionStatement::Assignment(source, lhs, reg, rhs) => {
                AssignmentStatement {
                    source,
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
            powdr_ast::parsed::asm::FunctionStatement::Instruction(source, instruction, inputs) => {
                InstructionStatement {
                    source,
                    instruction,
                    inputs,
                }
                .into()
            }
            powdr_ast::parsed::asm::FunctionStatement::Label(source, name) => {
                LabelStatement { source, name }.into()
            }
            _ => unimplemented!(),
        }
    }

    pub fn parse_pil_statement(input: &str) -> PilStatement {
        let ctx = ParserContext::new(None, input);
        PIL_STATEMENT_PARSER.parse(&ctx, input).unwrap()
    }

    pub fn parse_register_declaration<T: FieldElement>(
        input: &str,
    ) -> RegisterDeclarationStatement {
        let ctx = ParserContext::new(None, input);
        match REGISTER_DECLARATION_PARSER.parse(&ctx, input).unwrap() {
            MachineStatement::RegisterDeclaration(source, name, flag) => {
                let ty = match flag {
                    Some(RegisterFlag::IsAssignment) => RegisterTy::Assignment,
                    Some(RegisterFlag::IsPC) => RegisterTy::Pc,
                    Some(RegisterFlag::IsReadOnly) => RegisterTy::ReadOnly,
                    None => RegisterTy::Write,
                };
                RegisterDeclarationStatement { source, name, ty }
            }
            _ => unreachable!(),
        }
    }
}
