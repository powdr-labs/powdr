/// Generate the rom
/// If this was written in the language, it would be something like
// fn romgen(self) -> Self {
//     self.instructions.extend([
//         Instruction::with_name("_return"), // etc, could also be a special keyword instead of a function
//         Instruction::with_name("_reset"), // etc, to reset the registers
//         Instruction::with_name("_jump_to_operation"), // etc, to jump to the correct operation
//     ]);
//
//     let main = Function::with_name("_main").statements(
//         [
//             Statement::Label("_start"),
//             Statement::Instruction("_reset"),
//             Statement::Instruction("_jump_to_operation")
//         ].chain(self.functions.iter().flat_map(|f|
//                 once(Statement::Label(format!("_{}", f.name))).chain(f.statements),
//         ))
//     );
//
//     self.functions.push(main); // this could be a special field in the machine also, instead of a function
//
//     self
// }

// Alternatively, this could be modelled as a Machine wrapper. It would be inlined, as we need access to the submachine pc, etc

// machine DynLib<Machine> {
//     // we could indicate that we want this machine to be inlined here, allowing us to add constraints about it
//     inlined Machine m;
//
//     // declare assignment registers for return values
//     <%
//     for o in m.outputs {
//         reg format!("o")[<=];
//     }
//     %>

//     constraints {
//         pol witness function_id;
//         pol commit function_id;
//         ((1 - instr_return) * (function_id' - function_id)) = 0;
//     }

//     // a return instruction which does nothing, we use its flag in constraints
//     instr return {}

//     // reset registers to 0 and leave inputs free
//     instr _reset {
//         <%
//         for reg in m.write_regs {
//             reg = 0;
//         }
//         for i in m.inputs {
//             format!("{}_read_free", i) = 1;
//         }
//         %>
//     }

//     instr _jump_to_instruction {
//         m.pc' = function_id;
//     }

//     instr _loop {
//         m.pc' = m.pc;
//     }

//     function main(<%m.inputs%>) -> <%m.outputs%> {
//         _start::
//         _reset::
//         _jump_to_instruction;
//         <%
//         for f in m.functions {
//             format!("_{}::", f.name);
//             f.body
//         }
//         %>
//         _sink::
//         _loop;
//     }
// }
use ast::asm_analysis::{AnalysisASMFile, Machine, Rom};
use number::FieldElement;
use std::{iter::once, marker::PhantomData};

use crate::utils::{
    parse_instruction_body, parse_instruction_definition, parse_operation_statement,
};

/// Generate the ROM for each machine based on its functions
pub fn generate_rom<T: FieldElement>(file: AnalysisASMFile<T>) -> AnalysisASMFile<T> {
    RomGenerator::default().generate(file)
}

#[derive(Default)]
struct RomGenerator<T> {
    marker: PhantomData<T>,
}

impl<T: FieldElement> RomGenerator<T> {
    fn generate(&self, file: AnalysisASMFile<T>) -> AnalysisASMFile<T> {
        AnalysisASMFile {
            machines: file
                .machines
                .into_iter()
                .map(|(name, m)| (name, self.generate_machine_rom(m)))
                .collect(),
        }
    }

    fn generate_machine_rom(&self, mut machine: Machine<T>) -> Machine<T> {
        if machine.has_pc() {
            let function_id = "_function_id";

            let pc = machine.pc().unwrap();

            // add the necessary embedded instructions
            let embedded_instructions = [
                parse_instruction_definition(&format!(
                    "instr _jump_to_operation {{ {pc}' = {function_id} }}",
                )),
                parse_instruction_definition(&format!(
                    "instr _reset {{ {} }}",
                    machine
                        .write_registers()
                        .map(|r| format!("{} = 0", r.name))
                        .collect::<Vec<_>>()
                        .join(", ")
                )),
                parse_instruction_definition(&format!("instr _loop {{ {}' = {} }}", pc, pc)),
            ];

            machine.instructions.extend(embedded_instructions);

            // implement the return instruction. For this dispatcher, returning jumps to 0
            machine
                .instructions
                .iter_mut()
                .find(|i| i.name == "return")
                .unwrap()
                .body = parse_instruction_body(&format!("{{ {} = 0 }}", machine.pc().unwrap()));
            let rom = vec![
                vec![
                    parse_operation_statement("_start::"),
                    parse_operation_statement("_reset;"),
                    // the latch is constrained to start at 0 using _L1
                    parse_operation_statement("_jump_to_operation;"),
                ],
                // define one label for each operation
                machine
                    .functions
                    .iter()
                    .flat_map(|o| {
                        once(parse_operation_statement(&format!("_{}::", o.name)))
                            // execute the operation after substituting the
                            .chain(o.body.statements.clone().into_iter())
                    })
                    .collect(),
                vec![
                    parse_operation_statement("_sink::"),
                    parse_operation_statement("_loop;"),
                ],
            ]
            .into_iter()
            .flatten()
            .collect();

            machine.function_id = Some(function_id.into());

            machine.rom = Some(Rom {
                statements: rom,
                batches: None,
            });
        } else {
            // do nothing for static machines
        };

        machine
    }
}

#[cfg(test)]
mod tests {
    use number::Bn254Field;
    use parser::parse_asm;
    use pretty_assertions::assert_eq;

    use crate::type_check::TypeChecker;

    use super::*;

    fn parse_check_and_romgen<T: FieldElement>(input: &str) -> Machine<T> {
        let mut checker = TypeChecker::default();
        checker.machines_types.insert("VM".into(), None);
        checker
            .check_machine_type(parse_asm(None, input).unwrap().machines.pop().unwrap())
            .unwrap();
        let generator = RomGenerator::default();
        generator.generate_machine_rom(
            checker
                .machines_types
                .into_iter()
                .next()
                .unwrap()
                .1
                .unwrap(),
        )
    }

    #[test]
    fn vm() {
        let vm = r#"
            machine VM {

                reg pc[@pc];
                reg X[<=];
                reg Y[<=];
                reg Z[<=];
                reg A;
                reg B;
            
                instr add X, Y -> Z { X + Y = Z }
            
                instr sub X, Y -> Z { X - Y = Z }
            
                function add x: field, y: field -> field {
                    A <=Z= add(x, y);
                    return A;
                }
            
                function sub x: field, y: field -> field {
                    A <=Z= sub(x, y);
                    return A;
                }
            }
        "#;

        let machine: Machine<Bn254Field> = parse_check_and_romgen(vm);

        assert_eq!(
            machine.constraints[0].to_string().trim(),
            r#"
constraints {
pol commit function_id;
((1 - instr_return) * (function_id' - function_id)) = 0;
}
"#
            .trim()
            .trim()
        );

        assert_eq!(
            machine.rom.unwrap().to_string().replace('\t', "    "),
            r#"    
rom {
     _start::
     _reset;
     _jump_to_operation;
     _add::
     A <=Z= add(_input_0, _input_1);
     return A;
     _sub::
     A <=Z= sub(_input_0, _input_1);
     return A;
     _sink::
     _loop;
}
"#
            .trim()
        );
    }
}
