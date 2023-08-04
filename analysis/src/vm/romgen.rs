//! Generate one ROM per machine from all declared functions

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
use ast::asm_analysis::{
    AnalysisASMFile, Batch, Incompatible, IncompatibleSet, Machine, PilBlock, Rom,
};
use number::FieldElement;
use std::marker::PhantomData;

use crate::utils::{
    parse_function_statement, parse_instruction_body, parse_instruction_definition,
    parse_pil_statement,
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
                        .map(|r| format!("{}' = 0", r.name))
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
                .body = parse_instruction_body(&format!("{{ {}' = 0 }}", machine.pc().unwrap()));

            // generate the rom
            // the functions are already batched, we just batch the dispatcher manually here
            // we could also run batching again on the dispatcher

            let mut rom: Vec<Batch<T>> = vec![];

            // add the beginning of the dispatcher
            rom.extend(vec![
                Batch::from(vec![
                    parse_function_statement("_start::"),
                    parse_function_statement("_reset;"),
                ])
                .reason(IncompatibleSet::from(Incompatible::Unimplemented)),
                // the latch is constrained to start at 0 using _L1
                Batch::from(vec![parse_function_statement("_jump_to_operation;")])
                    .reason(IncompatibleSet::from(Incompatible::Label)),
            ]);

            // add each function, setting the function_id to the current position in the ROM
            for function in machine.functions.iter_mut() {
                function.id = Some(T::from(rom.len() as u64));

                let mut batches: Vec<_> = function
                    .body
                    .statements
                    .clone()
                    .into_iter_batches()
                    .collect();
                // modify the first batch to include the label just for debugging purposes, it's always possible to batch it so it's free
                batches
                    .first_mut()
                    .expect("function should have at least one statement as it must return")
                    .statements
                    .insert(
                        0,
                        parse_function_statement(&format!("_{}::", function.name)),
                    );

                // modify the last batch to be caused by the coming label
                let last = batches
                    .last_mut()
                    .expect("function should have at least one statement as it must return");
                last.set_reason(IncompatibleSet::from(Incompatible::Label));

                rom.extend(batches);
            }

            // THE FOLLOWING IS NECESSARY BECAUSE OF WITGEN, IT COULD BE REMOVED ONCE WITGEN CAN CALL THE INFINITE LOOP ITSELF
            // we get the location of the sink so that witgen jumps to it after the first call is done
            // this constrains the VM to being able to execute only one call, which will be fixed in the future
            let sink_id = T::from(rom.len() as u64);
            let latch = machine.latch.as_ref().unwrap();
            let sigma = "_sigma";

            machine.constraints.push(PilBlock {
                start: 0,
                statements: vec![
                    // declare `_sigma` as the sum of the latch, will be 0 and then 1 after the end of the first call
                    parse_pil_statement(&format!("col witness {sigma}")),
                    parse_pil_statement(&format!(
                        "{sigma}' = (1 - first_step') * ({sigma} + {latch})"
                    )), // HACKY: THIS ASSUMES `first_step` is defined here!!
                    // once `_sigma` is 1, constrain `_function_id` to the label of the sink
                    parse_pil_statement(&format!("{sigma} * ({function_id} - {sink_id}) = 0")),
                ],
            });
            ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

            rom.extend(vec![Batch::from(vec![
                parse_function_statement("_sink::"),
                parse_function_statement("_loop;"),
            ])]);

            machine.function_id = Some(function_id.into());

            machine.rom = Some(Rom {
                statements: rom.into_iter().collect(),
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
    use pretty_assertions::assert_eq;

    use crate::vm::test_utils::generate_rom_str;

    use super::*;

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

        let file: AnalysisASMFile<Bn254Field> = generate_rom_str(vm);

        assert_eq!(
            file.machines
                .iter()
                .next()
                .unwrap()
                .1
                .rom
                .as_ref()
                .unwrap()
                .statements
                .to_string()
                .replace('\t', "    "),
            r#"
_start::
_reset;
// END BATCH Unimplemented
_jump_to_operation;
// END BATCH Label
_add::
A <=Z= add(_input_0, _input_1);
// END BATCH Unimplemented
return A;
// END BATCH Label
_sub::
A <=Z= sub(_input_0, _input_1);
// END BATCH Unimplemented
return A;
// END BATCH Label
_sink::
_loop;
// END BATCH
"#
            .replace('\t', "    ")
            .trim()
        );
    }
}
