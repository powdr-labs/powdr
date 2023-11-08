use acvm::acir::brillig::Opcode as BrilligOpcode;
use acvm::acir::circuit::Circuit;
use acvm::acir::circuit::Opcode;
use acvm::brillig_vm::brillig::BinaryFieldOp;
use acvm::brillig_vm::brillig::Label;
use acvm::brillig_vm::brillig::RegisterIndex;

use rand::Rng;
use std::fs;
use std::io::Write;
use std::iter;

use std::collections::HashMap;

/// Module to convert brillig assmebly into powdr assembly

// struct BrilligArchitecture {}

// impl Architecture for BrilligArchitecture {
//     fn instruction_ends_control_flow(instr: &str) -> bool {
//         match instr {
//             "li" | "lui" | "la" | "mv" | "add" | "addi" | "sub" | "neg" | "mul" | "mulhu"
//             | "divu" | "xor" | "xori" | "and" | "andi" | "or" | "ori" | "not" | "slli" | "sll"
//             | "srli" | "srl" | "srai" | "seqz" | "snez" | "slt" | "slti" | "sltu" | "sltiu"
//             | "sgtz" | "beq" | "beqz" | "bgeu" | "bltu" | "blt" | "bge" | "bltz" | "blez"
//             | "bgtz" | "bgez" | "bne" | "bnez" | "jal" | "jalr" | "call" | "ecall" | "ebreak"
//             | "lw" | "lb" | "lbu" | "lh" | "lhu" | "sw" | "sh" | "sb" | "nop" | "fence"
//             | "fence.i" | "amoadd.w.rl" | "amoadd.w" => false,
//             "j" | "jr" | "tail" | "ret" | "trap" => true,
//             _ => {
//                 panic!("Unknown instruction: {instr}");
//             }
//         }
//     }

//     fn get_references<'a, R: asm_utils::ast::Register, F: asm_utils::ast::FunctionOpKind>(
//         instr: &str,
//         args: &'a [asm_utils::ast::Argument<R, F>],
//     ) -> Vec<&'a str> {
//         // fence arguments are not symbols, they are like reserved
//         // keywords affecting the instruction behavior
//         if instr.starts_with("fence") {
//             Vec::new()
//         } else {
//             symbols_in_args(args)
//         }
//     }
// }

fn main() {
    // Read in file called bytecode.acir
    let bytecode = fs::read("bytecode.acir").expect("Unable to read file");
    // Convert the read-in base64 file into Vec<u8>
    let decoded = base64::decode(bytecode).expect("Failed to decode base64");
    let bytecode = decoded;

    // Create a new circuit from the bytecode instance
    let circuit: Circuit =
        Circuit::deserialize_circuit(&bytecode).expect("Failed to deserialize circuit");

    println!("circuit: {:?}", circuit);

    // Get the brillig opcodes
    let brillig = extract_brillig(circuit.opcodes);
    print!("{:?}", brillig);

    let preamble = get_preamble();
    let program = construct_main(brillig);
    let powdr = brillig_machine(&preamble, program);

    println!("powdr: {:?}", powdr);

    // temp write the output to a file
    let mut file = fs::File::create("brillig_out.asm").expect("Could not create file");
    file.write_all(powdr.as_bytes())
        .expect("Could not write to file");
}

fn brillig_machine(
    // machines: &[&str],
    preamble: &str,
    // submachines: &[(&str, &str)],
    program: Vec<String>,
) -> String {
    format!(
        r#"
machine Main {{

{}

    function main {{
{}
    }}
}}    
"#,
        preamble,
        program
            .into_iter()
            .map(|line| format!("\t\t{line}"))
            .collect::<Vec<_>>()
            .join("\n")
    )
}

// Output the powdr assembly with the given circuit
fn construct_main(program: Opcode) -> Vec<String> {
    let mut main_asm: Vec<String> = Vec::new();

    // For each instruction in brillig, we want o
    let trace = match program {
        Opcode::Brillig(brillig) => brillig.bytecode,
        _ => {
            panic!("Opcode is not of type brillig");
        }
    };

    println!();
    println!();
    trace.iter().for_each(|i| println!("{:?}", i));
    println!();
    println!();

    // Label of [index], String, where index is the generated name of the jump, we will place a jump label there when
    // we encounter it to prove
    let mut labels: HashMap<Label, String> = HashMap::new();

    for (index, instr) in trace.into_iter().enumerate() {
        println!("{:?}", instr);
        println!();
        println!();
        println!();
        // powdr_asm.push_str(&instr.to_string());

        // If we require a label to be placed at the jump location then we add it
        if let Some(jump) = labels.get(&index) {
            main_asm.push(format!("{}::", jump));
        }

        match instr {
            BrilligOpcode::Const { destination, value } => {
                let number = value.to_usize().to_string();
                main_asm.push(format!("{} <=X= {};", print_register(destination), number));
            }
            BrilligOpcode::Stop => {
                main_asm.push("return;".to_owned());
            }
            BrilligOpcode::Return => {
                main_asm.push("ret;".to_owned());
            }
            // Calls -> look more into how this is performed internally
            // For calls we want to store the current pc in a holding register such that we can return to it
            // We then want to jump to that location in the bytecode
            BrilligOpcode::Call { location } => {
                // Generate a label for the location we are going to
                let label = gen_label();
                labels.insert(location, label.clone()); // This label will be inserted later on!

                main_asm.push(format!("call {};", label));
            }
            BrilligOpcode::BinaryFieldOp {
                destination,
                op,
                lhs,
                rhs,
            } => {
                // Match the given operation
                match op {
                    BinaryFieldOp::Add => {
                        main_asm.push(format!(
                            "{} <== add({}, {});",
                            print_register(destination),
                            print_register(lhs),
                            print_register(rhs)
                        ));
                    }
                    BinaryFieldOp::Sub => {
                        main_asm.push(format!(
                            "{} <== sub({}, {});",
                            print_register(destination),
                            print_register(lhs),
                            print_register(rhs)
                        ));
                    }
                    // Equals is currently a mix of the equals instruction and the using the X is 0 witness column
                    BinaryFieldOp::Equals => {
                        main_asm.push(format!(
                            "tmp <== sub({}, {});",
                            print_register(lhs),
                            print_register(rhs)
                        ));
                        main_asm.push(format!("{} <== eq(tmp);", print_register(destination),));
                    }
                    BinaryFieldOp::Mul => {
                        main_asm.push(format!(
                            "{} <== mul({}, {});",
                            print_register(destination),
                            print_register(lhs),
                            print_register(rhs)
                        ));
                    }
                    // TODO: div
                    _ => println!("not implemented"),
                }
            }
            _ => println!("not implemented"),
        }
    }

    println!("main_asm: {:?}", main_asm);

    main_asm
}

fn gen_label() -> String {
    let mut rng = rand::thread_rng();
    let hex_chars: Vec<char> = "abcdef".chars().collect();
    // Lmao chat gpt fix
    let label: String = iter::repeat(())
        .map(|()| rng.gen_range(0..hex_chars.len()))
        .map(|i| hex_chars[i])
        .take(4)
        .collect();

    label
}

fn print_register(r_index: RegisterIndex) -> String {
    let num = r_index.to_usize();
    format!("r{}", num).to_owned()
}

// Read the preamble from the brillig.asm machine
fn get_preamble() -> String {
    fs::read_to_string("brillig.asm").expect("Unable to read file")
}

fn extract_brillig(opcodes: Vec<Opcode>) -> Opcode {
    if opcodes.len() != 1 {
        panic!("There should only be one brillig opcode");
    }
    let opcode = &opcodes[0];
    if opcode.name() != "brillig" {
        panic!("Opcode is not of type brillig");
    }
    opcode.clone()
}
