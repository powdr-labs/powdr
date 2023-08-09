use core::panic;
use std::{
    collections::BTreeMap,
    iter,
    sync::{Arc, Mutex},
};

use ast::{
    analyzed::Analyzed,
    asm_analysis::{AssignmentStatement, FunctionStatement, InstructionStatement, Machine},
    parsed::{
        asm::{FunctionCall, Param},
        Expression::Number,
        UnaryOperator,
    },
};
use ff::PrimeField;
use nova_snark::provider::bn256_grumpkin::{self};
use nova_snark::{
    compute_digest,
    supernova::{gen_commitmentkey_by_r1cs, PublicParams, RecursiveSNARK, RunningClaim},
    traits::Group,
};
use num_bigint::BigUint;
use number::{BigInt, FieldElement};

use crate::{
    circuit::{NovaStepCircuit, SecondaryStepCircuit},
    nonnative::bignat::limbs_to_nat,
    utils::WitnessGen,
    LIMB_WIDTH,
};

// TODO support other cycling curve
type G1 = bn256_grumpkin::bn256::Point;
type G2 = bn256_grumpkin::grumpkin::Point;

pub(crate) fn nova_prove<T: FieldElement>(
    analyzed: &Analyzed<T>,
    main_machine: &Machine<T>,
    _: Vec<(&str, Vec<T>)>,
    witness: Vec<(&str, Vec<T>)>,
) -> () {
    if polyexen::expr::get_field_p::<<G1 as Group>::Scalar>() != T::modulus().to_arbitrary_integer()
    {
        panic!("powdr modulus doesn't match nova modulus. Make sure you are using Bn254");
    }

    // TODO to avoid clone witness object, witness are wrapped by lock so it can be shared below
    // need to refactor this part to support parallel folding
    // redesign this part if possible
    // println!("fixed {:?}", fixed);
    // println!("witness {:?}", witness);
    let witness = Arc::new(Mutex::new(WitnessGen::new(
        witness
            .iter()
            .map(|(k, v)| (k.strip_prefix("main.").unwrap(), v))
            .collect::<Vec<(&str, &Vec<T>)>>(),
    )));

    // collect all instr from pil file
    let instr_index_mapping: BTreeMap<String, usize> = main_machine
        .instructions
        .iter()
        .flat_map(|k| Some(k.name.clone()))
        .enumerate()
        .map(|(i, v)| (v.clone(), i))
        .collect();
    // collect all register
    let regs_index_mapping: BTreeMap<String, usize> = main_machine
        .registers
        .iter()
        .filter(|reg| {
            reg.flag.is_none() // only collect writable register
        })
        .enumerate()
        .map(|(i, reg)| (reg.name.clone(), i))
        .collect();
    // instruction <-> input/output params mapping
    let instr_io_mapping = main_machine
        .instructions
        .iter()
        .map(|k| {
            let input_index: Vec<Param> = k.params.inputs.params.clone();
            let output_index: Vec<Param> = if let Some(output) = &k.params.outputs {
                output.params.iter().for_each(|output_param| {
                    assert_eq!(output_param.ty.is_none(), true); // do not support signature other than register type
                });
                output.params.clone()
            } else {
                vec![]
            };
            (k.name.clone(), (input_index, output_index))
        })
        .collect::<BTreeMap<String, (Vec<Param>, Vec<Param>)>>();

    // firstly, compile pil ROM to simple memory commitment
    // Idea to represent a instruction is by linear combination lc(instruction,input params.., output params..)
    // params can be register or constant. For register, first we translate to register index
    // decomposed(inst_encoded, 1 << LIMB_WIDTH) = [<instr index>, operand1, operand2, operand3...]
    // each operand should be fit into 1 << LIMB_WIDTH
    // TODO1: move this part to setup stage.
    // TODO2: replace this part with more efficient memory commitment strategy, e.g. folding KZG

    let rom = main_machine.rom.as_ref().map(|rom| {
        rom.statements.iter().map(|statement| match statement {
            FunctionStatement::Assignment(AssignmentStatement {
                lhs,
                rhs,
                .. // ignore start
            }) => {
                let instr_name = match rhs {
                    box ast::parsed::Expression::FunctionCall(FunctionCall{id, ..}) => id,
                    s => unimplemented!("{:?}", s),
                };

                let mut io_params: Vec<<G1 as Group>::Scalar> = match rhs {
                    box ast::parsed::Expression::FunctionCall(FunctionCall{arguments, ..}) => arguments.iter().map(|argument| match argument {
                        ast::parsed::Expression::PolynomialReference(ast::parsed::PolynomialReference{ namespace, name, index, next }) => {
                            assert_eq!(*next, false);
                            assert_eq!(*namespace, None);
                            assert_eq!(*index, None);
                            <G1 as Group>::Scalar::from(regs_index_mapping[name] as u64)
                        },
                        Number(n) => {
                            <G1 as Group>::Scalar::from_bytes(&n.to_bytes_le().try_into().unwrap()).unwrap()
                        },
                        ast::parsed::Expression::UnaryOperation(ope,box Number(n)) => {
                            let value = <G1 as Group>::Scalar::from_bytes(&n.to_bytes_le().try_into().unwrap()).unwrap();
                            match ope {
                                UnaryOperator::Plus => value,
                                UnaryOperator::Minus => get_neg_value_within_limbsize(value, LIMB_WIDTH),
                            }
                        },
                        x => unimplemented!("unsupported expression {}", x),
                    }),
                    _ => unimplemented!(),
                }.collect();

                let output_params:Vec<<G1 as Group>::Scalar> = lhs.iter().map(|x| <G1 as Group>::Scalar::from(regs_index_mapping[x] as u64)).collect();
                io_params.extend(output_params); // append output register to the back of input register

                // Now we can do linear combination
                if let Some(instr_index) = instr_index_mapping.get(instr_name) {
                    limbs_to_nat(iter::once(<G1 as Group>::Scalar::from(*instr_index as u64)).chain(io_params.into_iter()), LIMB_WIDTH).to_biguint().unwrap()
                } else {
                    panic!("instr_name {:?} not found in instr_index_mapping {:?}", instr_name, instr_index_mapping);
                }
            }
            FunctionStatement::Instruction(InstructionStatement{ instruction, inputs, ..}) => {

                let io_params: Vec<<G1 as Group>::Scalar> = inputs.iter().map(|argument| match argument {
                    ast::parsed::Expression::PolynomialReference(ast::parsed::PolynomialReference{ namespace, name, index, next }) => {
                        assert_eq!(*next, false);
                        assert_eq!(*namespace, None);
                        assert_eq!(*index, None);
                        <G1 as Group>::Scalar::from(regs_index_mapping[name] as u64)
                    },
                    Number(n) => <G1 as Group>::Scalar::from_bytes(&n.to_bytes_le().try_into().unwrap()).unwrap(),
                    ast::parsed::Expression::UnaryOperation(ope,box Number(n)) => {
                        let value = <G1 as Group>::Scalar::from_bytes(&n.to_bytes_le().try_into().unwrap()).unwrap();
                        match ope {
                            UnaryOperator::Plus => value,
                            UnaryOperator::Minus => get_neg_value_within_limbsize(value, LIMB_WIDTH),
                        }
                    },
                    x => unimplemented!("unsupported expression {:?}", x),
                }).collect();

                // Now we can do linear combination
                if let Some(instr_index) = instr_index_mapping.get(instruction) {
                    limbs_to_nat(iter::once(<G1 as Group>::Scalar::from(*instr_index as u64)).chain(io_params.into_iter()), LIMB_WIDTH).to_biguint().unwrap()
                } else {
                    panic!("instr_name {} not found in instr_index_mapping {:?}", instruction, instr_index_mapping);
                }
            }
            s => unimplemented!("unimplemented statement {:?}", s),
        }).collect::<Vec<BigUint>>()
    }).unwrap();

    // rom.iter().for_each(|v| println!("rom value {:#32x}", v));

    // build step circuit
    // 2 cycles curve, secondary circuit responsible of folding first circuit running instances with new r1cs instance, no application logic
    let circuit_secondary = SecondaryStepCircuit::new(regs_index_mapping.len() + rom.len());
    let num_augmented_circuit = instr_index_mapping.len();

    // allocated running claims is the list of running instances witness. Number match #instruction
    let mut running_claims: Vec<
        RunningClaim<
            G1,
            G2,
            NovaStepCircuit<<G1 as Group>::Scalar, T>,
            SecondaryStepCircuit<<G2 as Group>::Scalar>,
        >,
    > = instr_index_mapping
        .iter()
        .map(|(instr_name, index)| {
            // Structuring running claims
            let test_circuit = NovaStepCircuit::<<G1 as Group>::Scalar, T>::new(
                rom.len(),
                *index,
                instr_name.to_string(),
                analyzed,
                &instr_io_mapping[instr_name],
                regs_index_mapping.len(),
                witness.clone(),
            );
            let running_claim = RunningClaim::<
                G1,
                G2,
                NovaStepCircuit<<G1 as Group>::Scalar, T>,
                SecondaryStepCircuit<<G2 as Group>::Scalar>,
            >::new(
                *index,
                test_circuit,
                circuit_secondary.clone(),
                num_augmented_circuit,
            );
            running_claim
        })
        .collect();

    // sort running claim by augmented_index, assure we can fetch them by augmented index later
    running_claims.sort_by(|a, b| {
        a.get_augmented_circuit_index()
            .cmp(&b.get_augmented_circuit_index())
    });

    // TODO detect the max circuit iterate and inspect all running claim R1CS shape, instead of assume first 1 is largest
    // generate the commitkey based on max num of constraints and reused it for all other augmented circuit
    // let (max_index_circuit, _) = running_claims
    //     .iter()
    //     .enumerate()
    //     .map(|(i, running_claim)| -> (usize, usize) {
    //         let (r1cs_shape_primary, _) = running_claim.get_r1cs_shape();
    //         (i, r1cs_shape_primary.)
    //     })
    //     .max_by(|(_, circuit_size1), (_, circuit_size2)| circuit_size1.cmp(circuit_size2))
    //     .unwrap();

    let ck_primary = gen_commitmentkey_by_r1cs(&running_claims[0].get_r1cs_shape().0);
    let ck_secondary = gen_commitmentkey_by_r1cs(&running_claims[0].get_r1cs_shape().1);

    // set unified ck_primary, ck_secondary and update digest for all running claim
    running_claims.iter_mut().for_each(|running_claim| {
        running_claim.set_commitmentkey(ck_primary.clone(), ck_secondary.clone())
    });

    let digest =
        compute_digest::<G1, PublicParams<G1, G2>>(&[running_claims[0].get_publicparams()]);
    let initial_program_counter = <G1 as Group>::Scalar::from(0);

    // process register
    let mut z0_primary: Vec<<G1 as Group>::Scalar> = iter::repeat(<G1 as Group>::Scalar::zero())
        .take(regs_index_mapping.len())
        .collect();
    // extend z0_primary/secondary with rom content
    z0_primary.extend(rom.iter().map(|memory_value| {
        let mut memory_value_bytes = memory_value.to_bytes_le();
        memory_value_bytes.resize(32, 0);
        <G1 as Group>::Scalar::from_bytes(&memory_value_bytes.try_into().unwrap()).unwrap()
    }));
    // secondary circuit just fill 0 on z0
    let z0_secondary: Vec<<G2 as Group>::Scalar> = iter::repeat(<G2 as Group>::Scalar::zero())
        .take(z0_primary.len() as usize)
        .collect();

    let mut recursive_snark_option: Option<RecursiveSNARK<G1, G2>> = None;

    // Have estimate of iteration via length of witness
    let num_steps = witness.lock().unwrap().num_of_iteration();
    for i in 0..num_steps {
        println!("round i {}, total step {}", i, num_steps);
        if i > 0 {
            // iterate through next witness row
            witness.lock().unwrap().next();
        }
        let program_counter = recursive_snark_option
            .as_ref()
            .map(|recursive_snark| recursive_snark.get_program_counter())
            .unwrap_or_else(|| initial_program_counter);
        // decompose value_in_rom into (opcode_index, input_operand1, input_operand2, ... output_operand) as witness,
        // then using opcode_index to invoke respective running instance
        let value_in_rom = &rom[u32::from_le_bytes(
            // convert program counter from field to usize (only took le 4 bytes)
            program_counter.to_repr().as_ref()[0..4].try_into().unwrap(),
        ) as usize];
        let mut opcode_index_bytes =
            (value_in_rom % BigUint::from(1_u32 << LIMB_WIDTH)).to_bytes_le();
        opcode_index_bytes.resize(4, 0);
        let opcode_index = u32::from_le_bytes(opcode_index_bytes.try_into().unwrap());

        let mut recursive_snark = recursive_snark_option.unwrap_or_else(|| {
            RecursiveSNARK::iter_base_step(
                &running_claims[opcode_index as usize],
                digest,
                program_counter,
                opcode_index as usize,
                num_augmented_circuit,
                &z0_primary,
                &z0_secondary,
            )
            .unwrap()
        });

        let res = recursive_snark.prove_step(
            &running_claims[opcode_index as usize],
            &z0_primary,
            &z0_secondary,
        );
        if let Err(e) = &res {
            println!("res failed {:?}", e);
        }
        assert!(res.is_ok());
        let res = recursive_snark.verify(
            &running_claims[opcode_index as usize],
            &z0_primary,
            &z0_secondary,
        );
        if let Err(e) = &res {
            println!("res failed {:?}", e);
        }
        assert!(res.is_ok());
        recursive_snark_option = Some(recursive_snark);
    }

    assert!(recursive_snark_option.is_some());

    // Now you can handle the Result using if let
    // let RecursiveSNARK {
    //     zi_primary,
    //     zi_secondary,
    //     program_counter,
    //     ..
    // } = &recursive_snark_option.unwrap();

    // println!("zi_primary: {:?}", zi_primary);
    // println!("zi_secondary: {:?}", zi_secondary);
    // println!("final program_counter: {:?}", program_counter);
}

/// get additive negative of value within limbsize
fn get_neg_value_within_limbsize(
    value: <G1 as Group>::Scalar,
    nbit: usize,
) -> <G1 as Group>::Scalar {
    let value = value.to_bytes();
    let (lsb, msb) = value.split_at(nbit / 8);
    assert_eq!(
        msb.iter().map(|v| *v as usize).sum::<usize>(),
        0,
        "value {:?} is overflow",
        value
    );
    let mut lsb = lsb.to_vec();
    lsb.resize(32, 0);
    let value = <G1 as Group>::Scalar::from_bytes(lsb[..].try_into().unwrap()).unwrap();

    let mut max_limb_plus_one_bytes = vec![0u8; nbit / 8 + 1];
    max_limb_plus_one_bytes[nbit / 8] = 1u8;
    max_limb_plus_one_bytes.resize(32, 0);
    let max_limb_plus_one =
        <G1 as Group>::Scalar::from_bytes(max_limb_plus_one_bytes[..].try_into().unwrap()).unwrap();

    let mut value_neg = (max_limb_plus_one - value).to_bytes()[0..nbit / 8].to_vec();
    value_neg.resize(32, 0);

    <G1 as Group>::Scalar::from_bytes(&value_neg[..].try_into().unwrap()).unwrap()
}
