use std::{
    collections::BTreeMap,
    marker::PhantomData,
    sync::{Arc, Mutex},
};

use ast::{
    analyzed::{Analyzed, Expression, IdentityKind, PolynomialReference},
    parsed::{asm::Param, BinaryOperator},
};
use bellperson::{
    gadgets::{boolean::Boolean, num::AllocatedNum},
    ConstraintSystem, SynthesisError,
};
use ff::PrimeField;
use itertools::Itertools;
use log::warn;
use nova_snark::traits::{circuit_supernova::StepCircuit, PrimeFieldExt};
use number::FieldElement;

use crate::{
    nonnative::{bignat::BigNat, util::Num},
    utils::{
        add_allocated_num, alloc_const, alloc_num_equals, alloc_one, conditionally_select,
        evaluate_expr, find_pc_expression, get_num_at_index, WitnessGen,
    },
    LIMB_WIDTH,
};

/// this NovaStepCircuit can compile single instruction in PIL into R1CS constraints
#[derive(Clone, Debug)]
pub struct NovaStepCircuit<'a, F: PrimeField, T: FieldElement> {
    _p: PhantomData<F>,
    augmented_circuit_index: usize,
    rom_len: usize,
    identity_name: String,
    io_params: &'a (Vec<Param>, Vec<Param>), // input,output index
    analyzed: &'a Analyzed<T>,
    num_registers: usize,
    witgen: Arc<Mutex<WitnessGen<'a, T>>>,
}

impl<'a, F, T> NovaStepCircuit<'a, F, T>
where
    F: PrimeField,
    T: FieldElement,
{
    /// new
    pub fn new(
        rom_len: usize,
        augmented_circuit_index: usize,
        identity_name: String,
        analyzed: &'a Analyzed<T>,
        io_params: &'a (Vec<Param>, Vec<Param>),
        num_registers: usize,
        witgen: Arc<Mutex<WitnessGen<'a, T>>>,
    ) -> Self {
        NovaStepCircuit {
            rom_len,
            augmented_circuit_index,
            identity_name,
            analyzed,
            io_params,
            num_registers,
            witgen,
            _p: PhantomData,
        }
    }
}

impl<'a, F, T> StepCircuit<F> for NovaStepCircuit<'a, F, T>
where
    F: PrimeFieldExt,
    T: FieldElement,
{
    fn arity(&self) -> usize {
        self.num_registers + self.rom_len
    }

    fn synthesize<CS: ConstraintSystem<F>>(
        &self,
        cs: &mut CS,
        _pc_counter: &AllocatedNum<F>,
        z: &[AllocatedNum<F>],
    ) -> Result<(AllocatedNum<F>, Vec<AllocatedNum<F>>), SynthesisError> {
        // mapping <name| can be register, constant, ...> to AllocatedNum<F>
        let mut poly_map = BTreeMap::new();

        // process pc
        poly_map.insert("pc".to_string(), _pc_counter.clone());

        // process constants and build map for its reference
        self.analyzed.constants.iter().try_for_each(|(k, v)| {
            let mut v_le = v.to_bytes_le();
            v_le.resize(64, 0);
            let v = alloc_const(
                cs.namespace(|| format!("const {:?}", v)),
                F::from_uniform(&v_le[..]),
                64,
            )?;
            poly_map.insert(k.clone(), v);
            Ok::<(), SynthesisError>(())
        })?;
        // add constant 1
        poly_map.insert("ONE".to_string(), alloc_one(cs.namespace(|| "constant 1"))?);

        // parse inst part to construct step circuit
        // decompose ROM[pc] into linear combination lc(opcode_index, operand_index1, operand_index2, ... operand_output)
        // Noted that here only support single output
        // register value can be constrait via `multiple select + sum` with register index on nova `zi` state
        // output register need to be constraints in the last of synthesize

        // NOTES: things that do not support
        // 1. next query. only support pc as next, other value are all query on same rotation
        // ...

        let rom_value = get_num_at_index(
            cs.namespace(|| "rom value"),
            _pc_counter,
            z,
            self.num_registers,
        )?;
        let (input_params, output_params) = self.io_params;
        // -------------
        let rom_value_bignat = BigNat::from_num(
            cs.namespace(|| "rom value bignat"),
            &Num::from(rom_value),
            LIMB_WIDTH as usize,
            1 + input_params.len() + output_params.len(), // 1 is opcode_index
        )?;
        let input_output_params_allocnum = rom_value_bignat
            .as_limbs()
            .iter()
            .enumerate()
            .map(|(limb_index, limb)| {
                limb.as_allocated_num(
                    cs.namespace(|| format!("rom decompose index {}", limb_index)),
                )
            })
            .collect::<Result<Vec<AllocatedNum<F>>, SynthesisError>>()?;

        let opcode_index_from_rom = &input_output_params_allocnum[0]; // opcode_index in 0th element
        let mut offset = 1;
        let input_params_allocnum = // fetch range belongs to input params
            &input_output_params_allocnum[offset..offset + input_params.len()];
        offset += input_params.len();
        let output_params_allocnum = // fetch range belongs to output params
            &input_output_params_allocnum[offset..offset + output_params.len()];

        let expected_opcode_index =
            AllocatedNum::alloc(cs.namespace(|| "expected_opcode_index"), || {
                Ok(F::from(self.augmented_circuit_index as u64))
            })?;
        cs.enforce(
            || "opcode equality",
            |lc| lc + opcode_index_from_rom.get_variable(),
            |lc| lc + CS::one(),
            |lc| lc + expected_opcode_index.get_variable(),
        );

        for id in &self.analyzed.identities {
            match id.kind {
                IdentityKind::Polynomial => {
                    // everthing should be in left.selector only
                    assert_eq!(id.right.expressions.len(), 0);
                    assert_eq!(id.right.selector, None);
                    assert_eq!(id.left.expressions.len(), 0);

                    let exp = id.expression_for_poly_id();

                    match exp {
                        Expression::BinaryOperation(
                            box Expression::BinaryOperation(
                                box Expression::PolynomialReference(
                                    PolynomialReference { name, .. },
                                    ..,
                                ),
                                BinaryOperator::Mul,
                                box rhs,
                            ),
                            ..,
                        ) => {
                            // only build constraints on matched identities
                            // TODO replace with better regular expression ?
                            if !(name.starts_with("main.instr")
                                && name.ends_with(&self.identity_name[..]))
                            {
                                continue;
                            }

                            let contains_next_ref = exp.contains_next_ref();
                            if contains_next_ref == true {
                                unimplemented!("not support column next in folding scheme")
                            }
                            let mut cs = cs.namespace(|| format!("rhs {}", rhs));
                            let num_eval = evaluate_expr(
                                cs.namespace(|| format!("constraint {}", name)),
                                &mut poly_map,
                                rhs,
                                self.witgen.clone(),
                            )?;
                            cs.enforce(
                                || format!("constraint {} = 0", name),
                                |lc| lc + num_eval.get_variable(),
                                |lc| lc + CS::one(),
                                |lc| lc,
                            );
                        }
                        _ => (), // println!("exp {:?} not support", exp),
                    }
                }
                _ => (),
            }
        }

        // constraint input name to index value
        input_params_allocnum
            .iter()
            .zip_eq(input_params.iter())
            .try_for_each(|(index, params)| {
                let (name, value) = match params {
                    // register
                    Param { name, ty: None } => (
                        name.clone(),
                        get_num_at_index(
                            cs.namespace(|| format!("regname {}", name)),
                            index,
                            z,
                            0,
                        )?,
                    ),
                    // constant
                    Param { name, ty: Some(ty) } if ty == "signed" || ty == "unsigned" => {
                        (
                            format!("instr_{}_param_{}", self.identity_name, name),
                            index.clone(),
                        )
                    },
                    s => {
                        unimplemented!("not support {}", s)
                    },
                };
                if let Some(reg) = poly_map.get(&name) {
                    cs.enforce(
                        || format!("{} - reg[{}_index] = 0", params, params),
                        |lc| lc + value.get_variable(),
                        |lc| lc + CS::one(),
                        |lc| lc + reg.get_variable(),
                    );
                    Ok::<(), SynthesisError>(())
                } else {
                    warn!(
                        "missing input reg name {} in polymap with key {:?}, Probably instr {} defined but never used",
                        params,
                        poly_map.keys(),
                        self.identity_name,
                    );
                    Ok::<(), SynthesisError>(())
                }
            })?;

        // constraint zi_next[index] = (index == output_index) ? reg_assigned[output_reg_name]: zi[index]
        let zi_next = output_params_allocnum.iter().zip_eq(output_params.iter()).try_fold(
            z.to_vec(),
            |acc, (output_index, param)| {
                assert!(param.ty.is_none()); // output only accept register
                (0..z.len())
                    .map(|i| {
                        let i_alloc = AllocatedNum::alloc(
                            cs.namespace(|| format!("output reg i{} allocated", i)),
                            || Ok(F::from(i as u64)),
                        )?;
                        let equal_bit = Boolean::from(alloc_num_equals(
                            cs.namespace(|| format!("check reg {} equal bit", i)),
                            &i_alloc,
                            &output_index,
                        )?);
                        if let Some(output) = poly_map.get(&param.name) {
                            conditionally_select(
                                cs.namespace(|| {
                                    format!(
                                        "zi_next constraint with register index {} on reg name {}",
                                        i, param
                                    )
                                }),
                                output,
                                &acc[i],
                                &equal_bit,
                            )
                        } else {
                            warn!(
                                "missing output reg name {} in polymap with key {:?}, Probably instr {} defined but never used",
                                param,
                                poly_map.keys(),
                                self.identity_name,
                            );
                            Ok(acc[i].clone())
                        }
                    })
                    .collect::<Result<Vec<AllocatedNum<F>>, SynthesisError>>()
            },
        )?;

        // process pc_next
        // TODO: very inefficient to go through all identities for each folding, need optimisation
        for id in &self.analyzed.identities {
            match id.kind {
                IdentityKind::Polynomial => {
                    // everthing should be in left.selector only
                    assert_eq!(id.right.expressions.len(), 0);
                    assert_eq!(id.right.selector, None);
                    assert_eq!(id.left.expressions.len(), 0);

                    let exp = id.expression_for_poly_id();

                    if let Expression::BinaryOperation(
                        box Expression::PolynomialReference(
                            PolynomialReference { name, next, .. },
                            ..,
                        ),
                        BinaryOperator::Sub,
                        box rhs,
                        ..,
                    ) = exp
                    {
                        // lhs is `pc'`
                        if name == "main.pc" && *next == true {
                            let identity_name = format!("main.instr_{}", self.identity_name);
                            let exp = find_pc_expression::<T, F, CS>(rhs, &identity_name);
                            let pc_next = exp
                                .and_then(|expr| {
                                    evaluate_expr(
                                        // evaluate rhs pc bumping logic
                                        cs.namespace(|| format!("pc eval on {}", expr)),
                                        &mut poly_map,
                                        &expr,
                                        self.witgen.clone(),
                                    )
                                    .ok()
                                })
                                .unwrap_or_else(|| {
                                    // by default pc + 1
                                    add_allocated_num(
                                        cs.namespace(|| format!("instr {} pc + 1", identity_name)),
                                        &poly_map["pc"],
                                        &poly_map["ONE"],
                                    )
                                    .unwrap()
                                });
                            poly_map.insert("pc_next".to_string(), pc_next);
                        }
                    }
                }
                _ => (),
            }
        }
        Ok((poly_map["pc_next"].clone(), zi_next))
    }
}

/// A trivial step circuit that simply returns the input
#[derive(Clone, Debug, Default)]
pub struct SecondaryStepCircuit<F: PrimeField> {
    _p: PhantomData<F>,
    arity_len: usize,
}

impl<F> SecondaryStepCircuit<F>
where
    F: PrimeField,
{
    /// new TrivialTestCircuit
    pub fn new(arity_len: usize) -> Self {
        SecondaryStepCircuit {
            arity_len,
            _p: PhantomData,
        }
    }
}

impl<F> StepCircuit<F> for SecondaryStepCircuit<F>
where
    F: PrimeField,
{
    fn arity(&self) -> usize {
        self.arity_len
    }

    fn synthesize<CS: ConstraintSystem<F>>(
        &self,
        _cs: &mut CS,
        _pc_counter: &AllocatedNum<F>,
        z: &[AllocatedNum<F>],
    ) -> Result<(AllocatedNum<F>, Vec<AllocatedNum<F>>), SynthesisError> {
        Ok((_pc_counter.clone(), z.to_vec()))
    }
}
