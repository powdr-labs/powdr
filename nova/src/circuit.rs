use std::{
    collections::BTreeMap,
    iter,
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
        evaluate_expr, find_pc_expression, get_num_at_index, signed_limb_to_neg, WitnessGen,
    },
    FREE_INPUT_DUMMY_REG, FREE_INPUT_INSTR_NAME, FREE_INPUT_TY, LIMB_WIDTH,
};

/// this NovaStepCircuit can compile single instruction in PIL into R1CS constraints
#[derive(Clone, Debug)]
pub struct NovaStepCircuit<'a, F: PrimeField, T: FieldElement> {
    _p: PhantomData<F>,
    augmented_circuit_index: usize,
    pi_len: usize,
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
        pi_len: usize,
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
            pi_len,
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
        self.num_registers + self.pi_len + self.rom_len
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
            )?;
            poly_map.insert(k.clone(), v);
            Ok::<(), SynthesisError>(())
        })?;
        // add constant 1
        poly_map.insert("ONE".to_string(), alloc_one(cs.namespace(|| "constant 1"))?);

        // add constant 2^(LIMB_WIDTH + 1)
        let mut max_limb_plus_one = [0u8; 64];
        max_limb_plus_one[LIMB_WIDTH / 8] = 1u8;
        let max_limb_plus_one = F::from_uniform(&max_limb_plus_one[..]);
        poly_map.insert(
            "1 <<(LIMB_WIDTH + 1)".to_string(),
            alloc_const(
                cs.namespace(|| "constant 1 <<(LIMB_WIDTH + 1)"),
                max_limb_plus_one,
            )?,
        );

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
            &z[self.pi_len + self.num_registers..],
        )?;
        let (input_params, output_params) = self.io_params;
        // -------------
        let rom_value_bignat = BigNat::from_num(
            cs.namespace(|| "rom value bignat"),
            &Num::from(rom_value),
            LIMB_WIDTH,
            1 + input_params.len() + output_params.len(), // 1 is opcode_index
        )?;
        let input_output_params_allocnum = rom_value_bignat
            .as_limbs()
            .iter()
            .zip_eq(
                iter::once::<Option<&Param>>(None)
                    .chain(input_params.iter().map(Some))
                    .chain(output_params.iter().map(Some)),
            )
            .enumerate()
            .map(|(limb_index, (limb, param))| {
                match param {
                    // signed handling
                    Some(Param {
                        ty: Some(type_str), ..
                    }) if type_str == "signed" => signed_limb_to_neg(
                        cs.namespace(|| format!("limb index {}", limb_index)),
                        limb,
                        &poly_map["1 <<(LIMB_WIDTH + 1)"],
                        LIMB_WIDTH,
                    ),
                    _ => limb.as_allocated_num(
                        cs.namespace(|| format!("rom decompose index {}", limb_index)),
                    ),
                }
            })
            .collect::<Result<Vec<AllocatedNum<F>>, SynthesisError>>()?;

        let opcode_index_from_rom = &input_output_params_allocnum[0]; // opcode_index in 0th element
        let mut offset = 1;
        let input_params_allocnum = // fetch range belongs to input params
            &input_output_params_allocnum[offset..offset + input_params.len()];
        offset += input_params.len();
        let output_params_allocnum = // fetch range belongs to output params
            &input_output_params_allocnum[offset..offset + output_params.len()];

        let expected_opcode_index = alloc_const(
            cs.namespace(|| "expected_opcode_index"),
            F::from(self.augmented_circuit_index as u64),
        )?;
        cs.enforce(
            || "opcode equality",
            |lc| lc + opcode_index_from_rom.get_variable(),
            |lc| lc + CS::one(),
            |lc| lc + expected_opcode_index.get_variable(),
        );

        // process identites
        // all the shared constraints will be compiled into each augmented circuit
        // TODO optimized to compile only nessesary shared constraints
        for (index, id) in self.analyzed.identities.iter().enumerate() {
            match id.kind {
                IdentityKind::Polynomial => {
                    // everthing should be in left.selector only
                    assert_eq!(id.right.expressions.len(), 0);
                    assert_eq!(id.right.selector, None);
                    assert_eq!(id.left.expressions.len(), 0);

                    let exp = id.expression_for_poly_id();

                    // identities process as below
                    // case 1: ((main.instr_XXXX * ) - 0) = 0 => `BinaryOperation(BinaryOperation... `
                    // case 1.1: (() - 0) = 0 => `BinaryOperation(BinaryOperation... `
                    // case 2: PolynomialReference - () = 0 => `BinaryOperation(PolynomialReference...`
                    match exp {
                        // case 1/1.1
                        Expression::BinaryOperation(
                            box Expression::BinaryOperation(
                                box Expression::PolynomialReference(
                                    PolynomialReference { name, next, .. },
                                    ..,
                                ),
                                _,
                                box rhs,
                            ),
                            ..,
                        ) => {
                            // skip next
                            if *next {
                                unimplemented!("not support column next in folding scheme")
                            }
                            // skip first_step, for it's not being used in folding scheme
                            if name == "main.first_step" {
                                continue;
                            }
                            // only skip if constraint is dedicated to other instruction
                            if name.starts_with("main.instr")
                                && !name.ends_with(&self.identity_name[..])
                            {
                                continue;
                            }

                            let contains_next_ref = exp.contains_next_ref();
                            if contains_next_ref {
                                unimplemented!("not support column next in folding scheme")
                            }
                            let num_eval = if name.starts_with("main.instr")
                                && name.ends_with(&self.identity_name[..])
                            {
                                evaluate_expr(
                                    cs.namespace(|| format!("id index {} rhs {}", index, rhs)),
                                    &mut poly_map,
                                    rhs,
                                    self.witgen.clone(),
                                )?
                            } else {
                                evaluate_expr(
                                    cs.namespace(|| format!("id index {} exp {}", index, exp)),
                                    &mut poly_map,
                                    exp,
                                    self.witgen.clone(),
                                )?
                            };
                            cs.enforce(
                                || format!("id index {} constraint {} = 0", index, name),
                                |lc| lc + num_eval.get_variable(),
                                |lc| lc + CS::one(),
                                |lc| lc,
                            );
                        }
                        // case 2
                        Expression::BinaryOperation(
                            box Expression::PolynomialReference(
                                PolynomialReference { name, next, .. },
                                ..,
                            ),
                            _,
                            _,
                        ) => {
                            if *next {
                                continue;
                            }
                            let num_eval = evaluate_expr(
                                cs.namespace(|| format!("id index {} exp {}", index, exp)),
                                &mut poly_map,
                                exp,
                                self.witgen.clone(),
                            )?;
                            cs.enforce(
                                || format!("id index {} constraint {} = 0", index, name),
                                |lc| lc + num_eval.get_variable(),
                                |lc| lc + CS::one(),
                                |lc| lc,
                            );
                        }
                        _ => unimplemented!("exp {:?} not support", exp),
                    }
                }
                _ => (),
            }
        }

        // special handling for free input
        if self.identity_name == FREE_INPUT_INSTR_NAME {
            assert_eq!(
                input_params_allocnum.len(),
                1,
                "free input must have exactly one input param"
            );
            let public_input_index = &input_params_allocnum[0];
            // alloc a intermediate dummy intermediate witness
            // here just to retrieve it's value
            // it wont be under-constraints, as intermediate witness be constraints in below input/output parts
            poly_map.insert(
                FREE_INPUT_DUMMY_REG.to_string(),
                AllocatedNum::alloc(cs.namespace(|| "dummy var"), || {
                    public_input_index
                        .get_value()
                        .and_then(|index| {
                            let repr = &index.to_repr();
                            let index = u32::from_le_bytes(repr.as_ref()[0..4].try_into().unwrap());
                            assert!(index < self.pi_len as u32);
                            z[self.num_registers..][index as usize].get_value()
                        })
                        .ok_or(SynthesisError::AssignmentMissing)
                })?,
            );
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
                        )?,
                    ),
                    // constant
                    Param { name, ty: Some(ty) } if ty == "signed" || ty == "unsigned" => {
                        (
                            format!("instr_{}_param_{}", self.identity_name, name),
                            index.clone(),
                        )
                    },
                    // public io
                    Param { name, ty: Some(ty) } if ty == FREE_INPUT_TY => {
                        (
                            name.clone(),
                            get_num_at_index(
                                cs.namespace(|| format!("regname {}", name)),
                                index,
                                &z[self.num_registers..],
                            )?,
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
                        let i_alloc = alloc_const(
                            cs.namespace(|| format!("output reg i{} allocated", i)),
                            F::from(i as u64),
                        )?;
                        let equal_bit = Boolean::from(alloc_num_equals(
                            cs.namespace(|| format!("check reg {} equal bit", i)),
                            &i_alloc,
                            output_index,
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
                        if name == "main.pc" && *next {
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
