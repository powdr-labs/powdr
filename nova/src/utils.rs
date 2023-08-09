//! This module implements various util function, which is copy from Nova non-public mod
//! https://github.com/microsoft/Nova/blob/main/src/gadgets/mod.rs#L5
use std::{
    collections::BTreeMap,
    sync::{Arc, Mutex},
};

use crate::nonnative::util::Num;

#[allow(dead_code)]
use super::nonnative::bignat::{nat_to_limbs, BigNat};
use ast::{
    analyzed::{Expression, PolynomialReference},
    parsed::BinaryOperator,
};
use bellperson::{
    gadgets::{
        boolean::{AllocatedBit, Boolean},
        num::AllocatedNum,
        Assignment,
    },
    ConstraintSystem, LinearCombination, SynthesisError, Variable,
};
use ff::{
    derive::bitvec::{prelude::Lsb0, view::AsBits},
    Field, PrimeField, PrimeFieldBits,
};
use nova_snark::traits::{Group, PrimeFieldExt};
use num_bigint::BigInt;
use number::FieldElement;

/// Gets as input the little indian representation of a number and spits out the number
pub fn le_bits_to_num<Scalar, CS>(
    mut cs: CS,
    bits: &[AllocatedBit],
) -> Result<AllocatedNum<Scalar>, SynthesisError>
where
    Scalar: PrimeField + PrimeFieldBits,
    CS: ConstraintSystem<Scalar>,
{
    // We loop over the input bits and construct the constraint
    // and the field element that corresponds to the result
    let mut lc = LinearCombination::zero();
    let mut coeff = Scalar::ONE;
    let mut fe = Some(Scalar::ZERO);
    for bit in bits.iter() {
        lc = lc + (coeff, bit.get_variable());
        fe = bit.get_value().map(|val| {
            if val {
                fe.unwrap() + coeff
            } else {
                fe.unwrap()
            }
        });
        coeff = coeff.double();
    }
    let num = AllocatedNum::alloc(cs.namespace(|| "Field element"), || {
        fe.ok_or(SynthesisError::AssignmentMissing)
    })?;
    lc = lc - num.get_variable();
    cs.enforce(|| "compute number from bits", |lc| lc, |lc| lc, |_| lc);
    Ok(num)
}

/// Allocate a variable that is set to zero
pub fn alloc_zero<F: PrimeField, CS: ConstraintSystem<F>>(
    mut cs: CS,
) -> Result<AllocatedNum<F>, SynthesisError> {
    let zero = AllocatedNum::alloc(cs.namespace(|| "alloc"), || Ok(F::ZERO))?;
    cs.enforce(
        || "check zero is valid",
        |lc| lc,
        |lc| lc,
        |lc| lc + zero.get_variable(),
    );
    Ok(zero)
}

/// Allocate a variable that is set to one
pub fn alloc_one<F: PrimeField, CS: ConstraintSystem<F>>(
    mut cs: CS,
) -> Result<AllocatedNum<F>, SynthesisError> {
    let one = AllocatedNum::alloc(cs.namespace(|| "alloc"), || Ok(F::ONE))?;
    cs.enforce(
        || "check one is valid",
        |lc| lc + CS::one(),
        |lc| lc + CS::one(),
        |lc| lc + one.get_variable(),
    );

    Ok(one)
}

/// alloc a field num as a constant
/// where every bit is deterministic constraint in R1CS
pub fn alloc_const<F: PrimeField, CS: ConstraintSystem<F>>(
    mut cs: CS,
    value: F,
    n_bits: usize,
) -> Result<AllocatedNum<F>, SynthesisError> {
    let allocations: Vec<Variable> = value.to_repr().as_bits::<Lsb0>()[0..n_bits]
        .iter()
        .enumerate()
        .map(|(index, raw_bit)| {
            let bit = cs.alloc(
                || "boolean",
                || {
                    if *raw_bit {
                        Ok(F::ONE)
                    } else {
                        Ok(F::ZERO)
                    }
                },
            )?;
            if *raw_bit {
                cs.enforce(
                    || format!("{:?} index {index} true", value),
                    |lc| lc + bit,
                    |lc| lc + CS::one(),
                    |lc| lc + CS::one(),
                );
            } else {
                cs.enforce(
                    || format!("{:?} index {index} false", value),
                    |lc| lc + bit,
                    |lc| lc + CS::one(),
                    |lc| lc,
                );
            }
            Ok(bit)
        })
        .collect::<Result<Vec<Variable>, SynthesisError>>()?;
    let mut f = F::ONE;
    let sum = allocations
        .iter()
        .fold(LinearCombination::zero(), |lc, bit| {
            let l = lc + (f, *bit);
            f = f.double();
            l
        });
    let value_alloc =
        AllocatedNum::alloc(cs.namespace(|| format!("{:?} alloc const", value)), || {
            Ok(value)
        })?;
    let sum_lc = LinearCombination::zero() + value_alloc.get_variable() - &sum;
    cs.enforce(
        || format!("{:?} sum - value = 0", value),
        |lc| lc + &sum_lc,
        |lc| lc + CS::one(),
        |lc| lc,
    );
    Ok(value_alloc)
}

/// Allocate a scalar as a base. Only to be used is the scalar fits in base!
pub fn alloc_scalar_as_base<G, CS>(
    mut cs: CS,
    input: Option<G::Scalar>,
) -> Result<AllocatedNum<G::Base>, SynthesisError>
where
    G: Group,
    <G as Group>::Scalar: PrimeFieldBits,
    CS: ConstraintSystem<<G as Group>::Base>,
{
    AllocatedNum::alloc(cs.namespace(|| "allocate scalar as base"), || {
        let input_bits = input.unwrap_or(G::Scalar::ZERO).clone().to_le_bits();
        let mut mult = G::Base::ONE;
        let mut val = G::Base::ZERO;
        for bit in input_bits {
            if bit {
                val += mult;
            }
            mult = mult + mult;
        }
        Ok(val)
    })
}

/// interepret scalar as base
pub fn scalar_as_base<G: Group>(input: G::Scalar) -> G::Base {
    let input_bits = input.to_le_bits();
    let mut mult = G::Base::ONE;
    let mut val = G::Base::ZERO;
    for bit in input_bits {
        if bit {
            val += mult;
        }
        mult = mult + mult;
    }
    val
}

/// Allocate bignat a constant
pub fn alloc_bignat_constant<F: PrimeField, CS: ConstraintSystem<F>>(
    mut cs: CS,
    val: &BigInt,
    limb_width: usize,
    n_limbs: usize,
) -> Result<BigNat<F>, SynthesisError> {
    let limbs = nat_to_limbs(val, limb_width, n_limbs).unwrap();
    let bignat = BigNat::alloc_from_limbs(
        cs.namespace(|| "alloc bignat"),
        || Ok(limbs.clone()),
        None,
        limb_width,
        n_limbs,
    )?;
    // Now enforce that the limbs are all equal to the constants
    #[allow(clippy::needless_range_loop)]
    for i in 0..n_limbs {
        cs.enforce(
            || format!("check limb {i}"),
            |lc| lc + &bignat.limbs[i],
            |lc| lc + CS::one(),
            |lc| lc + (limbs[i], CS::one()),
        );
    }
    Ok(bignat)
}

/// Check that two numbers are equal and return a bit
pub fn alloc_num_equals<F: PrimeField, CS: ConstraintSystem<F>>(
    mut cs: CS,
    a: &AllocatedNum<F>,
    b: &AllocatedNum<F>,
) -> Result<AllocatedBit, SynthesisError> {
    // Allocate and constrain `r`: result boolean bit.
    // It equals `true` if `a` equals `b`, `false` otherwise
    let r_value = match (a.get_value(), b.get_value()) {
        (Some(a), Some(b)) => Some(a == b),
        _ => None,
    };

    let r = AllocatedBit::alloc(cs.namespace(|| "r"), r_value)?;

    // Allocate t s.t. t=1 if z1 == z2 else 1/(z1 - z2)

    let t = AllocatedNum::alloc(cs.namespace(|| "t"), || {
        Ok(if *a.get_value().get()? == *b.get_value().get()? {
            F::ONE
        } else {
            (*a.get_value().get()? - *b.get_value().get()?)
                .invert()
                .unwrap()
        })
    })?;

    cs.enforce(
        || "t*(a - b) = 1 - r",
        |lc| lc + t.get_variable(),
        |lc| lc + a.get_variable() - b.get_variable(),
        |lc| lc + CS::one() - r.get_variable(),
    );

    cs.enforce(
        || "r*(a - b) = 0",
        |lc| lc + r.get_variable(),
        |lc| lc + a.get_variable() - b.get_variable(),
        |lc| lc,
    );

    Ok(r)
}

/// If condition return a otherwise b
pub fn conditionally_select<F: PrimeField, CS: ConstraintSystem<F>>(
    mut cs: CS,
    a: &AllocatedNum<F>,
    b: &AllocatedNum<F>,
    condition: &Boolean,
) -> Result<AllocatedNum<F>, SynthesisError> {
    let c = AllocatedNum::alloc(cs.namespace(|| "conditional select result"), || {
        if *condition.get_value().get()? {
            Ok(*a.get_value().get()?)
        } else {
            Ok(*b.get_value().get()?)
        }
    })?;

    // a * condition + b*(1-condition) = c ->
    // a * condition - b*condition = c - b
    cs.enforce(
        || "conditional select constraint",
        |lc| lc + a.get_variable() - b.get_variable(),
        |_| condition.lc(CS::one(), F::ONE),
        |lc| lc + c.get_variable() - b.get_variable(),
    );

    Ok(c)
}

/// If condition return a otherwise b
pub fn conditionally_select_vec<F: PrimeField, CS: ConstraintSystem<F>>(
    mut cs: CS,
    a: &[AllocatedNum<F>],
    b: &[AllocatedNum<F>],
    condition: &Boolean,
) -> Result<Vec<AllocatedNum<F>>, SynthesisError> {
    a.iter()
        .zip(b.iter())
        .enumerate()
        .map(|(i, (a, b))| {
            conditionally_select(cs.namespace(|| format!("select_{i}")), a, b, condition)
        })
        .collect::<Result<Vec<AllocatedNum<F>>, SynthesisError>>()
}

/// If condition return a otherwise b where a and b are BigNats
pub fn conditionally_select_bignat<F: PrimeField, CS: ConstraintSystem<F>>(
    mut cs: CS,
    a: &BigNat<F>,
    b: &BigNat<F>,
    condition: &Boolean,
) -> Result<BigNat<F>, SynthesisError> {
    assert!(a.limbs.len() == b.limbs.len());
    let c = BigNat::alloc_from_nat(
        cs.namespace(|| "conditional select result"),
        || {
            if *condition.get_value().get()? {
                Ok(a.value.get()?.clone())
            } else {
                Ok(b.value.get()?.clone())
            }
        },
        a.params.limb_width,
        a.params.n_limbs,
    )?;

    // a * condition + b*(1-condition) = c ->
    // a * condition - b*condition = c - b
    for i in 0..c.limbs.len() {
        cs.enforce(
            || format!("conditional select constraint {i}"),
            |lc| lc + &a.limbs[i] - &b.limbs[i],
            |_| condition.lc(CS::one(), F::ONE),
            |lc| lc + &c.limbs[i] - &b.limbs[i],
        );
    }
    Ok(c)
}

/// Same as the above but Condition is an AllocatedNum that needs to be
/// 0 or 1. 1 => True, 0 => False
pub fn conditionally_select2<F: PrimeField, CS: ConstraintSystem<F>>(
    mut cs: CS,
    a: &AllocatedNum<F>,
    b: &AllocatedNum<F>,
    condition: &AllocatedNum<F>,
) -> Result<AllocatedNum<F>, SynthesisError> {
    let c = AllocatedNum::alloc(cs.namespace(|| "conditional select result"), || {
        if *condition.get_value().get()? == F::ONE {
            Ok(*a.get_value().get()?)
        } else {
            Ok(*b.get_value().get()?)
        }
    })?;

    // a * condition + b*(1-condition) = c ->
    // a * condition - b*condition = c - b
    cs.enforce(
        || "conditional select constraint",
        |lc| lc + a.get_variable() - b.get_variable(),
        |lc| lc + condition.get_variable(),
        |lc| lc + c.get_variable() - b.get_variable(),
    );

    Ok(c)
}

/// If condition set to 0 otherwise a. Condition is an allocated num
pub fn select_zero_or_num2<F: PrimeField, CS: ConstraintSystem<F>>(
    mut cs: CS,
    a: &AllocatedNum<F>,
    condition: &AllocatedNum<F>,
) -> Result<AllocatedNum<F>, SynthesisError> {
    let c = AllocatedNum::alloc(cs.namespace(|| "conditional select result"), || {
        if *condition.get_value().get()? == F::ONE {
            Ok(F::ZERO)
        } else {
            Ok(*a.get_value().get()?)
        }
    })?;

    // a * (1 - condition) = c
    cs.enforce(
        || "conditional select constraint",
        |lc| lc + a.get_variable(),
        |lc| lc + CS::one() - condition.get_variable(),
        |lc| lc + c.get_variable(),
    );

    Ok(c)
}

/// If condition set to a otherwise 0. Condition is an allocated num
pub fn select_num_or_zero2<F: PrimeField, CS: ConstraintSystem<F>>(
    mut cs: CS,
    a: &AllocatedNum<F>,
    condition: &AllocatedNum<F>,
) -> Result<AllocatedNum<F>, SynthesisError> {
    let c = AllocatedNum::alloc(cs.namespace(|| "conditional select result"), || {
        if *condition.get_value().get()? == F::ONE {
            Ok(*a.get_value().get()?)
        } else {
            Ok(F::ZERO)
        }
    })?;

    cs.enforce(
        || "conditional select constraint",
        |lc| lc + a.get_variable(),
        |lc| lc + condition.get_variable(),
        |lc| lc + c.get_variable(),
    );

    Ok(c)
}

/// If condition set to a otherwise 0
pub fn select_num_or_zero<F: PrimeField, CS: ConstraintSystem<F>>(
    mut cs: CS,
    a: &AllocatedNum<F>,
    condition: &Boolean,
) -> Result<AllocatedNum<F>, SynthesisError> {
    let c = AllocatedNum::alloc(cs.namespace(|| "conditional select result"), || {
        if *condition.get_value().get()? {
            Ok(*a.get_value().get()?)
        } else {
            Ok(F::ZERO)
        }
    })?;

    cs.enforce(
        || "conditional select constraint",
        |lc| lc + a.get_variable(),
        |_| condition.lc(CS::one(), F::ONE),
        |lc| lc + c.get_variable(),
    );

    Ok(c)
}

/// If condition set to 1 otherwise a
pub fn select_one_or_num2<F: PrimeField, CS: ConstraintSystem<F>>(
    mut cs: CS,
    a: &AllocatedNum<F>,
    condition: &AllocatedNum<F>,
) -> Result<AllocatedNum<F>, SynthesisError> {
    let c = AllocatedNum::alloc(cs.namespace(|| "conditional select result"), || {
        if *condition.get_value().get()? == F::ONE {
            Ok(F::ONE)
        } else {
            Ok(*a.get_value().get()?)
        }
    })?;

    cs.enforce(
        || "conditional select constraint",
        |lc| lc + CS::one() - a.get_variable(),
        |lc| lc + condition.get_variable(),
        |lc| lc + c.get_variable() - a.get_variable(),
    );
    Ok(c)
}

/// If condition set to 1 otherwise a - b
pub fn select_one_or_diff2<F: PrimeField, CS: ConstraintSystem<F>>(
    mut cs: CS,
    a: &AllocatedNum<F>,
    b: &AllocatedNum<F>,
    condition: &AllocatedNum<F>,
) -> Result<AllocatedNum<F>, SynthesisError> {
    let c = AllocatedNum::alloc(cs.namespace(|| "conditional select result"), || {
        if *condition.get_value().get()? == F::ONE {
            Ok(F::ONE)
        } else {
            Ok(*a.get_value().get()? - *b.get_value().get()?)
        }
    })?;

    cs.enforce(
        || "conditional select constraint",
        |lc| lc + CS::one() - a.get_variable() + b.get_variable(),
        |lc| lc + condition.get_variable(),
        |lc| lc + c.get_variable() - a.get_variable() + b.get_variable(),
    );
    Ok(c)
}

/// If condition set to a otherwise 1 for boolean conditions
pub fn select_num_or_one<F: PrimeField, CS: ConstraintSystem<F>>(
    mut cs: CS,
    a: &AllocatedNum<F>,
    condition: &Boolean,
) -> Result<AllocatedNum<F>, SynthesisError> {
    let c = AllocatedNum::alloc(cs.namespace(|| "conditional select result"), || {
        if *condition.get_value().get()? {
            Ok(*a.get_value().get()?)
        } else {
            Ok(F::ONE)
        }
    })?;

    cs.enforce(
        || "conditional select constraint",
        |lc| lc + a.get_variable() - CS::one(),
        |_| condition.lc(CS::one(), F::ONE),
        |lc| lc + c.get_variable() - CS::one(),
    );

    Ok(c)
}

/// c = a + b where a, b is AllocatedNum
pub fn add_allocated_num<F: PrimeField, CS: ConstraintSystem<F>>(
    mut cs: CS,
    a: &AllocatedNum<F>,
    b: &AllocatedNum<F>,
) -> Result<AllocatedNum<F>, SynthesisError> {
    let c = AllocatedNum::alloc(cs.namespace(|| "c"), || {
        Ok(*a.get_value().get()? + b.get_value().get()?)
    })?;
    cs.enforce(
        || "c = a + b",
        |lc| lc + a.get_variable() + b.get_variable(),
        |lc| lc + CS::one(),
        |lc| lc + c.get_variable(),
    );
    Ok(c)
}

#[allow(dead_code)]
/// c = a * b where a, b is AllocatedNum
pub fn mul_allocated_num<F: PrimeField, CS: ConstraintSystem<F>>(
    mut cs: CS,
    a: &AllocatedNum<F>,
    b: &AllocatedNum<F>,
) -> Result<AllocatedNum<F>, SynthesisError> {
    let c = AllocatedNum::alloc(cs.namespace(|| "c"), || {
        Ok(*a.get_value().get()? * b.get_value().get()?)
    })?;
    cs.enforce(
        || "c = a * b",
        |lc| lc + a.get_variable(),
        |lc| lc + b.get_variable(),
        |lc| lc + c.get_variable(),
    );
    Ok(c)
}

/// witness generation wrapper to support scan witness row by row
#[derive(Clone, Debug)]
pub struct WitnessGen<'a, T: FieldElement> {
    data: Vec<(&'a str, &'a Vec<T>)>,
    cur_index: usize,
    pub cur_witness: BTreeMap<&'a str, &'a T>,
}

impl<'a, T: FieldElement> WitnessGen<'a, T> {
    fn gen_current_witness(
        index: usize,
        data: &Vec<(&'a str, &'a Vec<T>)>,
        prev: &mut BTreeMap<&'a str, &'a T>,
    ) {
        data.iter().for_each(|(k, v)| {
            if let Some(v) = v.get(index) {
                prev.insert(*k, v);
            } else {
                panic!("out of bound: index {:?} but v is {:?}", index, v);
            }
        });
    }

    pub fn new(data: Vec<(&'a str, &'a Vec<T>)>) -> Self {
        let mut cur_witness = BTreeMap::new();
        Self::gen_current_witness(0, &data, &mut cur_witness);
        Self {
            data,
            cur_index: 0,
            cur_witness,
        }
    }

    pub fn get_wit_by_key(&self, k: &str) -> Option<&T> {
        if let Some(v) = self.cur_witness.get(k) {
            Some(v)
        } else {
            panic!("key {:?} not found in {:?}", k, self.cur_witness);
        }
    }

    pub fn next(&mut self) {
        self.cur_index += 1;
        Self::gen_current_witness(self.cur_index, &self.data, &mut self.cur_witness);
    }

    pub fn num_of_iteration(&self) -> usize {
        // just collect first witness column as len, since all column will be same length
        self.data
            .get(0)
            .map(|first| first.1.len())
            .unwrap_or_default()
    }
}

pub fn get_num_at_index<F: PrimeFieldExt, CS: ConstraintSystem<F>>(
    mut cs: CS,
    index_before_offset: &AllocatedNum<F>,
    z: &[AllocatedNum<F>],
    offset: usize,
) -> Result<AllocatedNum<F>, SynthesisError> {
    let index = AllocatedNum::alloc(cs.namespace(|| "index"), || {
        Ok(index_before_offset.get_value().unwrap() + F::from(offset as u64))
    })?;

    // select target when index match or empty
    let zero = alloc_zero(cs.namespace(|| "zero"))?;
    let _selected_num = z
        .iter()
        .enumerate()
        .map(|(i, z)| {
            let i_alloc = AllocatedNum::alloc(
                cs.namespace(|| format!("_selected_circuit_index i{} allocated", i)),
                || Ok(F::from(i as u64)),
            )?;
            let equal_bit = Boolean::from(alloc_num_equals(
                cs.namespace(|| format!("check selected_circuit_index {} equal bit", i)),
                &i_alloc,
                &index,
            )?);
            conditionally_select(
                cs.namespace(|| format!("select on index namespace {}", i)),
                z,
                &zero,
                &equal_bit,
            )
        })
        .collect::<Result<Vec<AllocatedNum<F>>, SynthesisError>>()?;

    let selected_num = _selected_num
        .iter()
        .enumerate()
        .try_fold(zero, |agg, (i, _num)| {
            add_allocated_num(cs.namespace(|| format!("selected_num {}", i)), _num, &agg)
        })?;
    Ok(selected_num)
}

/// get negative field value from signed limb
pub fn signed_limb_to_neg<F: PrimeFieldExt, CS: ConstraintSystem<F>>(
    mut cs: CS,
    limb: &Num<F>,
    max_limb_plus_one_const: &AllocatedNum<F>,
    nbit: usize,
) -> Result<AllocatedNum<F>, SynthesisError> {
    let limb_alloc = limb.as_allocated_num(cs.namespace(|| "rom decompose index"))?;
    let bits = limb.decompose(cs.namespace(|| "index decompose bits"), nbit)?;
    let signed_bit = &bits.allocations[nbit - 1];
    let twos_complement = AllocatedNum::alloc(cs.namespace(|| "alloc twos complement"), || {
        max_limb_plus_one_const
            .get_value()
            .zip(limb_alloc.get_value())
            .map(|(a, b)| a - b)
            .ok_or(SynthesisError::AssignmentMissing)
    })?;
    cs.enforce(
        || "constraints 2's complement",
        |lc| lc + twos_complement.get_variable() + limb_alloc.get_variable(),
        |lc| lc + CS::one(),
        |lc| lc + max_limb_plus_one_const.get_variable(),
    );
    let twos_complement_neg = AllocatedNum::alloc(cs.namespace(|| " 2's complment neg"), || {
        twos_complement
            .get_value()
            .map(|v| v.neg())
            .ok_or(SynthesisError::AssignmentMissing)
    })?;
    cs.enforce(
        || "constraints 2's complement additive neg",
        |lc| lc + twos_complement.get_variable() + twos_complement_neg.get_variable(),
        |lc| lc + CS::one(),
        |lc| lc,
    );

    let c = AllocatedNum::alloc(cs.namespace(|| "conditional select"), || {
        signed_bit
            .value
            .map(|signed_bit_value| {
                if signed_bit_value {
                    twos_complement_neg.get_value().unwrap()
                } else {
                    limb_alloc.get_value().unwrap()
                }
            })
            .ok_or(SynthesisError::AssignmentMissing)
    })?;

    // twos_complement_neg * condition + limb_alloc*(1-condition) = c ->
    // twos_complement_neg * condition - limb_alloc*condition = c - limb_alloc
    cs.enforce(
        || "index conditional select",
        |lc| lc + twos_complement_neg.get_variable() - limb_alloc.get_variable(),
        |_| signed_bit.bit.clone(),
        |lc| lc + c.get_variable() - limb_alloc.get_variable(),
    );
    Ok(c)
}

// TODO optmize constraints to leverage R1CS cost-free additive
// TODO combine FieldElement & PrimeField
pub fn evaluate_expr<'a, T: FieldElement, F: PrimeFieldExt, CS: ConstraintSystem<F>>(
    mut cs: CS,
    poly_map: &mut BTreeMap<String, AllocatedNum<F>>,
    expr: &Expression<T>,
    witgen: Arc<Mutex<WitnessGen<'a, T>>>,
) -> Result<AllocatedNum<F>, SynthesisError> {
    match expr {
        Expression::Number(n) => {
            AllocatedNum::alloc(cs.namespace(|| format!("{:x?}", n.to_string())), || {
                let mut n_le = n.to_bytes_le();
                n_le.resize(64, 0);
                Ok(F::from_uniform(&n_le[..]))
            })
        }
        // this is refer to another polynomial, in other word, witness
        Expression::PolynomialReference(PolynomialReference {
            index, name, next, ..
        }) => {
            let name = name.strip_prefix("main.").unwrap(); // TODO FIXME: trim namespace should be happened in unified place
            assert_eq!(*index, None);
            assert_eq!(*next, false);

            Ok(poly_map
                .entry(name.to_string())
                .or_insert_with(|| {
                    AllocatedNum::alloc(cs.namespace(|| format!("{}.{}", expr, name)), || {
                        let wit_value = witgen.lock().unwrap().get_wit_by_key(name).cloned();
                        let mut n_le = wit_value.unwrap().to_bytes_le();
                        n_le.resize(64, 0);
                        let f = F::from_uniform(&n_le[..]);
                        Ok(f)
                    })
                    .unwrap()
                })
                .clone())
        }
        Expression::BinaryOperation(lhe, op, rhe) => {
            let lhe = evaluate_expr(cs.namespace(|| "lhe"), poly_map, lhe, witgen.clone())?;
            let rhe = evaluate_expr(cs.namespace(|| "rhe"), poly_map, rhe, witgen)?;
            match op {
                BinaryOperator::Add => add_allocated_num(cs, &lhe, &rhe),
                BinaryOperator::Sub => {
                    let rhe_neg: AllocatedNum<F> =
                        AllocatedNum::alloc(cs.namespace(|| "inv"), || {
                            rhe.get_value()
                                .map(|v| v.neg())
                                .ok_or_else(|| SynthesisError::AssignmentMissing)
                        })?;

                    // (a + a_neg) * 1 = 0
                    cs.enforce(
                        || "(a + a_neg) * 1 = 0",
                        |lc| lc + rhe.get_variable() + rhe_neg.get_variable(),
                        |lc| lc + CS::one(),
                        |lc| lc,
                    );

                    add_allocated_num(cs, &lhe, &rhe_neg)
                }
                BinaryOperator::Mul => mul_allocated_num(cs, &lhe, &rhe),
                _ => unimplemented!("{}", expr),
            }
        }
        Expression::Constant(constant_name) => {
            poly_map
                .get(constant_name)
                .cloned()
                .ok_or_else(|| SynthesisError::AssignmentMissing) // constant must exist
        }
        _ => unimplemented!("{}", expr),
    }
}

// find rhs expression where the left hand side is the instr_name, e.g.  instr_name * (<next pc expression>)
// this is a workaround and inefficient way, since we are working on PIL file. Need to optimise it.
pub fn find_pc_expression<T: FieldElement, F: PrimeField, CS: ConstraintSystem<F>>(
    expr: &Expression<T>,
    instr_name: &String,
) -> Option<Box<Expression<T>>> {
    match expr {
        Expression::Number(_) => None,
        // this is refer to another polynomial, in other word, witness
        Expression::PolynomialReference(PolynomialReference { .. }) => None,
        Expression::BinaryOperation(lhe, operator, rhe) => {
            let find_match_expr = match lhe {
                // early pattern match on lhs to retrive the instr * (<pc_expression>)
                box Expression::PolynomialReference(PolynomialReference { name, .. }) => {
                    if name == instr_name && *operator == BinaryOperator::Mul {
                        Some(rhe)
                    } else {
                        None
                    }
                }
                _ => None,
            };
            find_match_expr
                .cloned()
                .or_else(|| find_pc_expression::<T, F, CS>(rhe, instr_name))
                .or_else(|| find_pc_expression::<T, F, CS>(lhe, instr_name))
        }
        Expression::Constant(_) => None,
        _ => unimplemented!("{}", expr),
    }
}
