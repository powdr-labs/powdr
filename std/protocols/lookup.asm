use std::array;
use std::array::fold;
use std::array::len;
use std::array::map;
use std::check::assert;
use std::check::panic;
use std::constraints::to_phantom_lookup;
use std::math::extension_field::Ext;
use std::math::extension_field::add_ext;
use std::math::extension_field::sub_ext;
use std::math::extension_field::mul_ext;
use std::math::extension_field::unpack_ext_array;
use std::math::extension_field::next_ext;
use std::math::extension_field::inv_ext;
use std::math::extension_field::eval_ext;
use std::math::extension_field::from_base;
use std::math::extension_field::from_array;
use std::math::extension_field::constrain_eq_ext;
use std::math::extension_field::required_extension_size;
use std::math::extension_field::needs_extension;
use std::protocols::fingerprint::fingerprint;
use std::protocols::fingerprint::fingerprint_inter;
use std::utils::unwrap_or_else;

let unpack_lookup_constraint: Constr -> (expr, expr[], expr, expr[]) = |lookup_constraint| match lookup_constraint {
    Constr::Lookup((lhs_selector, rhs_selector), values) => (
        unwrap_or_else(lhs_selector, || 1),
        map(values, |(lhs, _)| lhs),
        unwrap_or_else(rhs_selector, || 1),
        map(values, |(_, rhs)| rhs)
    ),
    _ => panic("Expected lookup constraint")
};

/// Compute z' = z + 1/(beta-a_i) * lhs_selector - m_i/(beta-b_i) * rhs_selector, using extension field arithmetic
let compute_next_z: Ext<expr>, Ext<expr>, Ext<expr>, Constr, expr -> fe[] = query |acc, alpha, beta, lookup_constraint, multiplicities| {
    let (lhs_selector, lhs, rhs_selector, rhs) = unpack_lookup_constraint(lookup_constraint);
    
    let lhs_denom = sub_ext(eval_ext(beta), fingerprint(array::eval(lhs), alpha));
    let rhs_denom = sub_ext(eval_ext(beta), fingerprint(array::eval(rhs), alpha));
    let m_ext = from_base(multiplicities);
    
    // acc' = acc + 1/(beta-a_i) * lhs_selector - m_i/(beta-b_i) * rhs_selector
    let res = add_ext(
        eval_ext(acc),
        sub_ext(
            mul_ext(
                inv_ext(lhs_denom), 
                eval_ext(from_base(lhs_selector))),
            mul_ext(
                mul_ext(eval_ext(m_ext), inv_ext(rhs_denom)),
                eval_ext(from_base(rhs_selector))
        )
    ));
    unpack_ext_array(res)
};
    
/// Transforms a single lookup constraint to identity constraints, challenges and
/// higher-stage witness columns.
/// Use this function if the backend does not support lookup constraints natively.
let lookup: Constr -> () = constr |lookup_constraint| {

    let extension_field_size = required_extension_size();

    // Alpha is used to compress the LHS and RHS arrays.
    let alpha = from_array(array::new(extension_field_size, |i| challenge(0, i + 1)));
    // Beta is used to update the accumulator.
    let beta = from_array(array::new(extension_field_size, |i| challenge(0, i + 1 + extension_field_size)));

    let (lhs_selector, lhs, rhs_selector, rhs) = unpack_lookup_constraint(lookup_constraint);

    let lhs_denom = sub_ext(beta, fingerprint_inter(lhs, alpha));
    let rhs_denom = sub_ext(beta, fingerprint_inter(rhs, alpha));
    let multiplicities;
    let m_ext = from_base(multiplicities);

    let acc = array::new(extension_field_size, |i| std::prover::new_witness_col_at_stage("acc", 1));
    let acc_ext = from_array(acc);
    let next_acc = next_ext(acc_ext);

    // Update rule:
    // acc' * (beta - A) * (beta - B)  + m * rhs_selector * (beta - A) = acc * (beta - A) * (beta - B) + lhs_selector * (beta - B)
    // => (acc' - acc) * lhs_denom * rhs_denom
    //    + m * rhs_selector * lhs_denom
    //    - lhs_selector * rhs_denom = 0

    let update_expr = sub_ext(
        add_ext(
            mul_ext(
                mul_ext(lhs_denom, rhs_denom),
                sub_ext(next_acc, acc_ext)
            ),
            mul_ext(
                mul_ext(m_ext, from_base(rhs_selector)),
                lhs_denom
            )
        ),
        mul_ext(
            from_base(lhs_selector),
            rhs_denom
        )
    );

    let is_first: col = std::well_known::is_first;

    // First and last acc needs to be 0
    // (because of wrapping, the acc[0] and acc[N] are the same)
    is_first * (acc[0] - 1) = 0;
    array::new(array::len(acc) - 1, |i| is_first * acc[i + 1] = 0);
    constrain_eq_ext(update_expr, from_base(0));

    // Add an annotation for witness generation
    to_phantom_lookup(lookup_constraint, multiplicities);
    
    // In the extension field, we need a prover function for the accumulator.
    if needs_extension() {
        // TODO: Helper columns, because we can't access the previous row in hints
        let acc_next_col = std::array::map(acc, |_| std::prover::new_witness_col_at_stage("acc_next", 1));
        query |i| {
            let _ = std::array::zip(
                acc_next_col,
                compute_next_z(acc_ext, alpha, beta, lookup_constraint, multiplicities),
                |acc_next, hint_val| std::prover::provide_value(acc_next, i, hint_val)
            );
        };
        std::array::zip(acc, acc_next_col, |acc_col, acc_next| {
            acc_col' = acc_next
        });
    } else {
    }
};