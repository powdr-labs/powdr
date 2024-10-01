use std::array;
use std::array::fold;
use std::array::len;
use std::array::map;
use std::check::assert;
use std::check::panic;
use std::math::fp2::Fp2;
use std::math::fp2::add_ext;
use std::math::fp2::sub_ext;
use std::math::fp2::mul_ext;
use std::math::fp2::unpack_ext;
use std::math::fp2::unpack_ext_array;
use std::math::fp2::next_ext;
use std::math::fp2::inv_ext;
use std::math::fp2::eval_ext;
use std::math::fp2::from_base;
use std::math::fp2::fp2_from_array;
use std::math::fp2::constrain_eq_ext;
use std::math::fp2::required_extension_size;
use std::math::fp2::needs_extension;
use std::protocols::fingerprint::fingerprint;
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
let compute_next_z: Fp2<expr>, Fp2<expr>, Fp2<expr>, Constr, expr -> fe[] = query |acc, alpha, beta, lookup_constraint, multiplicities| {
    let (lhs_selector, lhs, rhs_selector, rhs) = unpack_lookup_constraint(lookup_constraint);
    
    let lhs_denom = sub_ext(beta, fingerprint(lhs, alpha));
    let rhs_denom = sub_ext(beta, fingerprint(rhs, alpha));
    let m_ext = from_base(multiplicities);
    
    // acc' = acc + 1/(beta-a_i) * lhs_selector - m_i/(beta-b_i) * rhs_selector
    let res = add_ext(
        eval_ext(acc),
        sub_ext(
            mul_ext(
                inv_ext(eval_ext(lhs_denom)), 
                eval_ext(from_base(lhs_selector))),
            mul_ext(
                mul_ext(eval_ext(m_ext), inv_ext(eval_ext(rhs_denom))),
                eval_ext(from_base(rhs_selector))
        )
    ));
    unpack_ext_array(res)
};
    
/// Transfroms a single lookup constraint to identity constraint, challenges and
/// higher-stage witness columns.
/// Use this function if the backend does not support lookup constraints natively.
/// WARNING: This function can currently not be used multiple times since
/// the used challenges would overlap.
/// TODO: Implement this for an array of constraints.
/// Arguments:
/// - lookup_constraint: The lookup constraint
/// - multiplicities: A multiplicities column which shows how many times each row of the RHS value appears in the LHS                  
let lookup: Constr, expr -> () = constr |lookup_constraint, multiplicities| {
    std::check::assert(required_extension_size() <= 2, || "Invalid extension size");
    // Alpha is used to compress the LHS and RHS arrays.
    let alpha = fp2_from_array(array::new(required_extension_size(), |i| challenge(0, i + 1)));
    // Beta is used to update the accumulator.
    let beta = fp2_from_array(array::new(required_extension_size(), |i| challenge(0, i + 3)));

    let (lhs_selector, lhs, rhs_selector, rhs) = unpack_lookup_constraint(lookup_constraint);

    let lhs_denom = sub_ext(beta, fingerprint(lhs, alpha));
    let rhs_denom = sub_ext(beta, fingerprint(rhs, alpha));
    let m_ext = from_base(multiplicities);

    let acc = array::new(required_extension_size(), |i| std::prover::new_witness_col_at_stage("acc", 1));
    let acc_ext = fp2_from_array(acc);
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

    let (acc_1, acc_2) = unpack_ext(acc_ext);
    // First and last acc needs to be 0
    // (because of wrapping, the acc[0] and acc[N] are the same)
    is_first * acc_1 = 0;
    is_first * acc_2 = 0;
    constrain_eq_ext(update_expr, from_base(0));

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