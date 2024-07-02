use std::array::fold;
use std::utils::unwrap_or_else;
use std::array::len;
use std::array::map;
use std::check::assert;
use std::check::panic;
use std::field::known_field;
use std::field::KnownField;
use std::math::fp2::Fp2;
use std::math::fp2::add_ext;
use std::math::fp2::sub_ext;
use std::math::fp2::mul_ext;
use std::math::fp2::unpack_ext;
use std::math::fp2::next_ext;
use std::math::fp2::inv_ext;
use std::math::fp2::eval_ext;
use std::math::fp2::from_base;
use std::math::fp2::constrain_eq_ext;

let unpack_lookup_constraint: Constr -> (expr, expr[], expr, expr[]) = |lookup_constraint| match lookup_constraint {
    Constr::Lookup((lhs_selector, rhs_selector), values) => (
        unwrap_or_else(lhs_selector, || 1),
        map(values, |(lhs, _)| lhs),
        unwrap_or_else(rhs_selector, || 1),
        map(values, |(_, rhs)| rhs)
    ),
    _ => panic("Expected lookup constraint")
};

/// Whether we need to operate on the F_{p^2} extension field (because the current field is too small).
let needs_extension: -> bool = || match known_field() {
    Option::Some(KnownField::Goldilocks) => true,
    Option::Some(KnownField::BN254) => false,
    None => panic("The lookup argument is not implemented for the current field!")
};

//* Generic for both permutation and lookup arguments
/// Maps [x_1, x_2, ..., x_n] to alpha**(n - 1) * x_1 + alpha ** (n - 2) * x_2 + ... + x_n
let<T: Add + Mul + FromLiteral> compress_expression_array: T[], Fp2<T> -> Fp2<T> = |expr_array, alpha| fold(
    expr_array,
    from_base(0),
    |sum_acc, el| add_ext(mul_ext(alpha, sum_acc), from_base(el))
);

// Compute z' = z + 1/(beta-a_i) * lhs_selector - m_i/(beta-b_i) * rhs_selector, using extension field arithmetic
let compute_next_z: Fp2<expr>, Fp2<expr>, Fp2<expr>, Constr, expr -> fe[] = query |acc, alpha, beta, lookup_constraint, multiplicities| {
    let (lhs_selector, lhs, rhs_selector, rhs) = unpack_lookup_constraint(lookup_constraint);
    
    let lhs_denom = sub_ext(beta, compress_expression_array(lhs, alpha));
    let rhs_denom = sub_ext(beta, compress_expression_array(rhs, alpha));
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
    match res {
        Fp2::Fp2(a0_fe, a1_fe) => [a0_fe, a1_fe]
    }
};
    
// Adds constraints that enforce that rhs is the lookup for lhs
// Arguments:
// - is_first: A column that is 1 for the first row and 0 for the rest
// - alpha: A challenge used to compress the LHS and RHS values
// - beta: A challenge used to update the accumulator
// - acc: A phase-2 witness column to be used as the accumulator. If 2 are provided, computations
//        are done on the F_{p^2} extension field.
// - lookup_constraint: The lookup constraint
// - multiplicities: The multiplicities which shows how many times each RHS value appears in the LHS                  
let lookup: expr, expr[], Fp2<expr>, Fp2<expr>, Constr, expr -> Constr[] = |is_first, acc, alpha, beta, lookup_constraint, multiplicities| {

    let (lhs_selector, lhs, rhs_selector, rhs) = unpack_lookup_constraint(lookup_constraint);

    let _ = assert(len(lhs) == len(rhs), || "LHS and RHS should have equal length");

    let with_extension = match len(acc) {
        1 => false,
        2 => true,
        _ => panic("Expected 1 or 2 accumulator columns!")
    };

    let _ = if !with_extension {
        assert(!needs_extension(), || "The Goldilocks field is too small and needs to move to the extension field. Pass two accumulators instead!")
    } else { () };

    // On the extension field, we'll need two field elements to represent the challenge.
    // If we don't need an extension field, we can simply set the second component to 0,
    // in which case the operations below effectively only operate on the first component.
    let fp2_from_array = |arr| if with_extension { Fp2::Fp2(arr[0], arr[1]) } else { from_base(arr[0]) };
    let acc_ext = fp2_from_array(acc);

    let lhs_denom = sub_ext(beta, compress_expression_array(lhs, alpha));
    let rhs_denom = sub_ext(beta, compress_expression_array(rhs, alpha));
    let m_ext = from_base(multiplicities);

    let next_acc = if with_extension {
        next_ext(acc_ext)
    } else {
        // The second component is 0, but the next operator is not defined on it...
        from_base(acc[0]')
    };

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

    let (acc_1, acc_2) = unpack_ext(acc_ext);

    [
        is_first * acc_1 = 0,

        is_first * acc_2 = 0
    ] + constrain_eq_ext(update_expr, from_base(0))
};