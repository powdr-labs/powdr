use std::check::assert;
use std::check::panic;
use std::math::fp2::Fp2;
use std::math::fp2::add_ext;
use std::math::fp2::sub_ext;
use std::math::fp2::mul_ext;
use std::math::fp2::inv_ext;
use std::math::fp2::eval_ext;
use std::math::fp2::unpack_ext;
use std::math::fp2::unpack_ext_array;
use std::math::fp2::next_ext;
use std::math::fp2::from_base;
use std::math::fp2::needs_extension;
use std::math::fp2::is_extension;
use std::math::fp2::fp2_from_array;
use std::math::fp2::constrain_eq_ext;
use std::protocols::fingerprint::fingerprint_with_id;
use std::prover::eval;

/// Sends the tuple (id, tuple...) to the bus by adding
/// `multiplicity / (beta - fingerprint(id, tuple...))` to `acc`
/// It is the callers responsibility to properly constrain the multiplicity (e.g. constrain
/// it to be boolean) if needed.
///
/// # Arguments:
///
/// - is_first: A column that is 1 for the first row and 0 for the rest
/// - id: Id for machines to be able to access the same challenge
/// - tuple: An array of columns which will be sending by bus
/// - multiplicity: The multiplicity which shows how many times a column will be sent
/// - acc: A phase-2 witness column to be used as the accumulator. If 2 are provided, computations
///        are done on the F_{p^2} extension field.
/// - alpha: A challenge used to compress id and tuple
/// - beta: A challenge used to update the accumulator
///
/// # Returns:
///
/// - Constraints to be added to enforce the bus
let bus_interaction: expr, expr, expr[], expr, expr[], Fp2<expr>, Fp2<expr> -> Constr[] = |is_first, id, tuple, multiplicity, acc, alpha, beta| {

    // Implemented as: folded = (beta - fingerprint(id, tuple...));
    // `multiplicity / (beta - fingerprint(id, tuple...))` to `acc`
    let folded = sub_ext(beta, fingerprint_with_id(id, tuple, alpha));
    let folded_next = next_ext(folded);

    let m_ext = from_base(multiplicity);
    let m_ext_next = next_ext(m_ext);

    let acc_ext = fp2_from_array(acc);
    let next_acc = next_ext(acc_ext);

    let is_first_next = from_base(is_first');

    // Update rule:
    // fingerprint_with_id(id, (a1', a2', ...), alpha) * (acc' - acc * (1 - is_first')) - multiplicity' = 0
    let update_expr = sub_ext(
        mul_ext(folded_next, sub_ext(next_acc, mul_ext(acc_ext, sub_ext(from_base(1), is_first_next)))), m_ext_next
    );
    
    constrain_eq_ext(update_expr, from_base(0))
};

/// Compute acc' = acc * (1 - is_first') + multiplicity / fingerprint_with_id(id, (a1', a2')),
/// using extension field arithmetic.
/// This is intended to be used as a hint in the extension field case; for the base case
/// automatic witgen is smart enough to figure out the value of the accumulator.
let compute_next_z_send: expr, expr, expr[], expr, Fp2<expr>, Fp2<expr>, Fp2<expr> -> fe[] = query |is_first, id, tuple, multiplicity, acc, alpha, beta| {
    // Implemented as: folded = (beta - fingerprint(id, tuple...));
    // `multiplicity / (beta - fingerprint(id, tuple...))` to `acc`
    let folded = sub_ext(beta, fingerprint_with_id(id, tuple, alpha));
    let folded_next = next_ext(folded);

    let m_ext = from_base(multiplicity);
    let m_ext_next = next_ext(m_ext);

    let is_first_next = eval(is_first');
    let current_acc = if is_first_next == 1 {from_base(0)} else {eval_ext(acc)};
    
    // acc' = acc * (1 - is_first') + multiplicity / fingerprint_with_id(id, (a1', a2'))
    let res = add_ext(
        current_acc,
        mul_ext(eval_ext(m_ext_next), inv_ext(eval_ext(folded_next)))
    );

    unpack_ext_array(res)
};

/// Compute acc' = acc * (1 - is_first') - multiplicity / fingerprint_with_id(id, (a1', a2')),
/// using extension field arithmetic.
/// This is intended to be used as a hint in the extension field case; for the base case
/// automatic witgen is smart enough to figure out the value of the accumulator.
let compute_next_z_receive: expr, expr, expr[], expr, Fp2<expr>, Fp2<expr>, Fp2<expr> -> fe[] = query |is_first, id, tuple, multiplicity, acc, alpha, beta| 
    compute_next_z_send(is_first, id, tuple, -multiplicity, acc, alpha, beta);

/// Convenience function for bus interaction to send columns
let bus_send: expr, expr, expr[], expr, expr[], Fp2<expr>, Fp2<expr> -> Constr[] = |is_first, id, tuple, multiplicity, acc, alpha, beta| {
    bus_interaction(is_first, id, tuple, multiplicity, acc, alpha, beta)
};

/// Convenience function for bus interaction to receive columns
let bus_receive: expr, expr, expr[], expr, expr[], Fp2<expr>, Fp2<expr> -> Constr[] = |is_first, id, tuple, multiplicity, acc, alpha, beta| {
    bus_interaction(is_first, id, tuple, -1 * multiplicity, acc, alpha, beta)
};