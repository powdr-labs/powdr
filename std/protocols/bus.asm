use std::check::assert;
use std::check::panic;
use std::math::fp2::Fp2;
use std::math::fp2::add_ext;
use std::math::fp2::sub_ext;
use std::math::fp2::mul_ext;
use std::math::fp2::inv_ext;
use std::math::fp2::eval_ext;
use std::math::fp2::unpack_ext;
use std::math::fp2::next_ext;
use std::math::fp2::from_base;
use std::math::fp2::needs_extension;
use std::math::fp2::is_extension;
use std::math::fp2::fp2_from_array;
use std::math::fp2::constrain_eq_ext;
use std::protocols::fingerprint::fingerprint_with_id;

// The main implementation, sends the tuple (id, tuple...) to the bus by adding
// `multiplicity / (beta - fingerprint(id, tuple...))` to `acc`
// It is the callers responsibility to properly constrain the multiplicity (e.g. constrain
// it to be boolean) if needed.
let bus_interaction: expr, expr, expr[], expr, expr[], Fp2<expr>, Fp2<expr> -> Constr[] = |is_first, id, tuple, multiplicity, acc, alpha, beta| {
    let _ = if !is_extension(acc) {
        assert(!needs_extension(), || "The Goldilocks field is too small and needs to move to the extension field. Pass two accumulators instead!")
    } else { };

    // On the extension field, we'll need two field elements to represent the challenge.
    // If we don't need an extension field, we can simply set the second component to 0,
    // in which case the operations below effectively only operate on the first component.
    let fp2_from_array = |arr| if is_extension(acc) { Fp2::Fp2(arr[0], arr[1]) } else { from_base(arr[0]) };
    let acc_ext = fp2_from_array(acc);

    let next_acc = if is_extension(acc) {
        next_ext(acc_ext)
    } else {
        // The second component is 0, but the next operator is not defined on it...
        from_base(acc[0]')
    };

    // Implemented as: folded = (beta - fingerprint(id, tuple...));
    // `multiplicity / (beta - fingerprint(id, tuple...))` to `acc`
    let folded = sub_ext(beta, fingerprint_with_id(id, tuple, alpha));
    let folded_next = next_ext(folded);

    let m_ext = from_base(multiplicity);
    let m_ext_next = next_ext(m_ext);

    let is_first_next = from_base(is_first');

    
    // Update rule:
    // fingerprint_with_id(id, (a1', a2', ...), alpha) * (acc' - acc * (1 - is_first')) - multiplicity' = 0
    let update_expr = sub_ext(
        mul_ext(folded_next, sub_ext(next_acc, mul_ext(acc_ext, sub_ext(from_base(1), is_first_next)))), m_ext_next
    );

    let (acc_1, acc_2) = unpack_ext(acc_ext);
    let (expr_1, expr_2) = unpack_ext(update_expr);
    
    constrain_eq_ext(update_expr, from_base(0))
};

let compute_next_z_send: expr, expr, expr[], expr, Fp2<expr>, Fp2<expr>, Fp2<expr> -> fe[] = query |is_first, id, tuple, multiplicity, acc, alpha, beta| {
    // Implemented as: folded = (beta - fingerprint(id, tuple...));
    // `multiplicity / (beta - fingerprint(id, tuple...))` to `acc`
    let folded = sub_ext(beta, fingerprint_with_id(id, tuple, alpha));
    let folded_next = next_ext(folded);

    let m_ext = from_base(multiplicity);
    let m_ext_next = next_ext(m_ext);

    let is_first_next = from_base(is_first');
    
    // acc' = acc * (1 - is_first') + multiplicity / fingerprint_with_id(id, (a1', a2'))
    let res = add_ext(
        mul_ext(eval_ext(acc), eval_ext(sub_ext(from_base(1), is_first_next))),
        mul_ext(eval_ext(m_ext_next), inv_ext(eval_ext(folded_next)))
    );

    match res {
        Fp2::Fp2(a0_fe, a1_fe) => [a0_fe, a1_fe]
    }
};

let compute_next_z_receive: expr, expr, expr[], expr, Fp2<expr>, Fp2<expr>, Fp2<expr> -> fe[] = query |is_first, id, tuple, multiplicity, acc, alpha, beta| {
    // Implemented as: folded = (beta - fingerprint(id, tuple...));
    // `multiplicity / (beta - fingerprint(id, tuple...))` to `acc`
    let folded = sub_ext(beta, fingerprint_with_id(id, tuple, alpha));
    let folded_next = next_ext(folded);

    let m_ext = from_base(multiplicity);
    let m_ext_next = next_ext(m_ext);

    let is_first_next = from_base(is_first');
    
    // acc' = acc * (1 - is_first') - multiplicity / fingerprint_with_id(id, (a1', a2'))
    let res = sub_ext(
        mul_ext(eval_ext(acc), eval_ext(sub_ext(from_base(1), is_first_next))),
        mul_ext(eval_ext(m_ext_next), inv_ext(eval_ext(folded_next)))
    );

    match res {
        Fp2::Fp2(a0_fe, a1_fe) => [a0_fe, a1_fe]
    }
};

// Convenience functions
let bus_send: expr, expr, expr[], expr, expr[], Fp2<expr>, Fp2<expr> -> Constr[] = |is_first, id, tuple, multiplicity, acc, alpha, beta| {
    bus_interaction(is_first, id, tuple, multiplicity, acc, alpha, beta)
};

let bus_receive: expr, expr, expr[], expr, expr[], Fp2<expr>, Fp2<expr> -> Constr[] = |is_first, id, tuple, multiplicity, acc, alpha, beta| {
    bus_interaction(is_first, id, tuple, -1 * multiplicity, acc, alpha, beta)
};