use std::check::assert;
use std::array;
use std::math::extension_field::Ext;
use std::math::extension_field::add_ext;
use std::math::extension_field::sub_ext;
use std::math::extension_field::mul_ext;
use std::math::extension_field::inv_ext;
use std::math::extension_field::eval_ext;
use std::math::extension_field::unpack_ext_array;
use std::math::extension_field::next_ext;
use std::math::extension_field::from_base;
use std::math::extension_field::from_array;
use std::math::extension_field::constrain_eq_ext;
use std::protocols::fingerprint::fingerprint_with_id;
use std::protocols::fingerprint::fingerprint_with_id_inter;
use std::math::extension_field::required_extension_size;
use std::prover::eval;
use std::field::known_field;
use std::field::KnownField;
use std::check::panic;

/// Sends the payload (id, payload...) to the bus by adding
/// `multiplicity / (beta - fingerprint(id, payload...))` to `acc`
/// It is the callers responsibility to properly constrain the multiplicity (e.g. constrain
/// it to be boolean) if needed.
///
/// # Arguments:
/// - id: Interaction Id
/// - payload: An array of expressions to be sent to the bus
/// - multiplicity: The multiplicity which shows how many times a column will be sent
/// - latch: a binary expression which indicates where the multiplicity can be non-zero.
let bus_interaction: expr, expr[], expr, expr -> () = constr |id, payload, multiplicity, latch| {

    let extension_field_size = required_extension_size();

    // Alpha is used to compress the LHS and RHS arrays.
    let alpha = from_array(array::new(extension_field_size, |i| challenge(0, i + 1)));
    // Beta is used to update the accumulator.
    let beta = from_array(array::new(extension_field_size, |i| challenge(0, i + 1 + extension_field_size)));

    // Implemented as: folded = (beta - fingerprint(id, payload...));
    let materialize_folded = match known_field() {
        // Materialized as a witness column for two reasons:
        // - It makes sure the constraint degree is independent of the input payload.
        // - We can access folded', even if the payload contains next references.
        // Note that if all expressions are degree-1 and there is no next reference,
        // this is wasteful, but we can't check that here.
        Option::Some(KnownField::Goldilocks) => true,
        Option::Some(KnownField::BabyBear) => true,
        Option::Some(KnownField::KoalaBear) => true,
        Option::Some(KnownField::M31) => true,
        // The case above triggers our hand-written witness generation, but on Bn254, we'd not be
        // on the extension field and use the automatic witness generation.
        // However, it does not work with a materialized folded payload. At the same time, Halo2
        // (the only prover that supports BN254) does not have a hard degree bound. So, we can
        // in-line the expression here.
        Option::Some(KnownField::BN254) => false,
        _ => panic("Unexpected field!")
    };
    let folded = if materialize_folded {
        let folded = from_array(
            array::new(extension_field_size,
                    |i| std::prover::new_witness_col_at_stage("folded", 1))
        );
        constrain_eq_ext(folded, sub_ext(beta, fingerprint_with_id_inter(id, payload, alpha)));
        folded
    } else {
        sub_ext(beta, fingerprint_with_id_inter(id, payload, alpha))
    };

    let folded_next = next_ext(folded);

    let m_ext = from_base(multiplicity);
    let m_ext_next = next_ext(m_ext);

    let acc = array::new(extension_field_size, |i| std::prover::new_witness_col_at_stage("acc", 1));
    let acc_ext = from_array(acc);
    let next_acc = next_ext(acc_ext);

    let is_first: col = std::well_known::is_first;
    let is_first_next = from_base(is_first');

    // Update rule:
    // acc' =  acc * (1 - is_first') + multiplicity' / folded'
    // or equivalently:
    // folded' * (acc' - acc * (1 - is_first')) - multiplicity' = 0
    let update_expr = sub_ext(
        mul_ext(folded_next, sub_ext(next_acc, mul_ext(acc_ext, sub_ext(from_base(1), is_first_next)))), m_ext_next
    );
    
    constrain_eq_ext(update_expr, from_base(0));

    // Add phantom bus interaction
    Constr::PhantomBusInteraction(multiplicity, id, payload, latch, acc);
};

/// Multi version of `bus_interaction`.
/// Batches two bus interactions.
/// Requires a prove system constraint degree bound of 4 or more.
/// In practice, saves `acc` and `is_first` columns and rotated columns thereof.
let bus_multi_interaction: expr, expr[], expr, expr, expr, expr[], expr, expr -> () = constr |id_0, payload_0, multiplicity_0, latch_0, id_1, payload_1, multiplicity_1, latch_1| {

    let extension_field_size = required_extension_size();

    // Deriving two alphas and two betas is mathematically equivalent to the non-multi version.
    // Alpha is used to compress the LHS and RHS arrays.
    let alpha_0 = from_array(array::new(extension_field_size, |i| challenge(0, i + 1)));
    let alpha_1 = from_array(array::new(extension_field_size, |i| challenge(0, i + 1 + extension_field_size)));
    // Beta is used to update the accumulator.
    let beta_0 = from_array(array::new(extension_field_size, |i| challenge(0, i + 1 + extension_field_size * 2)));
    let beta_1 = from_array(array::new(extension_field_size, |i| challenge(0, i + 1 + extension_field_size * 3)));

    // Implemented as: folded = (beta - fingerprint(id, payload...));
    let materialize_folded = match known_field() {
        // Materialized as a witness column for two reasons:
        // - It makes sure the constraint degree is independent of the input payload.
        // - We can access folded', even if the payload contains next references.
        // Note that if all expressions are degree-1 and there is no next reference,
        // this is wasteful, but we can't check that here.
        Option::Some(KnownField::Goldilocks) => true,
        Option::Some(KnownField::BabyBear) => true,
        Option::Some(KnownField::KoalaBear) => true,
        Option::Some(KnownField::M31) => true,
        // The case above triggers our hand-written witness generation, but on Bn254, we'd not be
        // on the extension field and use the automatic witness generation.
        // However, it does not work with a materialized folded payload. At the same time, Halo2
        // (the only prover that supports BN254) does not have a hard degree bound. So, we can
        // in-line the expression here.
        Option::Some(KnownField::BN254) => false,
        _ => panic("Unexpected field!")
    };
    let folded_0 = if materialize_folded {
        let folded_0 = from_array(
            array::new(extension_field_size,
                    |i| std::prover::new_witness_col_at_stage("folded_0", 1))
        );
        constrain_eq_ext(folded_0, sub_ext(beta_0, fingerprint_with_id_inter(id_0, payload_0, alpha_0)));
        folded_0
    } else {
        sub_ext(beta_0, fingerprint_with_id_inter(id_0, payload_0, alpha_0))
    };
    let folded_1 = if materialize_folded {
        let folded_1 = from_array(
            array::new(extension_field_size,
                    |i| std::prover::new_witness_col_at_stage("folded_1", 1))
        );
        constrain_eq_ext(folded_1, sub_ext(beta_1, fingerprint_with_id_inter(id_1, payload_1, alpha_1)));
        folded_1
    } else {
        sub_ext(beta_1, fingerprint_with_id_inter(id_1, payload_1, alpha_1))
    };

    let folded_next_0 = next_ext(folded_0);
    let folded_next_1 = next_ext(folded_1);

    let m_ext_0 = from_base(multiplicity_0);
    let m_ext_1 = from_base(multiplicity_1);

    let m_ext_next_0 = next_ext(m_ext_0);
    let m_ext_next_1 = next_ext(m_ext_1);

    let acc = array::new(extension_field_size, |i| std::prover::new_witness_col_at_stage("acc", 1));
    let acc_ext = from_array(acc);
    let next_acc = next_ext(acc_ext);

    let is_first: col = std::well_known::is_first;
    let is_first_next = from_base(is_first');

    // Update rule:
    // acc' =  acc * (1 - is_first') + multiplicity_0' / folded_0' + multiplicity_1' / folded_1'
    // or equivalently:
    // folded_0' * folded_1' * (acc' - acc * (1 - is_first')) - multiplicity_0' * folded_1' - multiplicity_1' * folded_0' = 0
    let update_expr = sub_ext(
        sub_ext(
            mul_ext(
                mul_ext(folded_next_0, folded_next_1),
                sub_ext(next_acc, mul_ext(acc_ext, sub_ext(from_base(1), is_first_next)))
            ),
            mul_ext(m_ext_next_0, folded_next_1)
        ),
        mul_ext(m_ext_next_1, folded_next_0)
    );
    
    constrain_eq_ext(update_expr, from_base(0));

    // Add phantom bus interaction
    Constr::PhantomBusInteraction(multiplicity_0, id_0, payload_0, latch_0, unpack_ext_array(folded_0), acc);
    Constr::PhantomBusInteraction(multiplicity_1, id_1, payload_1, latch_1, unpack_ext_array(folded_1), acc);
};

/// Compute acc' = acc * (1 - is_first') + multiplicity' / fingerprint(id, payload...),
/// using extension field arithmetic.
/// This is intended to be used as a hint in the extension field case; for the base case
/// automatic witgen is smart enough to figure out the value of the accumulator.
let compute_next_z: expr, expr, expr[], expr, Ext<expr>, Ext<expr>, Ext<expr> -> fe[] = query |is_first, id, payload, multiplicity, acc, alpha, beta| {

    let m_next = eval(multiplicity');
    let m_ext_next = from_base(m_next);

    let is_first_next = eval(is_first');
    let current_acc = if is_first_next == 1 {from_base(0)} else {eval_ext(acc)};
    
    // acc' = current_acc + multiplicity' / folded'
    let res = if m_next == 0 {
        current_acc
    } else {
        // Implemented as: folded = (beta - fingerprint(id, payload...));
        // `multiplicity / (beta - fingerprint(id, payload...))` to `acc`
        let folded_next = sub_ext(eval_ext(beta), fingerprint_with_id(eval(id'), array::eval(array::next(payload)), alpha));
        add_ext(
            current_acc,
            mul_ext(m_ext_next, inv_ext(folded_next))
        )
    };

    unpack_ext_array(res)
};

/// Convenience function for bus interaction to send columns
let bus_send: expr, expr[], expr -> () = constr |id, payload, multiplicity| {
    // For bus sends, the multiplicity always equals the latch
    bus_interaction(id, payload, multiplicity, multiplicity);
};

/// Convenience function for bus interaction to receive columns
let bus_receive: expr, expr[], expr, expr -> () = constr |id, payload, multiplicity, latch| {
    bus_interaction(id, payload, -multiplicity, latch);
};

/// Convenience function for batching two bus sends.
let bus_multi_send: expr, expr[], expr, expr, expr[], expr -> () = constr |id_0, payload_0, multiplicity_0, id_1, payload_1, multiplicity_1| {
    // For bus sends, the multiplicity always equals the latch
    bus_multi_interaction(id_0, payload_0, multiplicity_0, multiplicity_0, id_1, payload_1, multiplicity_1, multiplicity_1);
};

/// Convenience function for batching two bus receives.
/// In practice, can also batch one bus send and one bus receive, but requires knowledge of this function and careful configuration of input parameters.
/// E.g. sending negative multiplicity and multiplicity for "multiplicity" and "latch" parameters for bus sends.
let bus_multi_receive: expr, expr[], expr, expr, expr, expr[], expr, expr -> () = constr |id_0, payload_0, multiplicity_0, latch_0, id_1, payload_1, multiplicity_1, latch_1| {
    bus_multi_interaction(id_0, payload_0, -multiplicity_0, latch_0, id_1, payload_1, -multiplicity_1, latch_1);
};
