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

// Helper function.
// Materialized as a witness column for two reasons:
// - It makes sure the constraint degree is independent of the input payload.
// - We can access folded', even if the payload contains next references.
// Note that if all expressions are degree-1 and there is no next reference,
// this is wasteful, but we can't check that here.
let materialize_folded: -> bool = || match known_field() {
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

// Helper function.
// Implemented as: folded = (beta - fingerprint(id, payload...));
let create_folded: expr, expr[], Ext<expr>, Ext<expr> -> Ext<expr> = constr |id, payload, alpha, beta| 
    if materialize_folded() {
        let folded = from_array(
            array::new(required_extension_size(),
                    |i| std::prover::new_witness_col_at_stage("folded", 1))
        );
        constrain_eq_ext(folded, sub_ext(beta, fingerprint_with_id_inter(id, payload, alpha)));
        folded
    } else {
        sub_ext(beta, fingerprint_with_id_inter(id, payload, alpha))
    };

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
let bus_multi_interaction_2: expr[], expr[][], expr[], expr[] -> () = constr |ids, payloads, multiplicities, latches| {
    // Check length of inputs
    let input_len: int = array::len(ids);
    assert(input_len == array::len(payloads), || "inputs ids and payloads have unequal lengths");
    assert(input_len == array::len(multiplicities), || "inputs ids and multiplicities have unequal lengths");
    assert(input_len == array::len(latches), || "inputs ids and latches have unequal lengths");

    let extension_field_size = required_extension_size();

    // Alpha is used to compress the LHS and RHS arrays.
    let alpha = from_array(array::new(extension_field_size, |i| challenge(0, i + 1)));
    // Beta is used to update the accumulator.
    let beta = from_array(array::new(extension_field_size, |i| challenge(0, i + 1 + extension_field_size)));

    // Create folded columns.
    let folded_arr = array::new(input_len, |i| create_folded(ids[i], payloads[i], alpha, beta)); // Ext<expr>[]
    let folded_next_arr = array::map(folded_arr, |folded| next_ext(folded)); // Ext<expr>[]

    let m_ext_arr = array::map(multiplicities, |multiplicity| from_base(multiplicity)); // Ext<expr>[]
    let m_ext_next_arr = array::map(m_ext_arr, |m_ext| next_ext(m_ext)); // Ext<expr>[]

    let acc = array::new(extension_field_size, |i| std::prover::new_witness_col_at_stage("acc", 1));
    let acc_ext = from_array(acc);
    let next_acc = next_ext(acc_ext);

    let is_first: col = std::well_known::is_first;
    let is_first_next = from_base(is_first');

    // Create helper columns to bound degree to 3 for arbitrary number of bus interactions.
    // Each helper processes two bus interactions:
    // helper_i = multiplicity_{2*i} / folded_{2*i} + multiplicity_{2*i+1} / folded_{2*i+1}
    // Or equivalently when expanded:
    // folded_{2*i} * folded_{2*i+1}' * helper_i - folded_{2*i+1} * multiplicity_{2*i} - folded_{2*i} * multiplicity_{2*i+1} = 0
    let helper_arr: expr[][] = array::new(
        input_len / 2,
        |helper|
        array::new(
            extension_field_size,
            |column| std::prover::new_witness_col_at_stage("helper", 1)
        )
    );
    let helper_ext_arr = array::map( // Ext<expr>[] (somehow type annotating this will cause a symbol not found error in analyzer)
        helper_arr,
        |helper| from_array(helper)
    );
    let helper_ext_next_arr = array::map(
        helper_ext_arr,
        |helper_ext| next_ext(helper_ext) 
    );
    // The expression to constrain.
    let helper_expr_arr = array::new( // Ext<expr>[]
        input_len / 2,
        |i| sub_ext(
            sub_ext(
                mul_ext(
                    mul_ext(folded_arr[2 * i], folded_arr[2 * i + 1]),
                    helper_ext_arr[i]
                ),
                mul_ext(folded_arr[2 * i + 1], m_ext_arr[2 * i])
            ),
            mul_ext(folded_arr[2 * i], m_ext_arr[2 * i + 1])
        )
    );
    // Return a flattened array of constraints. (Must use `array::fold` or the compiler won't allow nested Constr[][].)
    array::fold(helper_expr_arr, [], |init, helper_expr| constrain_eq_ext(helper_expr, from_base(0)));
    
    // Update rule:
    // acc' =  acc * (1 - is_first') + helper_0' + helper_1' + ...
    // Add up all helper columns.
    // Or equivalently:
    // acc * (1 - is_first') + helper_0' + helper_1' + ... - acc' = 0
    let update_expr = 
        sub_ext(
            add_ext(
                mul_ext(
                    acc_ext, 
                    sub_ext(from_base(1), is_first_next)
                ),
                // Sum of all helper columns.
                array::fold(helper_ext_next_arr, from_base(0), |sum, helper_ext_next| add_ext(sum, helper_ext_next))
            ),
            next_acc
        );
    constrain_eq_ext(update_expr, from_base(0));
            
    // Add array of phantom bus interactions
    array::new(
        input_len,
        |i| Constr::PhantomBusInteraction(
            multiplicities[i], 
            ids[i], 
            payloads[i], 
            latches[i], 
            unpack_ext_array(folded_arr[i]), 
            acc, 
            helper_arr[i / 2])
    );
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

/// Convenience function for batching two bus sends.
let bus_multi_send_2: expr[], expr[][], expr[] -> () = constr |ids, payloads, multiplicities| {
    // For bus sends, the multiplicity always equals the latch
    bus_multi_interaction_2(ids, payloads, multiplicities, multiplicities);
};

/// Convenience function for batching two bus receives.
/// In practice, can also batch one bus send and one bus receive, but requires knowledge of this function and careful configuration of input parameters.
/// E.g. sending negative multiplicity and multiplicity for "multiplicity" and "latch" parameters for bus sends.
let bus_multi_receive_2: expr[], expr[][], expr[], expr[] -> () = constr |ids, payloads, multiplicities, latches| {
    let negated_multiplicities: expr[] = array::map(multiplicities, |multiplicity| -multiplicity);
    bus_multi_interaction_2(ids, payloads, negated_multiplicities, latches);
};

let bus_interaction: expr, expr[], expr, expr -> () = constr |id, payload, multiplicity, latch| {};
let bus_multi_interaction: expr, expr[], expr, expr, expr, expr[], expr, expr -> () = constr |id_0, payload_0, multiplicity_0, latch_0, id_1, payload_1, multiplicity_1, latch_1| {};