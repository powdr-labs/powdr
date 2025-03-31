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

enum BusInteraction {
    // id, payload, multiplicity
    // For bus sends, the multiplicity always equals the latch
    Send(expr, expr[], expr),
    // id, payload, multiplicity, latch
    Receive(expr, expr[], expr, expr)
}

/// Helper function.
/// Materialized as a witness column for two reasons:
/// - It makes sure the constraint degree is independent of the input payload.
/// - We can access folded', even if the payload contains next references.
/// Note that if all expressions are degree-1 and there is no next reference,
/// this is wasteful, but we can't check that here.
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

/// Helper function.
/// Implemented as: folded = (beta - fingerprint(id, payload...));
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
/// # Arguments are plural for multiple bus interactions.
/// For each bus interaction:
/// - id: Interaction Id
/// - payload: An array of expressions to be sent to the bus
/// - multiplicity: The multiplicity which shows how many times a column will be sent
/// - latch: a binary expression which indicates where the multiplicity can be non-zero.
let bus_multi_interaction: expr[], expr[][], expr[], expr[] -> () = constr |ids, payloads, multiplicities, latches| {
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
    
    // In cases where there are odd number of bus interactions, the last bus interaction doesn't need helper column.
    // Instead, we have `update_expr` + multiplicity_last' / folded_last' = 0
    // Or equivalently:
    // `update_expr` * folded_last' + multiplicity_last' = 0
    let update_expr_final = if input_len % 2 == 1 {
        // Odd number of bus interactions
        add_ext(
            mul_ext(
                update_expr,
                folded_next_arr[input_len - 1]
            ),
            m_ext_next_arr[input_len - 1]
        )
    } else {
        // Even number of bus interactions
        update_expr
    };

    // Constrain the accumulator update identity
    constrain_eq_ext(update_expr_final, from_base(0));
            
    // Add array of phantom bus interactions
    array::new(
        input_len,
        |i| if input_len % 2 == 1 && i == input_len - 1 {
            Constr::PhantomBusInteraction(
                multiplicities[i], 
                ids[i], 
                payloads[i], 
                latches[i], 
                unpack_ext_array(folded_arr[i]), 
                acc, 
                Option::None
            )
        } else {
            Constr::PhantomBusInteraction(
                multiplicities[i], 
                ids[i], 
                payloads[i], 
                latches[i], 
                unpack_ext_array(folded_arr[i]), 
                acc, 
                Option::Some(helper_arr[i / 2])
            )
        }
    );

    // Add array of native bus interactions
    array::new(
        input_len,
        |i| Constr::BusInteraction(
            multiplicities[i], 
            ids[i], 
            payloads[i], 
            latches[i]
        )
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

/// Helper function.
/// Transpose user interface friendly `BusInteraction`
/// to constraint building friendly `expr[], expr[][], expr[], expr[]`, i.e. id, payload, multiplicity, latch.
/// This is because Rust-style tuple indexing, e.g. tuple.0, isn't supported yet.
let transpose_bus_inputs: BusInteraction[] -> (expr[], expr[][], expr[], expr[]) = |bus_inputs| {
    array::fold(
        bus_inputs, ([], [], [], []), 
        |(ids, payloads, multiplicities, latches), bus_input| {
            match bus_input {
                BusInteraction::Send(id, payload, multiplicity) => 
                    (ids + [id], payloads + [payload], multiplicities + [multiplicity], latches + [multiplicity]),
                BusInteraction::Receive(id, payload, multiplicity, latch) => 
                    (ids + [id], payloads + [payload], multiplicities + [-multiplicity], latches + [latch]),
            }
        }
    )
};

/// Convenience function for batching multiple bus interactions.
/// Transposes user inputs and then calls the key logic for batch building bus interactions.
/// Can batch both send and receive.
let bus_multi: BusInteraction[] -> () = constr |bus_inputs| {
    let (ids, payloads, multiplicities, latches) = transpose_bus_inputs(bus_inputs);
    bus_multi_interaction(ids, payloads, multiplicities, latches);
};

/// Input parameter only used in `bus_multi_linker`, not intended for end user.
enum BusLinkerType {
    Send,
    LookupReceive,
    PermutationReceive
}

/// Exposed to the linker and not intended for end user because it requires expert knowledge of Powdr's bus protocol.
/// Inputs in the order of: id, selector, payload, type for both lookup and permutation.
/// where type can be: bus send, lookup bus receive, or permutation bus receive.
let bus_multi_linker: (expr, expr, expr[], BusLinkerType)[] -> () = constr |inputs| {
    // Lookup requires adding a multiplicity column and constraining it to zero if selector is zero.
    // Permutation passes the selector to both multiplicity and latch fields as well.
    let inputs_inner = array::fold(inputs, [], constr |acc, input| {
        // Converted to input format for the inner function `bus_multi`:
        // For lookup bus receive, format is id, payload, multiplicity, selector
        // For permutation bus receive, format is id, payload, selector, selector
        let (id, selector, payload, type) = input;
        match type {
            BusLinkerType::Send => acc + [BusInteraction::Send(id, payload, selector)],
            BusLinkerType::LookupReceive => {
                let multiplicity;
                (1 - selector) * multiplicity = 0;
                acc + [BusInteraction::Receive(id, payload, multiplicity, selector)]
            },
            BusLinkerType::PermutationReceive => acc + [BusInteraction::Receive(id, payload, selector, selector)],
        }
    });
    bus_multi(inputs_inner);
};

/// Builds a single bus interaction by using `bus_multi_interaction` for optimized performance.
/// This is for user's convenience to supply inputs directly rather than a vector of tuples of inputs in the multi version.
let bus_interaction: expr, expr[], expr, expr -> () = constr |id, payload, multiplicity, latch| {
    bus_multi_interaction([id], [payload], [multiplicity], [latch]);
};

/// Convenience function for single bus interaction to send/receive columns
let bus: BusInteraction -> () = constr |bus_input| {
    match bus_input {
        // For bus sends, the multiplicity always equals the latch
        BusInteraction::Send(id, payload, multiplicity) => bus_interaction(id, payload, multiplicity, multiplicity),
        BusInteraction::Receive(id, payload, multiplicity, latch) => bus_interaction(id, payload, -multiplicity, latch)
    }
};

