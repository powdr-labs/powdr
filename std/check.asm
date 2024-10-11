/// This is a built-in function taking a string argument and terminating
/// evaluation unsuccessfully with this argument as explanation.
/// This symbol is not an empty array, the actual semantics are overridden.
let panic: string -> ! = [];

/// Checks the condition and panics if it is false.
/// IMPORTANT: Since this does not generate any constraints, the verifier will not
/// check these assertions. This function should only be used to verify
/// prover-internal consistency.
/// The panic message is obtained by calling the function `reason`.
/// Returns an empty array on success, which allows it to be used at statement level.
let assert: bool, (-> string) -> () = |condition, reason| if !condition { panic(reason()) } else { () };

/// Checks that the used field covers enough bits.
/// This is useful to ensure the soundness of different machines written for different fields.
let require_field_bits = |b, err_f| assert(std::field::modulus() > 2**b, err_f);
