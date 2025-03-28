use std::math::fp2::Fp2;
use std::math::extension_field::Ext;
use std::math::extension_field::unpack_ext_array;
use std::math::extension_field::constrain_eq_ext;
use std::prover::challenge;
use std::protocols::fingerprint::fingerprint;
use std::protocols::fingerprint::fingerprint_inter;
use std::array;
use std::convert::expr;
use std::prover::eval;

machine Main with degree: 2048 {

    col witness x(i) query Query::Hint(42);

    // Fold tuple [x, x + 1, ..., x + n - 1]
    // Note that, by setting a fairly large `n`, we test that performance is not exponential in `n`.
    let n = 100;
    let tuple = array::new(n, |i| x + 1);

    // Add `fingerprint_value` witness columns and constrain them using `fingerprint_inter`
    col witness stage(1) fingerprint_value0, fingerprint_value1;
    let fingerprint_value = Ext::Fp2(Fp2::Fp2(fingerprint_value0, fingerprint_value1));
    let alpha = Ext::Fp2(Fp2::Fp2(challenge(0, 0), challenge(0, 1)));
    constrain_eq_ext(fingerprint_inter(tuple, alpha), fingerprint_value);

    // Add `fingerprint_value_hint` witness columns and compute the fingerprint in a hint using `fingerprint`
    let fingerprint_hint: -> fe[] = query || {
        let tuple_eval = array::new(array::len(tuple), |i| eval(tuple[i]));
        unpack_ext_array(fingerprint(tuple_eval, alpha))
    };

    col witness stage(1) fingerprint_value0_hint(i) query Query::Hint(fingerprint_hint()[0]);
    col witness stage(1) fingerprint_value1_hint(i) query Query::Hint(fingerprint_hint()[1]);

    // Assert consistency between `fingerprint` and `fingerprint_inter`
    // We compare squares to avoid the linear constraint being optimized away, which currently leads to an issue because `fingerprint_value0_hint` has a hint
    fingerprint_value0 * fingerprint_value0 = fingerprint_value0_hint * fingerprint_value0_hint;
    fingerprint_value1 * fingerprint_value1 = fingerprint_value1_hint * fingerprint_value1_hint;

}