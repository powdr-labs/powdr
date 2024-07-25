use std::expand_fixed::expand;
use std::expand_fixed::once;
use std::expand_fixed::repeat;
use std::result::Result;
use std::check::assert;

// test utility
let assert_eq: Result<(int -> int), string>, Result<int[], string> -> () = |v, expected| {
    match (v, expected) {
        (Result::Err(e1), Result::Err(e2)) => assert(e1 == e2, || "error doesn't match"),
        (Result::Ok(fun), Result::Ok(a)) => {
            let _ = std::array::new(std::prover::degree(), |i| assert(fun(i) == a[i], || "test failed"));
        },
        _ => std::check::panic("")
    }
};

machine Main with degree: 8 {

    let degree = std::prover::degree();

    // let F0 = [1, 2, 3]* + [1] + [1, 2, 3, 1];
    let r0 = expand([repeat([1, 2, 3]), once([1]), once([1, 2, 3, 1])], degree);
    assert_eq(r0, Result::Ok([1, 2, 3, 1, 1, 2, 3, 1]));

    // let F1 = [1, 2]*;
    let r1 = expand([repeat([1, 2])], degree);
    assert_eq(r1, Result::Ok([1, 2, 1, 2, 1, 2, 1, 2]));

    // // let F2 = [1, 2] + [1, 2, 1, 2, 1, 2];
    let r2 = expand([once([1, 2]), once([1, 2, 1, 2, 1, 2])], degree);
    assert_eq(r2, Result::Ok([1, 2, 1, 2, 1, 2, 1, 2]));

    // let F3 = []* + [1, 2, 1, 2, 1, 2, 3, 4];
    let r3 = expand([repeat([]), once([1, 2, 1, 2, 1, 2, 3, 4])], degree);
    assert_eq(r3, Result::Ok([1, 2, 1, 2, 1, 2, 3, 4]));

    // let F4 = []* + [1, 2, 1, 2] + [1]*;
    let r4 = expand([repeat([]), once([1, 2, 1, 2]), repeat([1])], degree);
    assert_eq(r4, Result::Ok([1, 2, 1, 2, 1, 1, 1, 1]));

    // let F5 = [1]* + [1]*; // should panic
    let r5 = expand([repeat([1]), repeat([1])], degree);
    assert_eq(r5, Result::Err("unsolvable because two terms are repeated"));

    // let F6 = [1, 2, 3, 4] + [1, 2, 3, 4] + [1]; // should panic
    let r6 = expand([once([1, 2, 3, 4]), once([1, 2, 3, 4]), once([1])], degree);
    assert_eq(r6, Result::Err("non repeated array terms do not fit in degree"));

    // let F7 = [1, 2, 3, 4] + [1, 2, 3, 4] + [1]*; // should succeed as we can repeat `[1]` zero times
    let r7 = expand([once([1, 2, 3, 4]), once([1, 2, 3, 4]), repeat([1])], degree);
    assert_eq(r7, Result::Ok([1, 2, 3, 4, 1, 2, 3, 4]));
}
