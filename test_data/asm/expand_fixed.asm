use std::prover::degree;
use std::check::assert;

// A term in an array expression
enum ArrayTerm {
    Repeat(int[]),
    Once(int[])
}

// returns the total size of the repeated array in this array expression
let solve: ArrayTerm[] -> int = |terms| {
    let (_, total_size_of_non_repeated) = std::array::fold(terms, (false, 0), |(found_repeated, len), term| {
        match term {
            // we can have `[]*` as many times as we want
            ArrayTerm::Repeat([]) => (found_repeated, len),
            ArrayTerm::Repeat(a) => {
                if found_repeated {
                    std::check::panic("unsolvable because two terms are repeated")
                } else {
                    (true, len)
                }
            },
            ArrayTerm::Once(a) => (found_repeated, len + std::array::len(a))
        }
    });
    std::check::assert(total_size_of_non_repeated <= degree(), || "non repeated array terms do not fit in degree");
    degree() - total_size_of_non_repeated
};

// returns a function representing the array expression
let expand: ArrayTerm[] -> (int -> int) = |terms| {
    // get the total size of the repeated term
    let size_of_repeated = solve(terms);
    |i| {
        let (_, res) = std::array::fold(terms, (0, 0), |(offset, res), term| {
            let (a, len) = match term {
                ArrayTerm::Repeat(a) => (a, size_of_repeated),
                ArrayTerm::Once(a) => (a, std::array::len(a))
            };

            let index = i - offset;

            (
                offset + len,
                if 0 <= index && index < len {
                    res + a[index % std::array::len(a)]
                } else {
                    res
                }
            )
        });
        res
    }
};

let repeat: int[] -> ArrayTerm = |a| ArrayTerm::Repeat(a);

let once: int[] -> ArrayTerm = |a| ArrayTerm::Once(a);

// test utility
let assert_eq: expr, expr -> () = constr |left, right| {
    let diff;
    diff = left - right;
    diff = 0;
};

machine Main with degree: 8 {
    // let F0 = [1, 2, 3]* + [1] + [1, 2, 3, 1];
    let r0 = expand([repeat([1, 2, 3]), once([1]), once([1, 2, 3, 1])]);
    col fixed res0(i) { r0(i) };
    col fixed expected0 = [1, 2, 3, 1, 1, 2, 3, 1];
    assert_eq(res0, expected0);

    // let F1 = [1, 2]*;
    let r1 = expand([repeat([1, 2])]);
    col fixed res1(i) { r1(i) };
    col fixed expected1 = [1, 2, 1, 2, 1, 2, 1, 2];
    assert_eq(res1, expected1);

    // // let F2 = [1, 2] + [1, 2, 1, 2, 1, 2];
    let r2 = expand([once([1, 2]), once([1, 2, 1, 2, 1, 2])]);
    col fixed res2(i) { r2(i) };
    col fixed expected2 = [1, 2, 1, 2, 1, 2, 1, 2];
    assert_eq(res2, expected2);

    // let F3 = []* + [1, 2, 1, 2, 1, 2, 3, 4];
    let r3 = expand([repeat([]), once([1, 2, 1, 2, 1, 2, 3, 4])]);
    col fixed res3(i) { r3(i) };
    col fixed expected3 = [1, 2, 1, 2, 1, 2, 3, 4];
    assert_eq(res3, expected3);

    // let F4 = []* + [1, 2, 1, 2] + [1]*;
    let r4 = expand([once([1, 2, 1, 2]), repeat([1])]);
    col fixed res4(i) { r4(i) };
    col fixed expected4 = [1, 2, 1, 2, 1, 1, 1, 1];
    assert_eq(res4, expected4);

    // let F5 = [1]* + [1]*; // should panic
    let r5 = expand([repeat([1]), repeat([1])]);
    // col fixed res5(i) { r5(i) }; // uncomment this line to panic   
}
