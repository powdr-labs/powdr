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
let assert_eq: (int -> int), int[] -> () = |fun, expected| {
    let _ = std::array::new(degree(), |i| assert(fun(i) == expected[i], || "test failed"));
};

machine Main with degree: 8 {
    // let F0 = [1, 2, 3]* + [1] + [1, 2, 3, 1];
    let r0 = expand([repeat([1, 2, 3]), once([1]), once([1, 2, 3, 1])]);
    assert_eq(r0, [1, 2, 3, 1, 1, 2, 3, 1]);

    // let F1 = [1, 2]*;
    let r1 = expand([repeat([1, 2])]);
    assert_eq(r1, [1, 2, 1, 2, 1, 2, 1, 2]);

    // // let F2 = [1, 2] + [1, 2, 1, 2, 1, 2];
    let r2 = expand([once([1, 2]), once([1, 2, 1, 2, 1, 2])]);
    assert_eq(r2, [1, 2, 1, 2, 1, 2, 1, 2]);

    // let F3 = []* + [1, 2, 1, 2, 1, 2, 3, 4];
    let r3 = expand([repeat([]), once([1, 2, 1, 2, 1, 2, 3, 4])]);
    assert_eq(r3, [1, 2, 1, 2, 1, 2, 3, 4]);

    // let F4 = []* + [1, 2, 1, 2] + [1]*;
    let r4 = expand([once([1, 2, 1, 2]), repeat([1])]);
    assert_eq(r4, [1, 2, 1, 2, 1, 1, 1, 1]);

    // let F5 = [1]* + [1]*; // should panic
    let r5 = expand([repeat([1]), repeat([1])]);
    // TODO: how to test this?
}
