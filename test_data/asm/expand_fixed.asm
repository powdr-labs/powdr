use std::prover::degree;
use std::check::assert;

// A term in an array expression
enum ArrayTerm {
    Repeat(int[]),
    Once(int[])
}

enum Result<T, E> {
    Ok(T),
    Err(E)
}

// returns the total size of the repeated array in this array expression
let solve: ArrayTerm[] -> Result<int, string> = |terms| {
    let (_, res) = std::array::fold(terms, (false, Result::Ok(0)), |(found_repeated, l), term| {
        match l {
            Result::Err(e) => (found_repeated, Result::Err(e)),
            Result::Ok(len) => match term {
                // we can have `[]*` as many times as we want
                ArrayTerm::Repeat([]) => (found_repeated, Result::Ok(len)),
                ArrayTerm::Repeat(a) => {
                    if found_repeated {
                        (true, Result::Err("unsolvable because two terms are repeated"))
                    } else {
                        (true, Result::Ok(len))
                    }
                },
                ArrayTerm::Once(a) => (found_repeated, Result::Ok(len + std::array::len(a)))
            }
        }
    });
    match res {
        Result::Ok(total_size_of_non_repeated) => if total_size_of_non_repeated <= degree() {
            Result::Ok(degree() - total_size_of_non_repeated)
        } else {
            Result::Err("non repeated array terms do not fit in degree")
        },
        Result::Err(e) => Result::Err(e)
    }
};

// returns a function representing the array expression
let expand: ArrayTerm[] -> Result<(int -> int), string> = |terms| {
    // get the total size of the repeated term
    match solve(terms) {
        Result::Ok(size_of_repeated) => Result::Ok(|i| {
            let (_, res) = std::array::fold(terms, (0, 0), |(offset, res), term| {
                let (a, len) = match term {
                    ArrayTerm::Repeat([]) => ([], 0),
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
        }),
        Result::Err(e) => Result::Err(e)
    }
};

let repeat: int[] -> ArrayTerm = |a| ArrayTerm::Repeat(a);

let once: int[] -> ArrayTerm = |a| ArrayTerm::Once(a);

// test utility
let assert_eq: Result<(int -> int), string>, Result<int[], string> -> () = |v, expected| {
    match (v, expected) {
        (Result::Err(e1), Result::Err(e2)) => assert(e1 == e2, || "error doesn't match"),
        (Result::Ok(fun), Result::Ok(a)) => {
            let _ = std::array::new(degree(), |i| assert(fun(i) == a[i], || "test failed"));
        },
        _ => std::check::panic("")
    }
};

machine Main with degree: 8 {
    // let F0 = [1, 2, 3]* + [1] + [1, 2, 3, 1];
    let r0 = expand([repeat([1, 2, 3]), once([1]), once([1, 2, 3, 1])]);
    assert_eq(r0, Result::Ok([1, 2, 3, 1, 1, 2, 3, 1]));

    // let F1 = [1, 2]*;
    let r1 = expand([repeat([1, 2])]);
    assert_eq(r1, Result::Ok([1, 2, 1, 2, 1, 2, 1, 2]));

    // // let F2 = [1, 2] + [1, 2, 1, 2, 1, 2];
    let r2 = expand([once([1, 2]), once([1, 2, 1, 2, 1, 2])]);
    assert_eq(r2, Result::Ok([1, 2, 1, 2, 1, 2, 1, 2]));

    // let F3 = []* + [1, 2, 1, 2, 1, 2, 3, 4];
    let r3 = expand([repeat([]), once([1, 2, 1, 2, 1, 2, 3, 4])]);
    assert_eq(r3, Result::Ok([1, 2, 1, 2, 1, 2, 3, 4]));

    // let F4 = []* + [1, 2, 1, 2] + [1]*;
    let r4 = expand([repeat([]), once([1, 2, 1, 2]), repeat([1])]);
    assert_eq(r4, Result::Ok([1, 2, 1, 2, 1, 1, 1, 1]));

    // let F5 = [1]* + [1]*; // should panic
    let r5 = expand([repeat([1]), repeat([1])]);
    assert_eq(r5, Result::Err("unsolvable because two terms are repeated"));

    // let F6 = [1, 2, 3, 4] + [1, 2, 3, 4] + [1]; // should panic
    let r6 = expand([once([1, 2, 3, 4]), once([1, 2, 3, 4]), once([1])]);
    assert_eq(r6, Result::Err("non repeated array terms do not fit in degree"));

    // let F7 = [1, 2, 3, 4] + [1, 2, 3, 4] + [1]*; // should succeed as we can repeat `[1]` zero times
    let r7 = expand([once([1, 2, 3, 4]), once([1, 2, 3, 4]), repeat([1])]);
    assert_eq(r7, Result::Ok([1, 2, 3, 4, 1, 2, 3, 4]));
}
