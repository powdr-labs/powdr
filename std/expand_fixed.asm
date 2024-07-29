use std::result::Result;
use std::check::panic;

/// A term in an array expression
enum ArrayTerm {
    Repeat(int[]),
    Once(int[])
}

// returns the total size of the repeated array in this array expression
let compute_length_of_repeated_part: ArrayTerm[], (-> int) -> Result<int, string> = |terms, degree| {
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
let expand: ArrayTerm[], (-> int) -> Result<(int -> int), string> = |terms, degree| {
    // return early if all terms are constant with the same value
    let constant_value = std::array::fold(terms, Result::Ok(Option::None), |r, term| {
        let a = match term {
            ArrayTerm::Once(a) => a,
            ArrayTerm::Repeat(a) => a,
        };

        match r {
            Result::Ok(c) => {
                std::array::fold(a, Result::Ok(c), |e, value| {
                    match e {
                        Result::Ok(Option::None) => Result::Ok(Option::Some(value)),
                        Result::Ok(Option::Some(prev_value)) => if value == prev_value { Result::Ok(Option::Some(value)) } else { Result::Err(()) },
                        Result::Err(()) => Result::Err(())
                    }
                })
            },
            Result::Err(()) => Result::Err(())
        }
    });

    match constant_value {
        // all values are the same. This is not strict enough and would allow expressions whose size does not match the degree
        // we should fix this by detecting multiple repeated parts earlier, and check the minimum size of the expanded expression
        // is smaller than the minimum degree of the machine
        Result::Ok(Option::Some(v)) => Result::Ok(|_| v),
        _ => {
            // get the total size of the repeated term
            match compute_length_of_repeated_part(terms, degree) {
                Result::Ok(size_of_repeated) => {
                    Result::Ok(|i| {
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
                    })
                },
                Result::Err(e) => Result::Err(e)
            }
        }
    }
};

let expand_unwrapped: ArrayTerm[], (-> int) -> (int -> int) = |terms, degree| {
    match expand(terms, degree) {
        Result::Ok(r) => r,
        Result::Err(e) => panic(e)
    }
};

let repeat: int[] -> ArrayTerm = |a| ArrayTerm::Repeat(a);

let once: int[] -> ArrayTerm = |a| ArrayTerm::Once(a);