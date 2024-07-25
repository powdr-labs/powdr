use std::result::Result;

// A term in an array expression
enum ArrayTerm {
    Repeat(int[]),
    Once(int[])
}

// returns the total size of the repeated array in this array expression
let solve: ArrayTerm[], int -> Result<int, string> = |terms, degree| {
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
        Result::Ok(total_size_of_non_repeated) => if total_size_of_non_repeated <= degree {
            Result::Ok(degree - total_size_of_non_repeated)
        } else {
            Result::Err("non repeated array terms do not fit in degree")
        },
        Result::Err(e) => Result::Err(e)
    }
};

// returns a function representing the array expression
let expand: ArrayTerm[], int -> Result<(int -> int), string> = |terms, degree| {
    // get the total size of the repeated term
    match solve(terms, degree) {
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