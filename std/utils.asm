
/// Evaluates to folder(...folder(folder(initial, f(0)), f(1)) ..., f(length - 1)),
/// i.e. calls f(0), f(1), ..., f(length - 1) and combines the results
/// using the function `folder`, starting with the value `initial`.
///
/// See `sum` for an example use.
let fold = |length, f, initial, folder|
    if length <= 0 {
        initial
    } else {
        folder(fold((length - 1), f, initial, folder), f((length - 1)))
    };

/// Evaluates to f(0) + f(1) + ... + f(length - 1).
let sum = |length, f| fold(length, f, 0, |acc, e| (acc + e));

/// Evaluates to a constraint that forces the witness column `c` to stay constant
/// until `latch` is 1. In the row following the row where `latch` is 1,
/// `c` is allowed to change.
///
/// Note: `latch` needs to be equal to `1`, and not merely non-zero for `c` to be able to change.
let unchanged_until = |c, latch| (c' - c) * (1 - latch) == 0;

/// Evaluates to a constraint that forces `c` to be either 0 or 1.
let force_bool = [|c| c * (1 - c) == 0][0];