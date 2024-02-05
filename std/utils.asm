
/// Evaluates to folder(...folder(folder(initial, f(0)), f(1)) ..., f(length - 1)),
/// i.e. calls f(0), f(1), ..., f(length - 1) and combines the results
/// using the function `folder`, starting with the value `initial`.
///
/// See `sum` for an example use.
// TODO the type checker originally inferred
// int, (int -> T1), T2, (T3, T1 -> T2) -> T2
// i.e. it seems the recursive call uses a different type scheme instantiation.
// This might be OK depending on the situation, but does it even work that T3 != T2?
let<T1, T2>
    fold: int, (int -> T1), T2, (T2, T1 -> T2) -> T2 = |length, f, initial, folder|
        if length <= 0 {
            initial
        } else {
            folder(fold((length - 1), f, initial, folder), f((length - 1)))
        };

/// Evaluates to f(0) + f(1) + ... + f(length - 1).
let<T: Add + FromLiteral> sum: int, (int -> T) -> T = |length, f| fold(length, f, 0, |acc, e| (acc + e));

// TODO what happens if we drop the FromLiteral requirement? Would it type check? Would it just add the requirement to the uses of 'sum'?

/// Evaluates to a constraint that forces the witness column `c` to stay constant
/// until `latch` is 1. In the row following the row where `latch` is 1,
/// `c` is allowed to change.
///
/// Note: `latch` needs to be equal to `1`, and not merely non-zero for `c` to be able to change.
let unchanged_until: expr, expr -> constr = |c, latch| (c' - c) * (1 - latch) = 0;

/// Evaluates to a constraint that forces `c` to be either 0 or 1.
let force_bool: expr -> constr = |c| c * (1 - c) = 0;
