
/// Evaluates to folder(...folder(folder(initial, f(0)), f(1)) ..., f(length - 1)),
/// i.e. calls f(0), f(1), ..., f(length - 1) and combines the results
/// using the function `folder`, starting with the value `initial`.
///
/// See `sum` for an example use.
let<T1, T2>
    fold: int, (int -> T1), T2, (T2, T1 -> T2) -> T2 = |length, f, initial, folder|
        if length <= 0 {
            initial
        } else {
            folder(fold((length - 1), f, initial, folder), f((length - 1)))
        };

/// Evaluates to f(0) + f(1) + ... + f(length - 1).
let<T: Add + FromLiteral> sum: int, (int -> T) -> T = |length, f| fold(length, f, 0, |acc, e| (acc + e));

/// Evaluates to a constraint that forces the witness column `c` to stay constant
/// until `latch` is 1. In the row following the row where `latch` is 1,
/// `c` is allowed to change.
///
/// Note: `latch` needs to be equal to `1`, and not merely non-zero for `c` to be able to change.
let unchanged_until: expr, expr -> constr = |c, latch| (c' - c) * (1 - latch) = 0;

/// Evaluates to a constraint that forces `c` to be either 0 or 1.
let force_bool: expr -> constr = |c| c * (1 - c) = 0;

/// Returns an array of functions such that the range of the `i`th function is exactly the
/// first `size[i]` numbers (i.e. `0` until `size[i] - 1`, inclusive), such that all combinations
/// of values of these functions appear as combined outputs.
/// Each of the functions cycles through its values, advancing to the next number whenever the
/// previous function has completed a cycle (or always advancing if it is the first function).
/// This function is useful for combined range checks or building the inputs for function
/// that is implemented in a lookup.
/// See binary.asm for an example.
let cross_product: int[] -> (int -> int)[] = |sizes| cross_product_internal(1, 0, sizes);

let cross_product_internal: int, int, int[] -> (int -> int)[] = |cycle_len, pos, sizes|
    if pos >= std::array::len(sizes) {
        // We could assert here that the degree is at least `cycle_len`
        []
    } else {
        [|i| (i / cycle_len) % sizes[pos]] +
            cross_product_internal(cycle_len * sizes[pos], pos + 1, sizes)
    };
