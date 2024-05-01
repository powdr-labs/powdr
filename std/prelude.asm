/// Like in Rust, all symbols from this file are available in all modules,
/// meaning that a failed symbol resolution always re-tries relative to this module.

use std::convert::expr as to_expr;
use std::convert::fe as to_fe;
use std::convert::int as to_int;

let true: bool = "" == "";
let false: bool = !true;

enum Option<T> {
    None,
    Some(T)
}

/// The "constraint" type.
enum Constr {
    /// A polynomial identity.
    Identity(expr, expr),
    /// A lookup constraint with selectors.
    Lookup(Option<expr>, expr[], Option<expr>, expr[]),
    /// A permutation constraint with selectors.
    Permutation(Option<expr>, expr[], Option<expr>, expr[]),
    /// A connection constraint (copy constraint).
    Connection(expr[], expr[])
}