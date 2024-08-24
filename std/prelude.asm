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
    Lookup((Option<expr>, Option<expr>), (expr, expr)[]),
    /// A permutation constraint with selectors.
    Permutation((Option<expr>, Option<expr>), (expr, expr)[]),
    /// A connection constraint (copy constraint).
    Connection((expr, expr)[])
}


/// The return type of a prover query function.
enum Query {
    /// Query a prover input element by index.
    Input(int),
    /// Writes a byte (second argument) to a file descriptor (first argument).
    /// fd 1 is stdout, fd 2 is stderr.
    Output(int, int),
    /// Generate a hint to fill a witness column with.
    Hint(fe),
    /// Query a prover input element by index and data id.
    DataIdentifier(int, int),
    /// This value is not (additionally) constrained by the query.
    None,
}

/// Adds a hint / query function to an existing witness column.
let set_hint: expr, (int -> std::prelude::Query) -> () = [];

/// Constructs a challenge object.
/// The arguments are the proof stage and the id of the challenge, in this order.
/// This is a built-in function.
let challenge: int, int -> expr = [];
