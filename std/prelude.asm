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

/// The "constraint" type, i.e. the result of the operators
/// "=", "in", "is" and "connect".
enum Constr {
    /// A polynomial identity, result of the "=" operator.
    Identity(expr, expr),
    /// A lookup constraint with selectors, result of the "in" operator.
    Lookup((Option<expr>, Option<expr>), (expr, expr)[]),
    /// A permutation constraint with selectors, result of the "is" operator.
    Permutation((Option<expr>, Option<expr>), (expr, expr)[]),
    /// A connection constraint (copy constraint), result of the "connect" operator.
    Connection((expr, expr)[])
}

/// This is the result of the "$" operator. It can be used as the left and
/// right hand side of a lookup or permutation constraint.
enum SelectedExprs {
    SelectedExprs(expr, expr[]),
    JustExprs(expr[]),
}


/// The return type of a prover query function.
enum Query {
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
