/// A function that evaluates an algebraic expression on the current row.
/// Since this needs the concept of a "current row", it is only
/// valid in query functions.
let eval: expr -> fe = [];

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

/// Constructs a challenge object.
/// The arguments are the proof stage and the id of the challenge, in this order.
let challenge: int, int -> expr = constr |st, id| std::prelude::challenge(st, id);

/// Returns the current number of rows, sometimes known as the "degree".
let degree: -> int = [];

/// Asserts that the current degree or row count is at least m.
let require_min_degree: int -> () = |m| std::check::assert(degree() >= m, || "Degree too small.");

/// Asserts that the current degree or row count is at most m;
let require_max_degree: int -> () = |m| std::check::assert(degree() <= m, || "Degree too large.");

/// Get two phase-2 challenges to use in all permutation/lookup arguments.
/// Note that this assumes that globally no other challenge of these IDs is used,
/// and that challenges for multiple permutation/lookup arguments are re-used.
/// We declare two components for each challenge here, in case we need to operate
/// on the extension field. If we don't, we won't end up needing it and the optimizer
/// will remove it.
let alpha1: expr = challenge(0, 1);
let alpha2: expr = challenge(0, 2);

let beta1: expr = challenge(0, 3);
let beta2: expr = challenge(0, 4);