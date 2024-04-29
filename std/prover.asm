/// A function that evaluates an algebraic expression on the current row.
/// Since this needs the concept of a "current row", it is only
/// valid in query functions.
let eval: expr -> fe = [];

/// The return type of a prover query function.
enum Query {
    /// Query a prover input element by index.
    Input(int),
    /// Print a character on stdout.
    PrintChar(int),
    /// Generate a hint to fill a witness column with.
    Hint(fe),
    /// Query a prover input element by index and data id.
    DataIdentifier(int, int),
    /// This value is not (additionally) constrained by the query.
    Unconstrained,
}

/// Constructs a challenge object.
/// The arguments are the proof stage and the id of the challenge, in this order.
let challenge: int, int -> expr = [];
