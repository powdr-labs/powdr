/// A function that evaluates an algebraic expression on the current row.
/// Since this needs the concept of a "current row", it is only
/// valid in query functions.
let eval: expr -> fe = [];

/// Returns the value of the algebraic expression on the current row if it is
/// known, otherwise returns None.
/// This function is only valid in query functions.
let try_eval: expr -> Option<fe> = [];

/// A function that can be used to set a cell value.
/// The parameters are column, row index and value.
/// It is an error to provide a value that is different to one already provided or determined.
/// This function is only valid in query functions.
let provide_value: expr, int, fe -> () = [];

/// Constructs a challenge object.
/// The arguments are the proof stage and the id of the challenge, in this order.
let challenge: int, int -> expr = constr |st, id| std::prelude::challenge(st, id);

/// Returns the current number of rows, sometimes known as the "degree".
let degree: -> int = [];

/// Asserts that the current degree or row count is at least m.
let require_min_degree: int -> () = |m| std::check::assert(degree() >= m, || "Degree too small.");

/// Asserts that the current degree or row count is at most m;
let require_max_degree: int -> () = |m| std::check::assert(degree() <= m, || "Degree too large.");