/// A function that evaluates an algebraic expression on the current row.
/// Since this needs the concept of a "current row", it is only
/// valid in query functions.
let eval: expr -> fe = [];

/// Constructs a challenge object.
/// The arguments are the proof stage and the id of the challenge, in this order.
let challenge: int, int -> expr = constr |st, id| std::prelude::challenge(st, id);

/// Returns the minimum number of rows in this namespace, sometimes known as the minimum "degree".
let min_degree: -> int = [];
/// Returns the maximum number of rows in this namespace, sometimes known as the maximum "degree".
let max_degree: -> int = [];
/// Returns the number of rows in this namespace, sometimes known as the "degree". Fails if the minimum and maximum degree are not equal.
let degree: -> int = [];

/// Asserts that the current degree or row count is at least m.
let require_min_degree: int -> () = |m| std::check::assert(degree() >= m, || "Degree too small.");

/// Asserts that the current degree or row count is at most m;
let require_max_degree: int -> () = |m| std::check::assert(degree() <= m, || "Degree too large.");