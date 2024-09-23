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

/// Provide a value to a column only if it has not been set yet.
let provide_if_unknown: expr, int, (-> fe) -> () = query |column, row, f| match try_eval(column) {
    Option::None => provide_value(column, row, f()),
    _ => (),
};

/// Retrieves a byte from a prover-provided (untrusted and not committed) input channel.
/// The parameters are the index of the channel and the index in the channel.
/// Index zero is the length of the channel (number of bytes) and index 1 is the first byte.
let get_input_from_channel: int, int -> fe = [];

/// Outputs a byte to a file descriptor.
let output_byte: int, int -> () = [];

let handle_query: expr, int, std::prelude::Query -> () = query |column, row, v| match v {
    Query::Hint(h) => provide_if_unknown(column, row, || h),
    Query::Input(i, j) => provide_if_unknown(column, row, || get_input_from_channel(i, j)),
    Query::Output(fd, b) => provide_if_unknown(column, row, || { output_byte(fd, b); 0 }),
    Query::None => (),
};

/// Constructs a challenge object.
/// The arguments are the proof stage and the id of the challenge, in this order.
let challenge: int, int -> expr = constr |st, id| std::prelude::challenge(st, id);

/// Creates a witness column at a given stage.
/// The first argument is a name suggestion, but it will be made unique.
/// The second argument is the stage.
let new_witness_col_at_stage: string, int -> expr = [];

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
