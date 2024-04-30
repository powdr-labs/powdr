/// Like in Rust, all symbols from this file are available in all modules,
/// meaning that a failed symbol resolution always re-tries relative to this module.

/// The Option type.
use std::utils::Option;

use std::convert::expr as to_expr;
use std::convert::fe as to_fe;
use std::convert::int as to_int;

let true: bool = "" == "";
let false: bool = !true;