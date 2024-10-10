// Verfies that a sum in the input has been computed properly.
// Input: sum, cnt, x_1, x_2, ..., x_cnt
//         ?, 4, 1, 2, 3, 4
// This input is assumed to be present in a minirust variable called "input"
// of type "Vec<FieldElement>"

// Code in `${`...`}` is rust-like code that is run by the prover
// to generate free inputs.

// This imports the `conv` module which contains functions to convert
// between integers and field elements.
use std::convert::int;

use std::prelude::Query;

machine Main with degree: 16 {
    reg pc[@pc]; // "@pc" means "pc' = pc + 1" is the default propagation (instead of pc' = pc) and it tracks the line in the program.

    function main {

        return;
    }
}
