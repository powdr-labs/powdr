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

machine Main with degree: 8 {
    reg pc[@pc]; // "@pc" means "pc' = pc + 1" is the default propagation (instead of pc' = pc) and it tracks the line in the program.
    reg X[<=]; // "<=" means it is the default assignment register.
    reg A;
    reg CNT;

    col witness XInv;
    col witness XIsZero;
    // assume X = 7
    // XisZero * 7 = 0 -> XIzZero = 0
    // 0 = 1 - 7 * XInv
    // => XInv = 1/7 (finite field)
    XIsZero  = 1 - X * XInv;
    XIsZero * X = 0;
    XIsZero * (1 - XIsZero) = 0;

    instr jmpz X, l: label { pc' = XIsZero * l + (1 - XIsZero) * (pc + 1) }
    instr jmp l: label { pc' = l }
    instr dec_CNT { CNT' = CNT - 1 }
    instr assert_zero X { XIsZero = 1 }

    function main {
        CNT <=X= ${ Query::Input(0, 2) };

        start:
        jmpz CNT, check;
        A <=X= A + ${ Query::Input(0, std::convert::int(std::prover::eval(CNT) + 2)) };
        // Could use "CNT <=X= CNT - 1", but that would need X.
        dec_CNT;
        jmp start;

        check:
        A <=X= A - ${ Query::Input(0, 1) };
        assert_zero A;
        return;
    }
}
