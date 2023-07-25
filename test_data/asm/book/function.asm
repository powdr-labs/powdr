/* ANCHOR: all */

machine Machine {

    degree 256;

    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg CNT;
    reg A;

    // an instruction to assert that a number is zero
    instr assert_zero X {
        X = 0
    }

    // an instruction to jump to a label
    instr jmp l: label {
        pc' = l 
    }

    // an instruction to jump to a label iff `X` is `0`, otherwise continue
    instr jmpz X, l: label { 
        pc' = XIsZero * l + (1 - XIsZero) * (pc + 1) 
    }
    
    // an instruction to return the square of an input
    // ANCHOR: square
    instr square X -> Y {
        Y = X * X
    }
    // ANCHOR_END: square

    function main {
        // initialise `A` to 2
        A <=X= 2;
        // initialise `CNT` to `3`
        // ANCHOR: literals
        CNT <=X= 3;
        // ANCHOR_END: literals
        // ANCHOR: label
        start::
        // ANCHOR_END: label
        // if `CNT` is `0`, jump to `end`
        jmpz CNT, end;
        // decrement `CNT`
        // ANCHOR: read_register
        CNT <=X= CNT - 1;
        // ANCHOR_END: read_register
        // square `A`
        // ANCHOR: instruction
        A <=Y= square(A);
        // ANCHOR_END: instruction
        // jump back to `start`
        jmp start;
        end::
        // check that `A == ((2**2)**2)**2`
        // ANCHOR: instruction_statement
        assert_zero A - ((2**2)**2)**2;
        // ANCHOR_END: instruction_statement
        // loop forever
        loop;
    }

    // an instruction to loop forever
    instr loop {
        pc' = pc
    }

    // some superpowers on `X` to allow us to check if it's 0
    constraints {
        col witness XInv;
        col witness XIsZero;
        XIsZero  = 1 - X * XInv;
        XIsZero * X = 0;
        XIsZero * (1 - XIsZero) = 0;
    }
}

/* ANCHOR_END: all */