
machine Main {
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg A;
    reg B;


    constraints {
        // Add a block state machine that adds 2 to a number by adding 1 two times
        col fixed add_two_RESET(i) { match i % 3 { 2 => 1, _ => 0 } };

        // State is initialized with the input and incremented by 1 in each step
        col witness add_two_state;
        // The input column needs to be constant for the entire block
        col witness add_two_input;

        // Because constraints are not cyclic, we need to explicitly constrain the first state
        first_step * (add_two_state - add_two_input) = 0;

        // Add %offset in a single step of computation
        constant %offset = 1;

        // If RESET is true, constrain the next state to be equal to the input
        // if RESET is false, increment the current state
        add_two_state' = (1 - add_two_RESET) * (add_two_state + %offset) + add_two_RESET * add_two_input';

        // If RESET is true, the next input is unconstrained
        // If RESET is false, the next input is equal to the current input
        // add_two_input' = (1 - add_two_RESET) * add_two_input;
        0 = (1 - add_two_RESET) * (add_two_input - add_two_input');
    }

    instr add2 Y -> X {
        {Y, X} in add_two_RESET { add_two_input, add_two_state }
    }
    instr loop { pc' = pc }

    function main {
        A <=X= 0;
        B <=X= add2(A);

        A <=X= 1;
        B <=X= add2(A);

        A <=X= 0xf5;
        B <=X= add2(A);

        loop;
    }
}