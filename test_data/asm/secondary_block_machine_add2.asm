machine Main {
    degree 16;

    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg A;
    reg B;

    // Add a block state machine that adds 2 to a number by adding 1 two times
    col fixed add_two_RESET = [0, 0, 1]*;
    col fixed LAST = [0]* + [1];
    col fixed RESET = [0, 0, 1]* + [1];

    // State is initialized with the input and incremented by 1 in each step
    col witness add_two_state;
    // The input column needs to be constant for the entire block
    col witness add_two_input;

    col fixed _first_step = [1] + [0]*;
    // Because constraints are not cyclic, we need to explicitly constrain the first state
    _first_step * (add_two_state - add_two_input) = 0;

    // Add offset in a single step of computation
    let offset = 1;

    // If RESET is true, constrain the next state to be equal to the input
    // if RESET is false, increment the current state
    add_two_state' = (1 - RESET) * (add_two_state + offset) + RESET * add_two_input';

    // If RESET is true, the next input is unconstrained
    // If RESET is false, the next input is equal to the current input
    0 = (1 - RESET) * (add_two_input - add_two_input');

    instr add2 Y -> X {
        {Y, X} in add_two_RESET { add_two_input, add_two_state }
    }

    function main {
        A <=X= 0;
        B <=X= add2(A);

        A <=X= 1;
        B <=X= add2(A);

        A <=X= 0xf5;
        B <=X= add2(A);

        return;
    }
}