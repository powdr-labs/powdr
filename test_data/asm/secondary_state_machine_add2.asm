reg pc[@pc];
reg X[<=];
reg Y[<=];
reg A;
reg B;


pil{
    // Add 2 to a number by adding 1 two times
    col fixed add_two_RESET(i) { match i % 3 { 2 => 1, _ => 0 } };

    col witness add_two_state;
    col witness add_two_input;

    // Because constraints are not cyclic, we need to explicitly constrain the first state
    first_step * (add_two_state - add_two_input) = 0;

    // If RESET is true, constrain the next state to be equal to the input
    // if RESET is false, increment the current state
    add_two_state' = (1 - add_two_RESET) * (add_two_state + 1) + add_two_RESET * add_two_input';

    // If RESET is true, the next input is unconstrained
    // If RESET is false, the next input is equal to the current input
    0 = (1 - add_two_RESET) * (add_two_input - add_two_input');
}

instr add2 Y -> X {
    {Y, X} in add_two_RESET { add_two_input, add_two_state }
}

instr loop { pc' = pc }

A <=X= 0;
B <=X= add2(A);

A <=X= 1;
B <=X= add2(A);

A <=X= 0xf5;
B <=X= add2(A);

loop;