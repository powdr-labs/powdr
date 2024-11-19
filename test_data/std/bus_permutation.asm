use std::protocols::permutation_via_bus::permutation_send;
use std::protocols::permutation_via_bus::permutation_receive;

machine Main with degree: 8 {

    col fixed x = [0, 0, 1, 2, 3, 4, 5, 6];
    col witness y;

    // A small block machine that computes f(x) = x + 42;
    col witness sub_x, sub_y, sub_sel;
    sub_y = sub_x + 42;

    // Currently, witgen fails if the block machine has just enough rows to
    // fit all the blocks, so let's not have a call in the last row.
    col fixed sel = [1, 1, 1, 1, 1, 1, 1, 0];

    // Add the bus permutation constraints
    let ID = 123;
    let permutation_constraint = sel $ [x, y] is sub_sel $ [sub_x, sub_y];
    permutation_send(ID, permutation_constraint);
    permutation_receive(ID, permutation_constraint);
}