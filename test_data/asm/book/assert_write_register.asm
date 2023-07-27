machine Machine {

    degree 8;

    // ANCHOR: component
reg pc[@pc];
reg A;

instr assert_A_is_zero {
    A = 0
}

function main {
    assert_A_is_zero;
    loop;
}
    // ANCHOR_END: component

    instr loop {
        pc' = pc
    }
}