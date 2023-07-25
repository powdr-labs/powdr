machine Machine {

    degree 8;

    // ANCHOR: component
reg pc[@pc];
reg X[<=];
reg A;    

instr assert_zero X {
    X = 0
}

function main {
    assert_zero A;
    loop;
}
    // ANCHOR_END: component

    instr loop {
        pc' = pc
    }
}