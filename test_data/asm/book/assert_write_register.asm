machine Main with degree: 8 {
    // ANCHOR: component
reg pc[@pc];
reg A;

instr assert_A_is_zero {
    A = 0
}

function main {
    assert_A_is_zero;
    return;
}
    // ANCHOR_END: component
}
