use std::convert::to_col;

// Very simple write-once memory, but without an mstore operation.
// As a result, this only works if the content of the `v` column has
// been provided externally.
machine MemReadWrite {

    degree 256;

    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg A;

    // Write-once memory
    let ADDR: col = to_col(|i| i);
    let v;
    // Loads a value. If the cell is empty, the prover can choose a value.
    instr mload X -> Y { {X, Y} in {ADDR, v} }

    instr assert_eq X, Y { X = Y }

    function main {
        A <== mload(17);
        assert_eq A, 42;

        A <== mload(62);
        assert_eq A, 123;

        A <== mload(255);
        assert_eq A, -1;

        return;
    }
}