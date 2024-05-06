use std::machines::memory::Memory;

machine Main with degree: 262144 {
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg A;

    col fixed STEP(i) { i };
    Memory memory;
    WithArg sub(memory);

    instr mload X -> Y ~ memory.mload X, STEP -> Y;
    instr mstore X, Y -> ~ memory.mstore X, STEP, Y ->;

    // TODO: changing these to permutation passes witgen but fails proving
    instr get X -> Y = sub.get;
    instr put X, Y -> = sub.put;

    instr assert_eq X, Y { X = Y }

    function main {
        // Store 4
        mstore 100, 4;

        // Read uninitialized memory
        A <== mload(104);
        assert_eq A, 0;

        // Read previously stored value
        A <== mload(100);
        assert_eq A, 4;

        // Read previously stored value in submachine
        A <== get(100);
        assert_eq A, 4;

        // Read uninitialized memory in submachine
        A <== get(8);
        assert_eq A, 0;

        // Write to memory in submachine
        put 8, 123;

        // Read it back
        A <== mload(8);
        assert_eq A, 123;
        A <== get(8);
        assert_eq A, 123;

        return;
    }
}

machine WithArg(mem: Memory) {
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg A;
    reg B;

    col fixed STEP(i) { i };

    instr mload X -> Y ~ mem.mload X, STEP -> Y;
    instr mstore X, Y -> ~ mem.mstore X, STEP, Y ->;

    function get a -> b {
        // Read value from memory
        A <== mload(a);

        return A;
    }

    function put a, b {
        // Store value into memory
        mstore a, b;
        return;
    }
}
