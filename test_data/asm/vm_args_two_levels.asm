use std::machines::memory::Memory;

machine Main with degree: 262144 {
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg A;

    col fixed STEP(i) { i };

    Memory memory;
    Child sub(memory);

    instr mload X -> Y ~ memory.mload X, STEP -> Y;
    instr mstore X, Y -> ~ memory.mstore X, STEP, Y ->;

    // TODO: changing these to permutation passes witgen but fails proving
    instr child_mload X -> Y = sub.mload X, STEP -> Y;
    instr child_mstore X, Y -> = sub.mstore X, STEP, Y ->;
    instr grandchild_mload X -> Y = sub.child_mload X, STEP -> Y;
    instr grandchild_mstore X, Y -> = sub.child_mstore X, STEP, Y ->;

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

        // Read previously stored value (from child)
        A <== child_mload(100);
        assert_eq A, 4;

        // Read previously stored value (from grandchild)
        A <== grandchild_mload(100);
        assert_eq A, 4;

        // Read uninitialized memory (from child)
        A <== child_mload(8);
        assert_eq A, 0;

        // Read uninitialized memory (from grandchild)
        A <== grandchild_mload(8);
        assert_eq A, 0;

        // Write to memory (from child)
        child_mstore 8, 123;

        // Read it back (from grandchild)
        A <== grandchild_mload(8);
        assert_eq A, 123;

        // Write to memory (from grandchild)
        grandchild_mstore 12, 457;

        // read it back (from child)
        A <== child_mload(12);
        assert_eq A, 457;

        return;
    }
}

machine Child(mem: Memory) {
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg A;
    reg B;

    GrandChild sub(mem);

    instr mload X, Y -> Z ~ mem.mload;
    instr mstore X, Y, Z -> ~ mem.mstore;

    instr child_mload X, Y -> Z = sub.mload;
    instr child_mstore X, Y, Z -> = sub.mstore;

    function mload a, step -> b {
        A <== mload(a, step);
        return A;
    }

    function mstore a, step, b {
        mstore a, step, b;
        return;
    }

    function child_mload a, step -> b {
        A <== child_mload(a, step);
        return A;
    }

    function child_mstore a, step, b {
        child_mstore a, step, b;
        return;
    }
}

machine GrandChild(mem: Memory) {
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg A;
    reg B;

    instr mload X, Y -> Z ~ mem.mload X, Y -> Z;
    instr mstore X, Y, Z -> ~ mem.mstore X, Y, Z ->;

    function mload a, step -> b {
        A <== mload(a, step);
        return A;
    }

    function mstore a, step, b {
        mstore a, step, b;
        return;
    }
}
