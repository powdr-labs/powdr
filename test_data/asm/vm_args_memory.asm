use std::machines::memory::Memory;
use std::machines::range::Byte2;

let N: int = 256;

machine Main with degree: N {
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg A;

    col fixed STEP(i) { i };
    Byte2 byte2;
    Memory memory(byte2);
    WithArg sub(memory);

    instr mload X -> Y link ~> Y = memory.mload(X, STEP);
    instr mstore X, Y -> link ~> memory.mstore(X, STEP, Y);

    instr get X -> Y link => Y = sub.get(X, STEP);
    instr put X, Y -> link => sub.put(X, STEP, Y);

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

machine WithArg(mem: Memory) with degree: N {
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg A;
    reg B;

    col fixed STEP(i) { i };

    instr mload X, Y -> Z link ~> Z = mem.mload(X, Y);
    instr mstore X, Y, Z -> link ~> mem.mstore(X, Y, Z);

    function get a, step -> b {
        // Read value from memory
        A <== mload(a, step);

        return A;
    }

    function put a, step, b {
        // Store value into memory
        mstore a, step, b;
        return;
    }
}
