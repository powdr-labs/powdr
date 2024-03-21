use std::memory::Memory;

machine Main {
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg A;

    degree 65536;

    col fixed STEP(i) { i };
    Memory memory;

    instr mload X -> Y ~ memory.mload X, STEP -> Y;
    instr mstore X, Y -> ~ memory.mstore X, STEP, Y ->;

    instr assert_eq X, Y {
        X = Y
    }

    function main {

        mstore 100, 4;
        
        A <== mload(104);
        assert_eq A, 0;

        A <== mload(100);
        assert_eq A, 4;

        mstore 100, 8;

        A <== mload(100);
        assert_eq A, 8;

        return;
    }
}