use std::machines::range::Byte2;
use std::machines::memory_bb::Memory;

machine Main with degree: 65536 {
    reg pc[@pc];
    reg X1[<=];
    reg X2[<=];
    reg Y1[<=];
    reg Y2[<=];
    reg A1;
    reg A2;

    col fixed STEP(i) { i };
    Byte2 byte2;
    Memory memory(byte2);

    instr mload X1, X2 -> Y1, Y2 link ~> (Y1, Y2) = memory.mload(X1, X2, STEP);
    instr mstore X1, X2, Y1, Y2 -> link ~> memory.mstore(X1, X2, STEP, Y1, Y2);

    instr assert_eq X1, X2, Y1, Y2 {
        X1 = Y1,
        X2 = Y2
    }

    function main {

        // Store 4
        mstore 100, 0, 4, 0;
        
        // Read uninitialized memory
        A1, A2 <== mload(104, 0);
        assert_eq A1, A2, 0, 0;

        // Read previously stored value
        A1, A2 <== mload(100, 0);
        assert_eq A1, A2, 4, 0;

        // Update previously stored value
        mstore 100, 0, 7, 0;
        mstore 100, 0, 8, 0;

        // Read updated values (twice)
        A1, A2 <== mload(100, 0);
        assert_eq A1, A2, 8, 0;
        A1, A2 <== mload(100, 0);
        assert_eq A1, A2, 8, 0;

        // Write to previously uninitialized memory cell
        mstore 104, 0, 1234, 0;
        A1, A2 <== mload(104, 0);
        assert_eq A1, A2, 1234, 0;

        // Write max value
        mstore 200, 0, 0xffff, 0xffff;
        A1, A2 <== mload(200, 0);
        assert_eq A1, A2, 0xffff, 0xffff;

        // Store at maximal address
        mstore 0xffff, 0xfffc, 1, 0;
        A1, A2 <== mload(0xffff, 0xfffc);
        assert_eq A1, A2, 1, 0;

        return;
    }
}
