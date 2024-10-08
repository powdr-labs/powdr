use std::machines::range::Bit12;
use std::machines::range::Byte2;
use std::machines::memory16::Memory16;

machine Main with degree: 65536 {
    reg pc[@pc];
    reg X1[<=];
    reg X2[<=];
    reg Y1[<=];
    reg Y2[<=];
    reg A1;
    reg A2;

    col fixed STEP(i) { i };
    Bit12 bit12;
    Byte2 byte2;
    Memory16 memory(bit12, byte2);

    instr mload X1, X2 -> Y1, Y2 link ~> (Y1, Y2) = memory.mload(X1, X2, STEP);
    instr mstore X1, X2, Y1, Y2 -> link ~> memory.mstore(X1, X2, STEP, Y1, Y2);

    instr assert_eq X1, Y1 {
        X1 = Y1
    }

    function main {

        // Store 4
        mstore 0, 100, 0, 4;
        
        // Read uninitialized memory
        A1, A2 <== mload(1, 100);
        assert_eq A1, 0;
        assert_eq A2, 0;

        // Read previously stored value
        A1, A2 <== mload(0, 100);
        assert_eq A1, 0;
        assert_eq A2, 4;

        // Update previously stored value
        mstore 0, 100, 0, 7;
        mstore 0, 100, 0, 8;

        // Read updated values (twice)
        A1, A2 <== mload(0, 100);
        assert_eq A1, 0;
        assert_eq A2, 8;
        A1, A2 <== mload(0, 100);
        assert_eq A1, 0;
        assert_eq A2, 8;

        // Write to previously uninitialized memory cell
        mstore 1, 100, 0, 1234;
        A1, A2 <== mload(1, 100);
        assert_eq A1, 0;
        assert_eq A2, 1234;

        // Write max value
        mstore 0, 200, 0xffff, 0xffff;
        A1, A2 <== mload(0, 200);
        assert_eq A1, 0xffff;
        assert_eq A2, 0xffff;

        // Store at maximal address
        mstore 0xffff, 0xfffc, 1, 0;
        A1, A2 <== mload(0xffff, 0xfffc);
        assert_eq A1, 1;
        assert_eq A2, 0;

        return;
    }
}
