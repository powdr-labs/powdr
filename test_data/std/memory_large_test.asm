use std::machines::range::Byte2;
use std::machines::large_field::memory::Memory;

let main_degree: int = 2**7;
let memory_degree: int = 2**5;

  machine Main with degree: main_degree {
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg A;

    col fixed STEP(i) { i };
    Byte2 byte2;
    Memory memory(byte2, memory_degree, memory_degree);

    instr mload X -> Y link ~> Y = memory.mload(X, STEP);
    instr mstore X, Y -> link ~> memory.mstore(X, STEP, Y);

    instr assert_eq X, Y {
        X = Y
    }

    function main {

        // Store 4
        mstore 100, 4;
        
        // Read uninitialized memory
        A <== mload(104);
        assert_eq A, 0;

        // Read previously stored value
        A <== mload(100);
        assert_eq A, 4;

        // Update previously stored value
        mstore 100, 7;
        mstore 100, 8;

        // Read updated values (twice)
        A <== mload(100);
        assert_eq A, 8;
        A <== mload(100);
        assert_eq A, 8;

        // Write to previously uninitialized memory cell
        mstore 104, 1234;
        A <== mload(104);
        assert_eq A, 1234;

        // Write very big field element
        mstore 200, -1;
        A <== mload(200);
        assert_eq A, -1;

        // Store at maximal address
        mstore 0xfffffffc, 1;
        A <== mload(0xfffffffc);
        assert_eq A, 1;



        return;
    }
}
