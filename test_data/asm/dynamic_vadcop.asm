use std::machines::memory::Memory;

// Copy of std::machines::range::Byte2 which sets the correct degree.
machine Byte2 with
    latch: latch,
    operation_id: operation_id,
    degree: 65536
{
    operation check<0> BYTE2 -> ;

    let BYTE2: col = |i| i & 0xffff;
    let latch: col = |i| 1;
    let operation_id: col = |i| 0;
}

machine Main {
    Arith arith;

    let STEP: col = |i| { i };
    Byte2 byte2;
    Memory memory(byte2);

    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg A;
    reg B;
    reg Z[<=]; // we declare this assignment register last to test that the ordering does not matter

    instr add X, Y -> Z link => Z = arith.add(X, Y);
    instr mul X, Y -> Z link => Z = arith.mul(X, Y);
    instr mload X -> Y link ~> Y = memory.mload(X, STEP);
    instr mstore X, Y -> link ~> memory.mstore(X, STEP, Y);
    instr assert_eq X, Y { X = Y }

    function main {
        mstore 0, 1;
        mstore 1, 2;

        A <== mload(0);
        B <== mload(1);

        A <== add(A, B);
        A <== mul(A, 9);
        assert_eq A, 27;
        return;
    }
}

machine Arith with
    latch: latch,
    operation_id: operation_id
{

    operation add<0> x[0], x[1] -> y;
    operation mul<1> x[0], x[1] -> y;

    let latch: col = |i| 1;
    let operation_id;
    let x[2];
    let y;

    y = operation_id * (x[0] * x[1]) + (1 - operation_id) * (x[0] + x[1]);
}
