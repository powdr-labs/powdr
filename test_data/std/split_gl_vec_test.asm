use std::machines::split::ByteCompare;
use std::machines::split::split_gl::SplitGL;
use std::machines::split::split_gl_vec::SplitGLVec4;
use std::machines::large_field::memory::Memory;
use std::machines::range::Byte2;

let main_degree: int = 2**8;
let memory_degree: int = 2**12;
let split_degree: int = 2**10;
let split_vec_degree: int = 2**10;

machine Main with degree: main_degree {
    reg pc[@pc];
    reg X0[<=];
    reg X1[<=];

    Byte2 byte2;
    Memory memory(byte2, memory_degree, memory_degree);

    ByteCompare byte_compare;
    SplitGL split_machine(byte_compare, split_degree, split_degree);

    SplitGLVec4 split_vec_machine(memory, split_machine, split_vec_degree, split_vec_degree);

    col fixed STEP(i) { 2 * i };

    instr mstore X0, X1 ->
        link ~> memory.mstore(X0, STEP, X1);

    instr split X0, X1
        link ~> split_vec_machine.split(X0, X1, STEP);

    col witness val;
    instr assert_eq X0, X1 ->
        link ~> val = memory.mload(X0, STEP)
    {
        val = X1
    }

    function main {
        // Store 8 field elements sequentially in memory
        mstore 100, 1;
        mstore 104, 0xffffffff00000000;
        mstore 108, 0xfffffffeffffffff;
        mstore 112, 0xabcdef0123456789;

        // Split the previously stored field elements
        split 100, 200;

        // Assert the field elements are what was written
        assert_eq 200, 1;
        assert_eq 204, 0;

        assert_eq 208, 0;
        assert_eq 212, 0xffffffff;

        assert_eq 216, 0xffffffff;
        assert_eq 220, 0xfffffffe;

        assert_eq 224, 0x23456789;
        assert_eq 228, 0xabcdef01;

        // Same split, but now overlaping the input and output
        split 100, 104;

        // Assert the field elements are what was written
        assert_eq 104, 1;
        assert_eq 108, 0;

        assert_eq 112, 0;
        assert_eq 116, 0xffffffff;

        assert_eq 120, 0xffffffff;
        assert_eq 124, 0xfffffffe;

        assert_eq 128, 0x23456789;
        assert_eq 132, 0xabcdef01;

        return;
    }
}
