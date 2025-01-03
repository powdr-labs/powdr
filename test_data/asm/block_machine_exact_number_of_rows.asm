use std::machines::binary::ByteBinary;
use std::machines::large_field::binary::Binary;

// This test is a simpler version of: test_data/std/binary_large_test.asm
// It tests that witness generation for block machines also works when the number of rows
// is exactly what's needed to fit the required number of blocks.
// The large-field Binary machine is a good test-case, because it has an irregular (i.e.,
// non-rectangular) block shape.

// Currently, we need min_degree != max_degree, otherwise the linker sets all degrees
// to the main degree.
machine Main with min_degree: 32, max_degree: 64 {
    reg pc[@pc];
    reg X0[<=];
    reg X1[<=];
    reg X2[<=];
    reg A;

    ByteBinary byte_binary;
    // We'll call the binary machine 4 times and the block size
    // is 4, so we need exactly 16 rows.
    Binary binary(byte_binary, 16, 16);

    instr and X0, X1 -> X2 link ~> X2 = binary.and(X0, X1);

    function main {

        A <== and(0xaaaaaaaa, 0xaaaaaaaa);
        A <== and(0x55555555, 0x55555555);
        A <== and(0x00000000, 0xffffffff);
        A <== and(0xffffffff, 0xffffffff);

        return;
    }
}
