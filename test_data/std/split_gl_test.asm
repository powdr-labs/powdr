use std::machines::split::ByteCompare;
use std::machines::split::split_gl::SplitGL;

let main_degree: int = 2**8;
let split_degree: int = 2**10;

machine Main with degree: main_degree {
    reg pc[@pc];
    reg X0[<=];
    reg X1[<=];
    reg X2[<=];
    reg low;
    reg high;

    ByteCompare byte_compare;
    SplitGL split_machine(byte_compare, split_degree, split_degree);

    instr split X0 -> X1, X2 link ~> (X1, X2) = split_machine.split(X0);

    instr assert_eq X0, X1 {
        X0 = X1
    }

    function main {

        // Min value
        // Note that this has two byte decompositions, 0x and p = 0xffffffff00000001.
        // The second would lead to a different split value, but should be ruled
        // out by the overflow check.
        low, high <== split(0);
        assert_eq low, 0;
        assert_eq high, 0;

        // Max value
        // On Goldilocks, this is 0xffffffff00000000.
        low, high <== split(-1);
        assert_eq low, 0;
        assert_eq high, 0xffffffff;

        // Max low value
        low, high <== split(0xfffffffeffffffff);
        assert_eq low, 0xffffffff;
        assert_eq high, 0xfffffffe;

        // Some other value
        low, high <== split(0xabcdef0123456789);
        assert_eq low, 0x23456789;
        assert_eq high, 0xabcdef01;

        return;
    }
}
