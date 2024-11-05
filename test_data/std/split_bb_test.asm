use std::machines::split::ByteCompare;
use std::machines::split::split_bb::SplitBB;

machine Main with degree: 65536 {
    reg pc[@pc];
    reg X0[<=];
    reg X1[<=];
    reg X2[<=];
    reg low;
    reg high;

    ByteCompare byte_compare;
    SplitBB split_machine(byte_compare);

    instr split X0 -> X1, X2 link ~> (X1, X2) = split_machine.split(X0);

    instr assert_eq X0, X1 {
        X0 = X1
    }

    function main {

        // Min value
        // Note that this has two byte decompositions, 0 and p = 0x78000001.
        // The second would lead to a different split value, but should be ruled
        // out by the overflow check.
        low, high <== split(0);
        assert_eq low, 0;
        assert_eq high, 0;

        // Max value
        // On BabyBear, this is 0x78000000.
        low, high <== split(-1);
        assert_eq low, 0;
        assert_eq high, 0x7800;

        // Max low value
        low, high <== split(0x77ffffff);
        assert_eq low, 0xffff;
        assert_eq high, 0x77ff;

        // Some other value
        low, high <== split(0x42abcdef);
        assert_eq low, 0xcdef;
        assert_eq high, 0x42ab;

        return;
    }
}
