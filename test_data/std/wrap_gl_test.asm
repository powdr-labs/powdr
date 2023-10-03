use std::wrap::wrap_gl::WrapGL;


machine Main {
    reg pc[@pc];
    reg X0[<=];
    reg X1[<=];
    reg A;

    degree 65536;

    WrapGL wrap_machine;

    instr wrap X0 -> X1 = wrap_machine.wrap

    instr assert_eq X0, X1 {
        X0 = X1
    }

    instr loop { pc' = pc }

    function main {

        // Min value
        // Note that this has two byte decompositions, 0x and p = 0xffffffff00000001.
        // The second would lead to a different wrapped value, but should be ruled
        // out by the overflow check.
        A <== wrap(0);
        assert_eq A, 0;

        // Max value
        // On Goldilocks, this is 0xffffffff00000000, which wraps to 0.
        A <== wrap(-1);
        assert_eq A, 0;

        // Max wrapped value
        A <== wrap(0xfffffffeffffffff);
        assert_eq A, 0xffffffff;

        // Some other value
        A <== wrap(0xabcdef0123456789);
        assert_eq A, 0x23456789;

        return;
    }
}