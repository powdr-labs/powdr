use std::array;

machine Main {

    degree 256;

    SumAndProd sap_machine;

    reg pc[@pc];
    reg X1[<=];
    reg X2[<=];
    reg X3[<=];
    reg X4[<=];
    reg Y1[<=];
    reg Y2[<=];
    reg Y3[<=];
    reg Y4[<=];
    reg A;
    reg B;
    reg C;
    reg D;

    instr sap X1, X2, X3, X4 -> Y1, Y2, Y3, Y4 = sap_machine.sap;
    instr assert_eq X1, X2 { X1 = X2 }

    function main {
        A, B, C, D <== sap(1, 4, 8, 17);
        assert_eq A, 7;
        assert_eq B, 30;
        assert_eq C, 1 + 32 + 17;
        assert_eq D, 18;

        return;
    }
}

machine SumAndProd(latch, _) {
    col fixed latch = [1]*;

    // This mixes arrays (x, y) and single columns (a, b, c, d).
    operation sap a, x, b -> c, y, d;

    col witness x[2];
    col witness y[2];
    let a;
    let b;
    let c;
    let d;

    y[0] = a + array::sum(x) + b;
    y[1] = a + x[0] * x[1] + b;
    c = 7;
    d = 18;
}
