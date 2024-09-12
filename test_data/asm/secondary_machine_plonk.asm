machine Main with
    degree: 256
{
    Pythagoras pythagoras;

    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg RES;

    instr c_squared X, Y -> Z link => Z = pythagoras.c_squared(X, Y);
    instr assert_eq X, Y { X = Y }

    function main {
        RES <== c_squared(3, 4);
        assert_eq RES, 25;

        RES <== c_squared(1, 1);
        assert_eq RES, 2;

        RES <== c_squared(1, 2);
        assert_eq RES, 5;

        RES <== c_squared(2, 1);
        assert_eq RES, 5;

        RES <== c_squared(123, 1234);
        assert_eq RES, 1537885;

        return;
    }
}

machine Pythagoras with
    latch: latch
{

    operation c_squared a, b -> c;

    col fixed latch = [1, 0, 0, 0]*;

    // This circuit computes c[2] = a[0]^2 + a[1]^2
    // Using the following layout:
    //   a     b     c
    //   a[0]  b[0]  c[0]    <-- Latch row
    //   a[1]  b[1]  c[1]    <-- MUL
    //   a[2]  b[2]  c[2]    <-- MUL
    //   a[3]  b[3]  c[3]    <-- ADD

    // This assumes the following copy constraints:
    // - a[0] = a[1]
    // - a[0] = b[1]
    // - b[0] = a[2]
    // - b[0] = b[2]
    // - c[1] = a[3]
    // - c[2] = b[3]
    // - c[0] = c[3]

    // Whether the current gate is multiplication
    col fixed MUL = [0, 1, 1, 0]*;

    // Whether the current gate is addition
    col fixed ADD = [0, 0, 0, 1]*;

    let a;
    let b;
    let c;

    // Implement addition and multiplication gate
    MUL * (a * b - c) = 0;
    ADD * (a + b - c) = 0;
}
