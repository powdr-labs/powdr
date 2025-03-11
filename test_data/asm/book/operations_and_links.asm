machine Main with degree: 16 {
    Add4 adder;

    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg W[<=];
    reg R[<=];
    reg A;

    instr assert_eq X, Y { X = Y }

    instr add4 X,Y,Z,W -> R link => R = adder.add4(X,Y,Z,W);

    function main {
       A <== add4(1, 2, 3, 4);
       assert_eq A, 10;
       A <== add4(2, 2, 2, 2);
       assert_eq A, 8;

       return;
    }
}

// ANCHOR: links
machine Add4 with
    degree: 32,
    latch: latch,
    operation_id: operation_id
{
    Add adder;

    operation add4<0> x, y, z, w -> r;

    // Links without a flag are active on every row.
    // - constrain the values of `x`, `y`, and `n` so that `n = adder.add(x, y)`
    link => n = adder.add(x, y);
    // - constrain the values of `z`, `w`, and `m` so that `m = adder.add(z, w)`
    link => m = adder.add(z, w);
    // - constrain the values of `m`, `n` and `r` so that `r = adder.add(m,n)`
    link => r = adder.add(m, n);

    col fixed operation_id = [0]*;
    col fixed latch = [1]*;

    col witness x;
    col witness y;
    col witness z;
    col witness w;
    col witness r;
    col witness m;
    col witness n;
}

// ANCHOR: one_operation
machine Add with
    degree: 32,
    latch: latch
{
    // operation name, with column names as inputs and outputs
    operation add a, b -> c;

    col fixed latch = [1]*;

    col witness a;
    col witness b;
    col witness c;

    // constraint "implementing" the operation
    c = a + b;
}
// ANCHOR_END: one_operation
// ANCHOR_END: links

// ANCHOR: many_operations
// machine declaration must include an operation id column name
machine AddSub with
    degree: 32,
    latch: latch,
    operation_id: op_id
{
    // each operation has its own unique operation id
    operation add<0> a, b -> c;
    operation sub<1> a, b -> c;

    col fixed latch = [1]*;
        // it also needs to be declared as a column
    col witness op_id;

    col witness a;
    col witness b;
    col witness c;

    // constraint "implementing" both operations, depending on `op_id`
    c = (1 - op_id) * (a + b) + op_id * (a - b);
}
// ANCHOR_END: many_operations
