// calls a constrained machine from a constrained machine
machine Arith(latch, operation_id) {

    degree 8;

    operation add<0> x, y -> z;

    constraints {
        col fixed operation_id = [0]*;
        col fixed latch = [1]*;
        col witness x;
        col witness y;
        col witness z;
        z = x + y;
    }
}

machine Main(latch, operation_id) {

    degree 8;

    Arith arith;

    // return `3*x + 3*y`, adding twice locally and twice externally
    operation main<0>;

    link instr_add x, y -> z = arith.add;

    constraints {
        col fixed operation_id = [0]*;
        col fixed x(i) { i / 4 };
        col fixed y(i) { i / 4 + 1 };
        col witness z;
        col witness res;
        col fixed latch = [0, 0, 0, 1]*; // return every 4th row

        // accumulate the intermediate results into `res`
        // we waste a row here as we initialize res at 0
        // this is due to a limitation in witgen
        res' = (1 - latch) * (res + z);

        // add locally when `instr_add` is off
        (1 - instr_add) * (x + y - z) = 0;
        // add using `arith` every other row
        col fixed instr_add = [0, 1]*;
    }
}