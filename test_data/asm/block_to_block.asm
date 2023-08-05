// calls two operations in a submachine whose interface is different: one is `x, y, z` while the other one is `z, x, y`

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

    // return `4*x + 4*y`, adding twice locally and twice externally
    operation main<0> x, y -> z;

    link instr_add x, y -> z = arith.add;

    constraints {
        col fixed operation_id = [0]*;
        col witness x;
        col witness y;
        col witness z;
        col fixed latch = [0, 0, 0, 1]*; // return every 4th row

        // add locally when `instr_add` is off
        (1 - instr_add) * (x + y - z) = 0;
        // add using arith every other row
        col fixed instr_add = [0, 1]*;
    }
}