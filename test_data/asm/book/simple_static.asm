machine SimpleStatic(latch, operation_id) {

    degree 8;

    operation power_4<0> x -> y;

    constraints {
        col fixed operation_id = [0]*;
        col fixed latch = [0, 0, 0, 1]*;
        col witness x;
        col witness y;

        // initialise y to x at the beginning of each block
        latch * (y' - x') = 0;
        // x is unconstrained at the beginning of the block

        // x is constant within a block
        (1 - latch) * (x' - x) = 0;
        // y is multiplied by x at each row
        (1 - latch) * (y' - x * y) = 0;
    }
}