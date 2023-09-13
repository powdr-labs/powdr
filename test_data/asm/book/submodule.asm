machine Other(latch, operation_id) {
    operation nothing<0>;

    constraints {
        col fixed latch = [1]*;
        col fixed operation_id = [0]*;

        col witness w;
        w * w = w;
    }
}