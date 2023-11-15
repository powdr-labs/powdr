// we don't need to specify an operation_id if we have a single operation
machine SingleOperation(latch, _) {
    operation nothing;

    col fixed latch = [1]*;
    col witness w;
    w = w * w;
}

machine Main {
    SingleOperation m;

    link 1 = m.nothing;
}