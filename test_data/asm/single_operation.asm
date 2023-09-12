// we don't need to specify an operation_id if we have a single operation
machine SingleOperation(latch, _) {
    operation nothing;

    // added for the same reason as in `empty.asm`
    col fixed A(i) { i };
    col fixed latch = [1]*;
    col witness w;
    w = w * w;
}

machine Main {
    SingleOperation m;

    col fixed A(i) { i };

    link 1 = m.nothing;
}