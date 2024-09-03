// we don't need to specify an operation_id if we have a single operation
machine SingleOperation with
    latch: latch
{
    operation nothing;

    col fixed latch = [1]*;
    col witness w;
    w = w * w;
}

machine Main with degree: 8 {
    SingleOperation m;

    link => m.nothing();
}
