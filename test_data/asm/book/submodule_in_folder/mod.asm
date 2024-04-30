machine Other with
    latch: latch,
    operation_id: operation_id
{
    operation nothing<0>;

    col fixed latch = [1]*;
    col fixed operation_id = [0]*;

    col witness w;
    w * w = w;
}
