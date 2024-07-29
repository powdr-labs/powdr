machine Other with
    latch: latch,
    operation_id: operation_id,
    degree: 8
{
    operation nothing<0>;

    col fixed latch = [1]*;
    col fixed operation_id = [0]*;

    col witness w;
    w * w = w;
}
