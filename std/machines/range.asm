machine Byte with
    latch: latch,
    operation_id: operation_id,
    degree: 256
{
    operation check<0> BYTE -> ;

    let BYTE: col = |i| i & 0xff;
    let latch: col = |i| 1;
    let operation_id: col = |i| 0;
}

machine Byte2 with
    latch: latch,
    operation_id: operation_id,
    degree: 65536
{
    operation check<0> BYTE2 -> ;

    let BYTE2: col = |i| i & 0xffff;
    let latch: col = |i| 1;
    let operation_id: col = |i| 0;
}

machine Bit2 with
    latch: latch,
    operation_id: operation_id,
    degree: 4
{
    operation check<0> BIT2 -> ;

    let BIT2: col = |i| i % 4;
    let latch: col = |i| 1;
    let operation_id: col = |i| 0;
}

machine Bit6 with
    latch: latch,
    operation_id: operation_id,
    degree: 64
{
    operation check<0> BIT6 -> ;

    let BIT6: col = |i| i % 64;
    let latch: col = |i| 1;
    let operation_id: col = |i| 0;
}

machine Bit7 with
    latch: latch,
    operation_id: operation_id,
    degree: 128
{
    operation check<0> BIT7 -> ;

    let BIT7: col = |i| i % 128;
    let latch: col = |i| 1;
    let operation_id: col = |i| 0;
}
