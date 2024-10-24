machine Byte with
    latch: latch,
    operation_id: operation_id,
    degree: 256
{
    operation check<0> BYTE -> ;

    let BYTE: col = |i| i & 0xff;
    col fixed latch = [1]*;
    col fixed operation_id = [0]*;
}

machine Byte2 with
    latch: latch,
    operation_id: operation_id,
    degree: 65536
{
    operation check<0> BYTE2 -> ;

    let BYTE2: col = |i| i & 0xffff;
    col fixed latch = [1]*;
    col fixed operation_id = [0]*;
}

machine Bit2 with
    latch: latch,
    operation_id: operation_id,
    degree: 4
{
    operation check<0> BIT2 -> ;

    let BIT2: col = |i| i % 4;
    col fixed latch = [1]*;
    col fixed operation_id = [0]*;
}

machine Bit6 with
    latch: latch,
    operation_id: operation_id,
    degree: 64
{
    operation check<0> BIT6 -> ;

    let BIT6: col = |i| i % 64;
    col fixed latch = [1]*;
    col fixed operation_id = [0]*;
}

machine Bit7 with
    latch: latch,
    operation_id: operation_id,
    degree: 128
{
    operation check<0> BIT7 -> ;

    let BIT7: col = |i| i % 128;
    col fixed latch = [1]*;
    col fixed operation_id = [0]*;
}

machine Bit12 with
    latch: latch,
    operation_id: operation_id,
    degree: 4096
{
    operation check<0> BIT12 -> ;

    let BIT12: col = |i| i % (2**12);
    let latch = 1;
    col fixed operation_id = [0]*;
}