machine Byte with
    latch: latch,
    degree: 256
{
    operation check BYTE -> ;

    let BYTE: col = |i| i & 0xff;
    col fixed latch = [1]*;
}

machine Byte2 with
    latch: latch,
    degree: 65536
{
    operation check BYTE2 -> ;

    let BYTE2: col = |i| i & 0xffff;
    col fixed latch = [1]*;
}

machine Bit2 with
    latch: latch,
    degree: 4
{
    operation check BIT2 -> ;

    let BIT2: col = |i| i % 4;
    col fixed latch = [1]*;
}

machine Bit6 with
    latch: latch,
    degree: 64
{
    operation check BIT6 -> ;

    let BIT6: col = |i| i % 64;
    col fixed latch = [1]*;
}

machine Bit7 with
    latch: latch,
    degree: 128
{
    operation check BIT7 -> ;

    let BIT7: col = |i| i % 128;
    col fixed latch = [1]*;
}

machine Bit12 with
    latch: latch,
    degree: 4096
{
    operation check BIT12 -> ;

    let BIT12: col = |i| i % (2**12);
    let latch = 1;
}
