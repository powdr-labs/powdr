/// A too-small version of std::machines::range::Byte2.
/// It is used in tests which happen to only use the first 256 values.
machine FakeByte2 with
    latch: latch,
    operation_id: operation_id,
    degree: 256
{
    operation check<0> BYTE2 -> ;

    let BYTE2: col = |i| i & 0xffff;
    col fixed latch = [1]*;
    col fixed operation_id = [0]*;
}