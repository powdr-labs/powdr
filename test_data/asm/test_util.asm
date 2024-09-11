/// A version of std::machines::range::Byte2 that actually constrains
/// to 0..255 because of the limited number of rows.
/// It is used in tests which happen to only use the first 256 values.
machine FakeByte2 with
    latch: latch,
    operation_id: operation_id,
    degree: 256
{
    operation check<0> BYTE2 -> ;

    let BYTE2: col = |i| i & 0xffff;
    let latch: col = |i| 1;
    let operation_id: col = |i| 0;
}