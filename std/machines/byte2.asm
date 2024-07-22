/// A machine to check that a field element represents two bytes. It uses an exhaustive lookup table.
machine Byte2 with
    latch: latch,
    operation_id: operation_id
{
    operation check<0> BYTE2 -> ;

    let BYTE2: col = |i| i & 0xffff;
    col fixed latch = [1]*;
    col fixed operation_id = [0]*;
}