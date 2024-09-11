// calls a constrained machine from a constrained machine
machine Arith with
    degree: 8,
    latch: latch,
    operation_id: operation_id
{
    operation add<0> x, y -> z;

    let operation_id: col = |i| 0;
    let latch: col = |i| 1;
    let x;
    let y;
    let z;
    z = x + y;
}

machine Main with
    degree: 8,
    latch: latch,
    operation_id: operation_id
{
    Arith arith;

    // return `3*x + 3*y`, adding twice locally and twice externally
    operation main<0>;

    link if instr_add => z = arith.add(x, y);

    let operation_id: col = |i| 0;
    let x: col = |i| { i / 4 };
    let y: col = |i| { i / 4 + 1 };
    let z;
    let res;
    col fixed latch = [0, 0, 0, 1]*; // return every 4th row

    // accumulate the intermediate results into `res`
    // we waste a row here as we initialize res at 0
    // this is due to a limitation in witgen
    res' = (1 - latch) * (res + z);

    // add locally when `instr_add` is off
    (1 - instr_add) * (x + y - z) = 0;
    // add using `arith` every other row
    col fixed instr_add = [0, 1]*;
}
