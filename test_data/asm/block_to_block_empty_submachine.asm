// This test is mostly a copy of block_to_block.asm, but:
// - The machines have dynamic length (either 4 or 8)
// - `instr_add` can be set by the prover.
//
// We set `instr_add` to zero in all rows at runtime. This simulates
// a secondary machine that is never called (and, because it depends on
// runtime information, it's not removed by the optimizer).

machine Arith with
    latch: latch,
    operation_id: operation_id
{
    operation add<0> x, y -> z;

    col fixed operation_id = [0]*;
    col fixed latch = [1]*;
    col witness x;
    col witness y;
    col witness z;
    z = x + y;
}

machine Main with
    min_degree: 4,
    max_degree: 8,
    latch: latch,
    operation_id: operation_id
{
    Arith arith(4, 8);

    // return `3*x + 3*y`, where the prover can chose whether to add locally or externally
    operation main<0>;

    link if instr_add => z = arith.add(x, y);

    col fixed operation_id = [0]*;
    col fixed x(i) { i / 4 };
    col fixed y(i) { i / 4 + 1 };
    col witness z;
    col witness res;
    col fixed latch = [0, 0, 0, 1]*; // return every 4th row

    // accumulate the intermediate results into `res`
    // we waste a row here as we initialize res at 0
    // this is due to a limitation in witgen
    res' = (1 - latch) * (res + z);

    // add locally when `instr_add` is off
    (1 - instr_add) * (x + y - z) = 0;

    // `instr_add` is unconstrained, but set to zero at runtime,
    // resulting in no calls to `arith`
    col witness instr_add(i) query Query::Hint(0);
}
