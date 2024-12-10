// This example tests that range constraints are passed back and forth between
// machines to solve a call.
// In this example, a 8-bit input is decomposed into two chunks (which requires witgen
// to know the range constraints of the chunks) and an 8-bit output is also decomposed
// into two chunks (which, again, requires witgen to know the range constraints in the
// calling machine).

machine Mul with
    degree: 16,
    latch: latch,
    operation_id: operation_id,
    call_selectors: sel,
{
    operation mul<0> input -> z;

    col fixed FOUR_BIT(i) { i & 0xf };

    col fixed operation_id = [0]*;
    col fixed latch = [1]*;
    let used = std::array::sum(sel);
    let input = x + 16 * y;

    col witness x;
    col witness y;
    col witness z;

    // Make range constraints conditional on "used", just so
    // that witgen is forced to infer the range constraints
    // while solving, rather than inferring global range constraints.
    used $ [ x ] in [ FOUR_BIT ];
    used $ [ y ] in [ FOUR_BIT ];

    z = x * y;
}

machine Main with
    degree: 16,
    latch: latch,
    operation_id: operation_id
{
    Mul mul;

    operation main<0>;

    link if latch ~> res = mul.mul(x);

    col fixed operation_id = [0]*;
    col fixed latch = [1, 0]*;
    // Just some numbers that can be decomposed into 2 4-bit chunks.
    col fixed x(i) { i * 15 };
    col witness res_lower;
    col witness res_upper;
    let res = res_lower + 16 * res_upper;

    // Again, make range constraints conditional
    col fixed FOUR_BIT(i) { i & 0xf };
    latch $ [ res_lower ] in [ FOUR_BIT ];
    latch $ [ res_upper ] in [ FOUR_BIT ];
}
