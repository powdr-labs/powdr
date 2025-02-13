use std::protocols::bus::bus_multi_receive;
use std::protocols::bus::bus_multi_send;
use std::protocols::bus::BusInteraction;

let ADD_BUS_ID = 123;
let MUL_BUS_ID = 456;
let SUB_BUS_ID = 789;
let DOUBLE_BUS_ID = 234;
let TRIPLE_BUS_ID = 321;

machine Main with
    degree: 8,
    latch: latch,
    operation_id: operation_id
{
    // Here, we simulate what an ASM bus linker would do using a "static" bus,
    // i.e., all bus IDs are known at compile time.
    // See dynamic_bus.asm for a more efficient implementation using a "dynamic" bus.

    // Add block machine
    col witness add_a, add_b, add_c, add_sel;
    std::utils::force_bool(add_sel);
    add_c = add_a + add_b;

    // Mul block machine
    col witness mul_a, mul_b, mul_c, mul_sel;
    std::utils::force_bool(mul_sel);
    mul_c = mul_a * mul_b;

    // Sub block machine
    col witness sub_a, sub_b, sub_c, sub_sel;
    std::utils::force_bool(sub_sel);
    sub_c = sub_a - sub_b;

    // Double block machine
    col witness double_a, double_b, double_c, double_sel;
    std::utils::force_bool(double_sel);
    double_c = 2 * double_a + 2 * double_b;

    // Triple block machine
    col witness triple_a, triple_b, triple_c, triple_sel;
    std::utils::force_bool(triple_sel);
    triple_c = 3 * triple_a + 3 * triple_b;

    // Multi bus receive
    bus_multi_receive(
      [
        BusInteraction::Receive(ADD_BUS_ID, [add_a, add_b, add_c], add_sel, add_sel),
        BusInteraction::Receive(MUL_BUS_ID, [mul_a, mul_b, mul_c], mul_sel, mul_sel),
        BusInteraction::Receive(SUB_BUS_ID, [sub_a, sub_b, sub_c], sub_sel, sub_sel),
        BusInteraction::Receive(DOUBLE_BUS_ID, [double_a, double_b, double_c], double_sel, double_sel),
        BusInteraction::Receive(TRIPLE_BUS_ID, [triple_a, triple_b, triple_c], triple_sel, triple_sel)
      ]
    );
    
    // Main machine
    col fixed is_add    = [1, 0, 0, 0, 0]*;
    col fixed is_mul    = [0, 1, 0, 0, 0]*;
    col fixed is_sub    = [0, 0, 1, 0, 0]*;
    col fixed is_double = [0, 0, 0, 1, 0]*;
    col fixed is_triple = [0, 0, 0, 0, 1]*;
    col fixed x(i) {i * 42};
    col fixed y(i) {i + 12345};
    col witness z;

    // Because the bus ID needs to be known at compile time, we have to do
    // a bus send for each receiver, even though at most one send will be
    // active in each row.
    bus_multi_send(
      [
        BusInteraction::Send(MUL_BUS_ID, [x, y, z], is_mul),
        BusInteraction::Send(ADD_BUS_ID, [x, y, z], is_add),
        BusInteraction::Send(DOUBLE_BUS_ID, [x, y, z], is_double),
        BusInteraction::Send(SUB_BUS_ID, [x, y, z], is_sub),
        BusInteraction::Send(TRIPLE_BUS_ID, [x, y, z], is_triple)
      ]
    );
}
