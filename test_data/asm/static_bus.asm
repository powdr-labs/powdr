use std::protocols::bus::bus_receive;
use std::protocols::bus::bus_send;

let ADD_BUS_ID = 123;
let MUL_BUS_ID = 456;

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
    bus_receive(ADD_BUS_ID, [add_a, add_b, add_c], add_sel, add_sel);

    // Mul block machine
    col witness mul_a, mul_b, mul_c, mul_sel;
    std::utils::force_bool(mul_sel);
    mul_c = mul_a * mul_b;
    bus_receive(MUL_BUS_ID, [mul_a, mul_b, mul_c], mul_sel, mul_sel);
    
    // Main machine
    col fixed is_mul = [0, 1]*;
    col fixed x(i) {i * 42};
    col fixed y(i) {i + 12345};
    col witness z;

    // Because the bus ID needs to be known at compile time, we have to do
    // a bus send for each receiver, even though at most one send will be
    // active in each row.
    bus_send(MUL_BUS_ID, [x, y, z], is_mul);
    bus_send(ADD_BUS_ID, [x, y, z], 1 - is_mul);
}
