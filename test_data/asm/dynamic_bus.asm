use std::protocols::bus::bus;
use std::protocols::bus::BusInteraction;

let ADD_BUS_ID = 123;
let MUL_BUS_ID = 456;

machine Main with
    degree: 8,
    latch: latch,
    operation_id: operation_id
{
    // Here, we simulate what an ASM bus linker would do using a "dynamic" bus,
    // i.e., where the bus IDs of the bus sends only need to be known at runtime.
    // See static_bus.asm for a less efficient implementation using a "static" bus.

    // Add block machine
    col witness add_a, add_b, add_c, add_sel;
    std::utils::force_bool(add_sel);
    add_c = add_a + add_b;
    bus(BusInteraction::Receive(ADD_BUS_ID, [add_a, add_b, add_c], add_sel, add_sel));

    // Mul block machine
    col witness mul_a, mul_b, mul_c, mul_sel;
    std::utils::force_bool(mul_sel);
    mul_c = mul_a * mul_b;
    bus(BusInteraction::Receive(MUL_BUS_ID, [mul_a, mul_b, mul_c], mul_sel, mul_sel));
    
    // Main machine
    col fixed is_mul = [0, 1]*;
    col fixed x(i) {i * 42};
    col fixed y(i) {i + 12345};
    col witness z;

    // Because we're doing exactly one of the two operations at any given time,
    // we only need to do one send, choosing the bus to send to at runtime.
    bus(BusInteraction::Send(is_mul * MUL_BUS_ID + (1 - is_mul) * ADD_BUS_ID, [x, y, z], 1));
}
