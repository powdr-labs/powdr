use std::protocols::bus::bus_multi_send;
use std::protocols::bus::BusInteraction;
use std::protocols::permutation_via_bus::permutation_multi_receive;

machine Main with degree: 8 {

    col fixed x = [0, 0, 1, 2, 3, 4, 5, 6];
    col witness y;
    col witness z;

    // A small block machine that computes f(x) = x + 42 and g(x) = x + 21
    col witness sub_x, sub_y, sub_z, sub_sel_0, sub_sel_1;
    sub_y = sub_x + 42;
    sub_z = sub_x + 21;

    // Witgen fails if we have too many rows, similar to `bus_permutation.asm`
    col fixed sel = [1, 1, 1, 0, 0, 0, 0, 0];

    // Add the bus permutation constraints
    let ID_0 = 123;
    let ID_1 = 456;
    bus_multi_send([
      BusInteraction::Send(ID_0, [x, y], sel),
      BusInteraction::Send(ID_1, [x, z], sel)
    ]);
    permutation_multi_receive([
      (ID_0, sub_sel_0, [sub_x, sub_y]),
      (ID_1, sub_sel_1, [sub_x, sub_z])
    ]);
}
