use std::protocols::bus::bus_multi;
use std::protocols::bus::BusInteraction;
use std::protocols::lookup_via_bus::lookup_multi_receive;

machine Main with degree: 8 {

    col fixed x = [1, 5, 2, 6, 4, 2, 6, 3];
    // y = random 
    col fixed y = [0, 0, 0,0, 0, 0, 0,0];
    // z = 2*x+3-y
    col fixed z = [5,13,7,15,11,7,15,9];

    // Pre-compute f(x) = x + 1 for all x in [1, 8]
    // Pre-compute g(x) = x + 2 for all x in [1, 8]
    col fixed INC_X = [1, 2, 3, 4, 5, 6, 7, 8];
    col fixed INC_Y = [2, 3, 4, 5, 6, 7, 8, 9];
    col fixed INC_Z = [3, 4, 5, 6, 7, 8, 9, 10];

    let LOOKUP_ID_0 = 42;
    let LOOKUP_ID_1 = 53;

    bus_multi([
      BusInteraction::Send(LOOKUP_ID_0, [x, y], 1),
      BusInteraction::Send(LOOKUP_ID_1, [x, z], 1)
    ]);

    lookup_multi_receive([
      (LOOKUP_ID_0, 1, [INC_X, INC_Y]),
      (LOOKUP_ID_1, 1, [INC_X, INC_Z])
    ]);
}
