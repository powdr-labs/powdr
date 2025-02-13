use std::protocols::bus::BusInteraction;
use std::protocols::bus::bus_multi_send;
use std::protocols::bus::bus_multi_receive_batch_lookup_permutation;

machine Main with degree: 8 {
    // Lookup LHS
    col fixed x = [1, 5, 2, 6, 4, 2, 6, 3]; // input
    col witness y; // output for x + 1
    col witness z; // output for x + 2

    let LOOKUP_ID_0 = 42;
    let LOOKUP_ID_1 = 53;

    // Lookup RHS
    // A small block machine that precomputes:
    // f(x) = x + 1 for all x in [1, 8]
    // g(x) = x + 2 for all x in [1, 8]
    col fixed LOOKUP_X = [1, 2, 3, 4, 5, 6, 7, 8];
    col fixed LOOKUP_Y = [2, 3, 4, 5, 6, 7, 8, 9];
    col fixed LOOKUP_Z = [3, 4, 5, 6, 7, 8, 9, 10];
    // Note that latch for lookup RHS is provided as a constant in BusInteraction::Send
    col fixed lookup_latch = [1]*;

    // Permutation LHS
    // Will use the same inputs x
    col witness a; // output for x + 42
    col witness b; // output for x + 21

    col fixed sel = [1, 1, 1, 0, 0, 0, 0, 0]; // LHS call selector (link if flag ~>)

    let PERMUTATION_ID_0 = 123;
    let PERMUTATION_ID_1 = 456;

    // Permutation RHS
    // A small block machine that computes:
    // f(x) = x + 42 for any x
    // g(x) = x + 21 for any xs
    col witness sub_sel_0, sub_sel_1; // RHS call selectors (block machine header `call_selectors`)
    col witness PERM_X, PERM_A, PERM_B;
    PERM_A = PERM_X + 42;
    PERM_B = PERM_X + 21;
    col fixed perm_latch = [1]*;

    // Multi send (same API for both lookup and permutation)
    bus_multi_send([
      BusInteraction::Send(LOOKUP_ID_0, [x, y], lookup_latch), // latch is always the same as multiplicity for sends (for both lookup and permutation)
      BusInteraction::Send(LOOKUP_ID_1, [x, z], lookup_latch),
      BusInteraction::Send(PERMUTATION_ID_0, [x, a], sel),
      BusInteraction::Send(PERMUTATION_ID_1, [x, b], sel)
    ]);
    
    // Multi receive (last argument `is_permutation` is 1 for permutation and 0 for lookup)
    bus_multi_receive_batch_lookup_permutation([
      (LOOKUP_ID_0, lookup_latch, [LOOKUP_X, LOOKUP_Y], 0), // selector is lookup_latch, multiplicity is a witness column (not an input here)
      (LOOKUP_ID_1, lookup_latch, [LOOKUP_X, LOOKUP_Z], 0), // selector is lookup_latch, multiplicity is a witness column (not an input here)
      (PERMUTATION_ID_0, sub_sel_0 * perm_latch, [PERM_X, PERM_A], 1), // selector is sub_sel_0 * perm_latch, multiplicity is selector negated
      (PERMUTATION_ID_1, sub_sel_1 * perm_latch, [PERM_X, PERM_B], 1) // selector is sub_sel_1 * perm_latch, multiplicity is selector negated
    ]);
}
