use std::protocols::bus::BusInteraction;
use std::protocols::bus::bus_multi;
use std::protocols::bus::bus_multi_linker;
use std::protocols::bus::BusLinkerType;

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
    // Note that the latch for the lookup RHS is provided as a constant in BusInteraction::Send
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

    // Batch all sends, lookup receives, and permutation receives
    // Input format: id, selector, payload, type
    bus_multi_linker([
      // Latch is always the same as multiplicity for sends (for both lookup and permutation)
      (LOOKUP_ID_0, lookup_latch, [x, y], BusLinkerType::Send),
      (LOOKUP_ID_1, lookup_latch, [x, z], BusLinkerType::Send),
      (PERMUTATION_ID_0, sel, [x, a], BusLinkerType::Send),
      (PERMUTATION_ID_1, sel, [x, b], BusLinkerType::Send),
      (LOOKUP_ID_0, lookup_latch, [LOOKUP_X, LOOKUP_Y], BusLinkerType::LookupReceive), // selector is lookup_latch, multiplicity is a witness column (not an input here)
      (LOOKUP_ID_1, lookup_latch, [LOOKUP_X, LOOKUP_Z], BusLinkerType::LookupReceive), // selector is lookup_latch, multiplicity is a witness column (not an input here)
      (PERMUTATION_ID_0, sub_sel_0 * perm_latch, [PERM_X, PERM_A], BusLinkerType::PermutationReceive), // selector is sub_sel_0 * perm_latch, multiplicity is `-selector`
      (PERMUTATION_ID_1, sub_sel_1 * perm_latch, [PERM_X, PERM_B], BusLinkerType::PermutationReceive) // selector is sub_sel_1 * perm_latch, multiplicity is `-selector`
    ]);
}
