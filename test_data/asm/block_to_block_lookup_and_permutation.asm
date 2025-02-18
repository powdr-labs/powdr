let N: int = 8;

// calls a constrained machine from a constrained machine
machine AddLookup with
    latch: latch,
    operation_id: operation_id
{
    // A small block machine that precomputes:
    // f(x) = x + 1 for all x in [1, 8]
    // g(x) = x + 2 for all x in [1, 8]
    col fixed latch = [1]*;
    col witness operation_id;

    operation add_1<1> LOOKUP_X -> LOOKUP_Y;
    operation add_2<2> LOOKUP_X -> LOOKUP_Z;

    col witness LOOKUP_X, LOOKUP_Y, LOOKUP_Z;
    LOOKUP_Y = LOOKUP_X + 1;
    LOOKUP_Z = LOOKUP_X + 2;
}

machine AddPerm with
    latch: latch,
    operation_id: operation_id,
    call_selectors: sel // RHS call selector
{
    // A small block machine that computes:
    // f(x) = x + 42 for any x
    // g(x) = x + 21 for any x
    col fixed latch = [1]*;
    col witness operation_id;
    
    operation add_42<42> PERM_X -> PERM_A;
    operation add_21<21> PERM_X -> PERM_B;

    col witness PERM_X, PERM_A, PERM_B;
    PERM_A = PERM_X + 42;
    PERM_B = PERM_X + 21;
}

machine Main with degree: N {
    AddLookup add_lookup(16, 16);
    AddPerm add_perm(N, N);
    
    // Input
    col fixed x = [1, 5, 2, 6, 4, 2, 6, 3];

    // Lookup LHS
    col witness y; // output for x + 1
    col witness z; // output for x + 2
    col fixed lookup_sel = [1]*;

    // Permutation LHS
    // Will use the same inputs x
    col witness a; // output for x + 42
    col witness b; // output for x + 21
    col fixed perm_sel = [1, 1, 1, 0, 0, 0, 0, 0];

    // Call via lookup
    link if lookup_sel => y = add_lookup.add_1(x);
    link if lookup_sel => z = add_lookup.add_2(x);

    // Call via permutation
    link if perm_sel ~> a = add_perm.add_42(x);
    link if perm_sel ~> b = add_perm.add_21(x);
}
