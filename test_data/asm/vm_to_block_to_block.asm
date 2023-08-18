machine Inc(latch, function_id) {

    degree 8;

    function inc<0> x -> y {
    }

    constraints {
        col fixed latch = [1]*;
        col witness x;
        col witness y;
        y = x + 1;
    }
}

machine Assert1(latch, function_id) {

    degree 8;

    Inc inc;

    function assert1<0> x {
    }

    constraints {
        col fixed latch = [1]*;
        col witness x;
        col witness y;

        // Increment x by calling into inc machine
        {x, y} in main_assert1_inc.latch {main_assert1_inc.x, main_assert1_inc.y};
        // Assert that result is 2
        y = 2;
    }
}

machine Main {

    degree 8;

    Assert1 assert1;

    reg pc[@pc];
    reg X[<=];
    reg A;

    instr assert1 X -> = assert1.assert1

    instr loop {
        pc' = pc
    }

    function main {
        assert1(1);
        loop;
    }
}