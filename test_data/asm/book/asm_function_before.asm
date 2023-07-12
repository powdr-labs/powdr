machine Machine {
    reg pc[@pc];

    function foo x: field, y: field -> field {
        return x + y;
    }
}