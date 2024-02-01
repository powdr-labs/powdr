mod utils {
    // This defines a function by means of a lambda expression that
    // computes the sum of an array of values. We fully specify its type.
    let sum: int, int[] -> int = |len, arr| match len {
        0 => 0,
        _ => arr[len - 1] + sum(len - 1, arr)
    };
    // A simple function that returns the input incremented by one,
    // as an expression.
    let incremented: expr -> expr = |x| x + 1;
    // This is a function that takes an expression as input and returns
    // a constraint enforcing this expression increments by a certain value
    // between rows.
    // The type will be inferred here because `'` is only valid on `expr`.
    let constrain_incremented_by = |x, inc| x' = x + inc;
}

machine Main {
    // Machines create local scopes in the way functions create local scopes:
    // - all symbols in the machine's module are available without prefix,
    // - new symbols can be defined but are only available inside the machine.
    reg A;
    reg pc[@pc];

    // This defines a witness column,
    let x;
    // and now we force it to stay unchanged.
    utils::constrain_incremented_by(x, 0);

    // We define an instruction that uses a complicated way to increment a register.
    instr incr_a { A = utils::incremented(A) }

    function main {
        return;
    }
}