machine Main {

    degree 16;

    DifferentSignatures sub;

    reg pc[@pc];
    reg X[<=];
    reg Y[<=];

    instr identity X -> Y = sub.identity
    instr one -> Y = sub.one
    instr nothing = sub.nothing

    function main {
        nothing;
        return;
    }
}

// A machine exposing functions of different signatures
machine DifferentSignatures {

    reg pc[@pc];

    function identity x: field -> field {
        return x;
    }

    function one -> field {
        return 1;
    }

    function nothing {
        return;
    }
}