machine Main with degree: 16 {
    DifferentSignatures sub;

    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg A;

    instr identity X -> Y link => Y = sub.identity(X);
    instr one -> Y link => Y = sub.one();
    instr nothing link => sub.nothing();

    function main {
        start:
        A <== one();
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
