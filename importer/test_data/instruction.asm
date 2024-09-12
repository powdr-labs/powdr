let identity: expr -> expr = |expr| expr;

machine Id {
    operation id<0> x, y;

    col witness x;
    col witness y;
    x = y;
}

machine Main {

    Id id;

    reg pc[@pc];
    reg X[<=];
    reg Y[<=];

    instr id X, l: label -> Y
        link => X = id.id(identity(l))
        link => Y = id.id(identity(Y))
    {
        Y = identity(X)
    }

    link => X = id.id(identity(X));
}