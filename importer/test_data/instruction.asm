mod tools {
    let identity: expr -> expr = |expr| expr;
}

use tools::identity;

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

    instr id X -> Y
        link => X = id.id(identity(X))
        link => Y = id.id(identity(Y))
    {
        Y = identity(X)
    }

    link => X = id.id(identity(X));
}