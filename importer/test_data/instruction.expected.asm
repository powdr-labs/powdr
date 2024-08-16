mod too {
    mod ls {
        let identity: expr -> expr = (|expr| expr);
    }
}
machine Id {
    operation id<0> x, y;
        pol commit x;
        pol commit y;
        x = y;
}
machine Main {
    ::Id id;
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    instr id X, l: label -> Y link => X = id::id(too::ls::identity(l)) link => Y = id::id(too::ls::identity(Y)){     Y = too::ls::identity(X) }
    link => X = id::id(too::ls::identity(X));
}
