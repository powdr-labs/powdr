mod tools {
    let identity: expr -> expr = (|expr| expr);
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
    instr id X -> Y link => X = id.id(tools::identity(X)) link => Y = id.id(tools::identity(Y)){     Y = tools::identity(X) }
    link => X = id.id(tools::identity(X));
}
