let identity: expr -> expr = |expr| expr;
let N: int = 32;
machine Id with degree: N {
    operation id<0> x, y;
    pol commit x;
    pol commit y;
    x = y;
}
machine Main with degree: N {
    ::Id id;
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    instr id X, l: label -> Y link => X = id.id(identity(l)) link => Y = id.id(identity(Y)){ Y = identity(X) }
    link => X = id.id(identity(X));
}
