enum O<X> {
    A(X),
    B,
}
let<T> f: T -> O<T>[] = |i| [O::A(i)];
let<Q> x: Q -> O<Q>[][] = |i| {
    let y: Q[] = f(i);
    [y]
};
let q: int -> int = |i| {
    let r = x(i);
    match r {
        [[O::A(x)]] => i,
    }
};
machine Main with degree: 4 {
    col witness w;
    let f: col = q;
    w = f * f;
}
