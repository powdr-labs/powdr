mod cols {
    let first: -> expr = {
        constr || {
            let fi: col = |i| if i == 0 { 1 } else { 0 };
            fi
        }
    };
    enum Generic<T> {
        A(T),
        B,
    }
    let<T> f: T -> Generic<T> = |i| {
        let g: -> Generic<T> = || {
            Generic::A(i)
        };
        g()
    };
}


machine Empty {
    col witness w;
    w - cols::first() = 0;
    let x;
    let fi: col = |i| match cols::f(i) {
        cols::Generic::A(_) => 7,
        cols::Generic::B => 9,
    };
    x = fi;

}