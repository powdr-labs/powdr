mod cols {
    enum Generic<T> {
        A(T),
        B,
    }
    let<T> f: -> Generic<T> = || {
            Generic::B
    };
}


machine Empty {
    let x;
    let fi: col = |i| match cols::f::<int>() {
        cols::Generic::A(_) => 7,
        cols::Generic::B => 9,
    };
    x = fi;

}