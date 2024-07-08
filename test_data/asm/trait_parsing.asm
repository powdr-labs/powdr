mod types {
    enum DoubleOpt<T> {
        None,
        Some(T, T)
    }

    trait ArraySum<T, DoubleOpt<T> > {
        array_sum: T[4 + 1] -> DoubleOpt,
    }
}


machine Empty {
    col witness w;
    w = w * w;
}