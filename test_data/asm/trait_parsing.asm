mod types {
    enum DoubleOpt<T> {
        None,
        Some(T, T)
    }

    trait ArraySum<T> {
        array_sum: T[4 + 1] -> DoubleOpt<T>,
    }
}


machine Empty with degree: 4 {
    col witness w;
    w = w * w;
}