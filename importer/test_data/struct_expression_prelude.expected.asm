mod std {
    mod prelude {
        enum Option<T> {
            None,
            Some(T),
        }
        enum E1 {
            A,
        }
        struct S1 {
            a: int,
            b: std::prelude::E1,
        }
    }
}
mod other {
    struct S2 {
        x: std::prelude::Option<int>,
        y: std::prelude::S1,
    }
}
let s1 = std::prelude::S1{ a: 1, b: std::prelude::E1::A };
let s2 = other::S2{ x: std::prelude::Option::Some(1), y: s1 };
