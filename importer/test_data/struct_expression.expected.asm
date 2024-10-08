mod other {
    enum E1 {
        A,
    }
    struct S1 {
        a: int,
        b: other::E1,
    }
    struct S2 {
        x: std::prelude::Option<int>,
        y: other::S1,
    }
}
let s1 = other::S1{ a: 1, b: other::E1::A };
let s2 = other::S2{ x: std::prelude::Option::Some(1), y: s1 };
