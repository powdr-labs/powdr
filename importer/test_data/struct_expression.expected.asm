mod other {
    enum E1 {
        A,
    }
    struct S1 {
        a: int,
        b: other::E1,
    }
}
let s1 = other::S1{ a: 1, b: other::E1::A };
