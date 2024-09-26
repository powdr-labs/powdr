mod other {
    enum E0 {
    }
    enum E1 {
        A,
    }
    enum E2 {
        A,
    }
}
trait Foo<F> {
    foo: other::E1 -> other::E2,
}
impl Foo<other::E0[3]> {
    foo: |e| {
        e = other::E1::A;
        other::E2::A
    },
}
