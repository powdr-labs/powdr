mod m {
    trait X<T> {
        a: -> T,
    }
}
trait Foo<F> {
    foo: int -> F,
}
let t = || {
    let g: int = m::X::a();
    let h: int = m::X::a();
    let i = m::X::a();
    let j: fe = Foo::foo(h);
};
