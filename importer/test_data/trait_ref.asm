mod m {
    trait X<T> {
        a: -> T,
    }
}

use m::X;
use m::X::a;

trait Foo<F> {
    foo: int -> F,
}

let t = || {
    let g: int = a();
    let h: int = X::a();
    let i = m::X::a();
    let j: fe = Foo::foo(h);
};