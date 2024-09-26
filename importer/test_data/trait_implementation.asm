mod other {
    enum E0 {

    }
    enum E1 {
        A,
    }
    enum E2 {
        A
    }
}

use other::E0;
use other::E1;
use other::E2;

trait Foo<F> {
    foo: E1 -> E2,
}

impl Foo<E0[3]> {
    foo: |e| {
        e = E1::A;
        other::E2::A
    }
}