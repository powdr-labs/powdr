mod other {
    enum E1 {
        A,
    }

    struct S1 {
        a: int,
        b: E1,
    }
}

use other::E1;
use other::S1;

let s1 = S1{ a: 1, b: E1::A };