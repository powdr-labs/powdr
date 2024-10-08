mod other {
    enum E1 {
        A,
    }

    struct S1 {
        a: int,
        b: E1,
    }

    struct S2 {
        x: Option<int>,
        y: S1,
    }
}

use other::E1;
use other::S1;
use other::S2;

let s1 = S1{ a: 1, b: E1::A };
let s2 = S2{ x: Option::Some(1), y: s1 };