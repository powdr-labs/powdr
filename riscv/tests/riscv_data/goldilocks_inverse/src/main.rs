#![no_main]
#![no_std]

use powdr_riscv_runtime::goldilocks::{Goldilocks, PRIME};

#[no_mangle]
fn main() {
    // Inverse of 1 is 1.
    assert_eq!(u64::from(Goldilocks::new(1).inverse()), 1);

    // Inverse of -1 is -1.
    assert_eq!(u64::from(Goldilocks::new(PRIME - 1).inverse()), PRIME - 1);

    // A list of values and their inverses.
    let cases = [
        (923978, 17687434846476327257),
        (235763497586, 13400833429194399868),
        (9827635653498,     16674998396781901341),
        (112870, 2318958550553056941),
        (289273673480943876, 13653526925771117123),
        (230295874986745876, 4470486487297737924),
        (6254867324987, 15986711629902721810),
        (2087, 16263540530772800743)
    ].map(|(value, inverse)| (Goldilocks::new(value), Goldilocks::new(inverse)));

    // The inversion must work both ways.
    for (value, inverse) in cases.iter() {
        assert_eq!(value.inverse(), *inverse);
        assert_eq!(inverse.inverse(), *value);
    }
}
