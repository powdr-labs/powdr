/// Inverts `x` in the finite field with modulus `modulus`.
/// Assumes that `modulus` is prime, but does not check it.
let inverse = |x, modulus|
    if x <= 0 || x >= modulus {
        std::check::panic("Tried to compute the inverse of zero, of a negative number or a number outside the field.")
    } else {
        // TODO this is written in a complicated way
        // because we do not have tuple destructuring assignment
        (|r|
            if r[0] < 0 { r[0] + modulus } else { r[0] }
        )(extended_gcd(x, modulus))
    };

/// Computes `x + y` modulo the modulus.
let add = |x, y, modulus| reduce(x + y, modulus);

let double = |x, modulus| add(x, x, modulus);

/// Computes `x - y` modulo the modulus.
let sub = |x, y, modulus| reduce(x - y, modulus);

/// Computes `x * y` modulo the modulus.
/// TODO maybe just computing "(x * y) % modulus" is faster...
let mul = |x, y, modulus|
    match y {
        0 => 0,
        1 => x,
        _ =>
            add(
                if y & 1 == 1 { x } else { 0 },
                mul(x, y >> 1, modulus) << 1,
                modulus
            )
    };

/// Computes `x / y` modulo the modulus.
let div = |x, y, modulus| mul(x, inverse(y, modulus), modulus);

let reduce = |x, modulus|
    if x < 0 {
        reduce(x + modulus, modulus)
    } else {
        if x >= modulus {
            reduce(x - modulus, modulus)
        } else {
            x
        }
    };

let extended_gcd = |a, b|
    if b == 0 {
        if a == 1 {
            [1, 0]
        } else {
            // a is the gcd, but we do not really want to compute it.
            std::check::panic("Inputs are not co-prime, inverse does not exist.")
        }
    } else {
        // TODO this is written in a complicated way
        // because we do not have tuple destructuring assignment
        (|r| [r[1], r[0] - (a / b) * r[1]])(extended_gcd(b, a % b))
    };