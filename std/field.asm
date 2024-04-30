/// A function that returns the current field modulus as an integer.
/// The actual implementation is replaced by a built-in function.
let modulus: -> int = [];

let GOLDILOCKS_PRIME: int = 0xffffffff00000001;
let BN254_PRIME: int = 0x30644e72e131a029b85045b68181585d2833e84879b9709143e1f593f0000001;

/// All known fields
enum KnownField {
    Goldilocks,
    BN254
}

/// Checks whether the function is called in a context where it is operating on
/// any of the known fields.
let known_field: -> Option<KnownField> = || if modulus() == GOLDILOCKS_PRIME {
    Option::Some(KnownField::Goldilocks)
} else {
    if modulus() == BN254_PRIME {
        Option::Some(KnownField::BN254)
    } else {
        Option::None
    }
};