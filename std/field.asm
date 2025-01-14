/// A function that returns the current field modulus as an integer.
/// The actual implementation is replaced by a built-in function.
let modulus: -> int = [];

let BN254_PRIME: int = 0x30644e72e131a029b85045b68181585d2833e84879b9709143e1f593f0000001;
let GOLDILOCKS_PRIME: int = 0xffffffff00000001;
let KOALABEAR_PRIME: int = 2**31 - 2**24 + 1;
let BABYBEAR_PRIME: int = 0x78000001;
let M31_PRIME: int = 2**31 - 1;

/// All known fields
enum KnownField {
    BN254,
    Goldilocks,
    KoalaBear,
    BabyBear,
    M31
}

/// Checks whether the function is called in a context where it is operating on
/// any of the known fields.
let known_field: -> Option<KnownField> = || if modulus() == BABYBEAR_PRIME {
    Option::Some(KnownField::BabyBear)
} else {
    if modulus() == KOALABEAR_PRIME {
        Option::Some(KnownField::KoalaBear)
    } else {
        if modulus() == GOLDILOCKS_PRIME {
            Option::Some(KnownField::Goldilocks)
        } else {
            if modulus() == BN254_PRIME {
                Option::Some(KnownField::BN254)
            } else {
                if modulus() == M31_PRIME {
                    Option::Some(KnownField::M31)
                } else {
                    Option::None
                }
            }
        }
    }
};

let require_known_field: KnownField, (-> string) -> () = |f, err| match (f, known_field()) {
    (KnownField::BN254, Option::Some(KnownField::BN254)) => (),
    (KnownField::Goldilocks, Option::Some(KnownField::Goldilocks)) => (),
    (KnownField::KoalaBear, Option::Some(KnownField::KoalaBear)) => (),
    (KnownField::BabyBear, Option::Some(KnownField::BabyBear)) => (),
    (KnownField::M31, Option::Some(KnownField::M31)) => (),
    _ => std::check::panic(err()),
};