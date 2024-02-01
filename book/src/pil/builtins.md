# Built-ins

## Functions

The following functions are built into the compiler.
They need to be defined to be accessible, but their assigned value is ignored and the compiler replaces it with the following.

### Array length

```rust
let<T> std::array::len: T[] -> int
```

Returns the length of an array as an integer.

Example:
```rust
let x = [1, 2, 3];
let l = std::array::len(x); // returns 3
```

### Panic

```rust
let std::check::panic: string -> !
```

Aborts evaluation and prints its argument as error message as a
side-effect of its evaluation.

Since `panic` does not generate a constraint, it cannot be used
for correctness checks. The verifier only checks constraints / identities and
thus ignores anything that could lead to a panic. Panic should only
be used to check prover-internal consistency.

Example:
```rust
let secp256k1_inverse = |x|
    if x == std::convert::fe(0) {
        panic!("Tried to compute the inverse of zero.")
    } else {
        std::math::ff::inverse(x, secp256k1_modulus);
    };
```

### Conversions

```rust
let<T: FromLiteral> std::convert::fe: T -> fe
```

This function is meant to be used on `int`, but also works on `fe` for convenience.

It converts a non-negative integer less than the field modulus to a field element.
Causes a type error in all other cases.

If the argument is already a field element, it is returned without modification.

```rust
let<T: FromLiteral> std::convert::int: T -> int
```

This function is meant to be used on `fe`, but also works on `int` for convenience.

It converts a field element to an integer.

If the argument is already an integer, it is returned without modification.

### Printing

```rust
let std::debug::print: string -> constr[]
```

This function takes a string and prints it on the standard output during evaluation, as a side-effect of its evaluation.

This function should only be used for debugging purposes.

Note that the function does not append a newline at the end.

It returns an empty `constr` array so that it can be used at statement level where
constraints are expected.

### Modulus

```rust
let std::field::modulus: -> int
```

Returns the current field's modulus as an integer.

Example:
```rust
// Inside a machine
if std::field::modulus() != 2**64 - 2**32 + 1 {
    panic!("This machine can only be used with the Goldilocks field.")
} else {
    []
};
```

## Operators

The following operators are supported by powdr-pil with their respective signatures.

```
let<T: Add> +: T, T -> T
let<T: Sub> -: T, T -> T
let<T: Neg> -: T -> T

let<T: Mul> *: T, T -> T
let /: int, int -> int
let %: int, int -> int
let<T: Pow> **: T, int -> T

let <<: int, int -> int
let >>: int, int -> int
let &: int, int -> int
let |: int, int -> int
let ^: int, int -> int

let<T: Ord> <: T, T -> bool
let<T: Ord> <=: T, T -> bool
let<T: Ord> >: T, T -> bool
let<T: Ord> >=: T, T -> bool
let<T: Ord> <: T, T -> bool

let<T: Eq> ==: T, T -> bool
let<T: Eq> !=: T, T -> bool

let =: expr, expr -> constr
let ': expr -> expr

let ||: bool, bool -> bool
let &&: bool, bool -> bool
let !: bool -> bool
```