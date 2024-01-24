# Built-in Functions

The following functions are built into the compiler.
They need to be defined to be accessible, but their assigned value is ignored and the compiler replaces it with the following.

### Array length

`std::array::len: _[] -> int`

Returns the length of an array as an integer.

Example:
```rust
let x = [1, 2, 3];
let l = std::array::len(x); // returns 3
```

### Panic

`std::check::panic: string -> !`

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

`std::convert::fe: int -> fe`

Converts a non-negative integer less than the field modulus to a field element.
Causes a type error in all other cases.

If the argument is already a field element, it is returned without modification.

`std::convert::int: fe -> int`

Converts a field element to an integer.

If the argument is already an integer, it is returned without modification.

### Printing

`std::debug::print: string -> []`

This function takes a string and prints it on the standard output during evaluation, as a side-effect of its evaluation.

This function should only be used for debugging purposes.

Any non-string argument is converted to a string first and then printed.

Note that the function does not append a newline at the end.

It returns an empty array so that it can be used at statement level where
constraints are expected.

### Modulus

`std::field::modulus: -> int`

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
