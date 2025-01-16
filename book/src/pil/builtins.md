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

```rust
let<T: FromLiteral> std::convert::expr: T -> expr
```

This function is meant to be used on `int`, but also works on `fe` and `expr` for convenience.

It converts an integer to an expr.

If the argument is already an expr, it is returned without modification.

### Printing

```rust
let std::debug::print: string -> Constr[]
```

This function takes a string and prints it on the standard output during evaluation, as a side-effect of its evaluation.

This function should only be used for debugging purposes.

Note that the function does not append a newline at the end.

It returns an empty `Constr` array so that it can be used at statement level where
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

### Evaluate

```rust
let std::prover::eval: expr -> fe
```

Evaluates a column (potentially with `'` applied) on the current row.

This function can only be used for prover queries or hints and it only
works on columns (and those with `'` applied). This means you cannot use
`std::prover::eval(x + 1)`.

In the following example, the column `x` is evaluated in a prover
hint that returns the square root of a number.
Example:
```rust
machine Sqrt {
    let sqrt_hint: fe -> fe = |x| match x {
        // Code to compute the square root of x goes here.
    };

    col witness x;
    col witness y(i) query std::prelude::Query::Hint(sqrt_hint(std::prover::eval(x)));

    y * y = x;

}}
```


### Challenges

```rust
let std::prelude::challenge: int, int -> expr
```

Constructs a challenge object, essentially asking the verifier for a random number.

The first argument is the proof stage and the second is the identifier of the challenge.

If you want two challenges to be different, you have to choose different IDs.


### Degree

```rust
let std::prover::min_degree: -> int
let std::prover::max_degree: -> int
let std::prover::degree: -> int
```

The `degree` function returns the number of rows / the length of the witness columns, also
known as the degree. Outside of fixed column definitions, `degree` fails if `min_degree` and `max_degree` are different.

### Hints

```rust
let std::prelude::set_hint: expr, (int -> std::prelude::Query) -> ()
```

This function can be used to set a "query function" for a witness column.
Query functions are used during witness generation and allow witness column cells
to receive a value even though they are not uniquely constrained by the constraints.

The first argument must be a witness column and the function can only be called
once per witness column.

## Types

There are some types that are not proper built-in types (in the sense that they are not treated
specially in the type system), but they are defined in the
standard library and are referenced by built-in functions.

### Constr

The type `Constr` or more specifically, `std::prelude::Constr` is the type of a constraint.
Expressions at statement level are required to evaluate either to `Constr` or `Constr[]`.

It is defined as follows:

```rust
enum Constr {
    /// A polynomial identity.
    Identity(expr, expr),
    /// A lookup constraint with selectors.
    Lookup((Option<expr>, Option<expr>), (expr, expr)[]),
    /// A permutation constraint with selectors.
    Permutation((Option<expr>, Option<expr>), (expr, expr)[]),
    /// A connection constraint (copy constraint).
    Connection((expr, expr)[])
}
```

The operator `=` can be applied on two `expr` values and results in a `Constr::Identity`.

The type implements no traits and allows no operators.


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

let<T: Eq> ==: T, T -> bool
let<T: Eq> !=: T, T -> bool

let =: expr, expr -> Constr
let ': expr -> expr

let ||: bool, bool -> bool
let &&: bool, bool -> bool
let !: bool -> bool
```
