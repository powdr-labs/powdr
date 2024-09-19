# Types

The powdr-pil language has the following types:

- `bool`
- `int` (integer)
- `fe` (field element)
- `string`
- tuple
- array
- function type
- `expr` (expression)
- `!` ("bottom" or "unreachable" type)
- enum types

> In addition, there are the `col` and `inter` types, but they are special in that
> they are only used for declaring columns, but cannot appear as the type of an expression.
> See [Declaring and Referencing Columns](#declaring-and-referencing-columns) for details.

Powdr-pil performs Hindley-Milner type inference. This means that, similar to Rust, the type of
a symbol does not always have to be specified. The compiler will try to find a type for every
symbol depending both on the value assigned to the symbol and on the context the symbol is used in.
It is an error if the type is not uniquely determined.

Symbols can have a generic type, but in those cases, you have to explicitly specify the generic type.
Such declarations can require type variables to satisfy certain trait bounds.
Currently, only built-in traits are supported (see the next section).

Literal numbers do not have a specific type, they can be either `int`, `fe` or `expr` (the types that
implement the `FromLiteral` trait), and their type can also stay generic until evaluation.

### Example

The following snippet defines a function that takes a value of a generic type and returns the value incremented by one.
The type bounds on the generic type are `FromLiteral` and `Add`. The type checker will complain if we do not specify
the type bounds. The bound `Add` is required because we use the `+` operator in the function and `FromLiteral` is needed
because we use the literal `1` as a value of that type.

```rust
let<T: FromLiteral + Add> add_one: T -> T = |i| i + 1;
```

## Declaring and Referencing Columns

A symbol declared to have type `col` or `inter` (or `col[k]` / `inter[k]`) is a bit special:

These symbols represent columns in the arithmetization and the types of values that can be assigned to
such symbols and the references to the symbols are different from their declared type.

If you assign a value to a `col` symbol, that value is expected to have type `int -> fe` or `int -> int` (or an array thereof).
This allows the simple declaration of a fixed column `let byte: col = |i| i & 0xff;` without complicated conversions.
The integer value is converted to a field element during evaluation, but it has to be non-negative and less than
the field modulus.

Symbols of declared type `col` are fixed (those with value) or witness columns (those without value).

A symbol of declared type `inter` is an intermediate column. You can assign it a value of type `expr`.
The idea of an intermediate column is that it is an algebraic expression of other columns that you do
not want to compute multiple times.

> Note that if you use `let x: expr = a * b;`, the symbol `x` is just a name in the PIL environment,
> this will not create an intermediate column. The difference between `inter` and `expr` in this case
> is that if you use `let x: inter = ...`, the expression might not be inlined into constraints (depending on the backend),
> while if you use `let x: expr = ...`, it will always be inlined.

If you reference a symbol of declared type `inter` or `col`, the type of the reference is `expr` (or `expr[]`).
A byte constraint is as easy as `[ X ] in [ byte ]`, since the expected types in plookup columns is `expr`.
The downside is that you cannot evaluate columns as functions. If you want to do that, you either have to assign
a copy to an `int -> int` symbol: `let byte_f: int -> int = |i| i & 0xff; let byte: col = byte_f;`.
Or you can use the built-in function `std::prover::eval` if you want to do that inside a prover query or hint.

All other symbols use their declared type both for their value and for references to these symbols.

## Built-in Traits

`FromLiteral`:
Implemented by `int`, `fe`, `expr`. The type of a number literal needs to implement `FromLiteral`.

`Add`: Implemented by `int`, `fe`, `expr`, `T[]`, `string`. Used by `<T: Add> +: T, T -> T` (binary plus).

`Sub`:
Implemented by `int`, `fe`, `expr`. Used by `<T: Sub> -: T, T -> T` (binary minus).

`Neg`:
Implemented by `int`, `fe`, `expr`. Used by `<T: Neg> -: T -> T` (unary minus).

`Mul`:
Implemented by `int`, `fe`, `expr`. Used by `<T: Mul> *: T, T -> T` (binary multiplication).

`Pow`:
Implemented by `int`, `fe`, `expr`, Used by `<T: Pow> **: T, int -> T` (exponentiation).

`Ord`:
Implemented by `int`. Used by `<T: Ord> op: T, T, -> bool` for `op` being one of `<`, `>`, `<=`, `>=`.

`Eq`:
Implemented by `int`, `fe`, `expr`. Used by `<T: Eq> op: T, T -> bool` for `op` being one of `==`, `!=`.


## List of Types

### Bool

Type name: `bool`

Booleans are the results of comparisons. They allow the following operators:

- `&&`: logical conjunction
- `||`: logical disjunction
- `!`: logical negation

Short-circuiting is *not* performed when evaluating boolean operators.
This means that `(1 == 1) || std::check::panic("reason")` will cause a panic abort.

### Integer

Type name: `int`

Integers in powdr-pil have unlimited size.
Array index requires an integer and row indices (for example the input to a fixed
column defined through a function) are also integers.

Integer implements `FromLiteral`, which means that literal numbers can be used in contexts where `int` is expected.

Integers allow the following operators, whose result is always an integer:

- `+`: addition
- `-`: subtraction (also unary negation)
- `*`: multiplication
- `/`: integer division rounding towards zero, division by zero results in a runtime error
- `**`: exponentiation, the exponent needs to be non-negative and fit 32 bits, otherwise a runtime error is triggered
- `%`: remainder after division, (for signed arguments, `p % q == sgn(p) * abs(p) % abs(q)`), remainder by zero results in a runtime error
- `&`: bit-wise conjunction
- `|`: bit-wise disjunction
- `^`: bit-wise exclusive or
- `<<`: bit-wise shift left, the shift amount needs to be non-negative and fit 32 bits, otherwise a runtime error is triggered
- `>>`: bit-wise shift right, the shift amount needs to be non-negative and fit 32 bits, otherwise a runtime error is triggered

The exponentiation operator on field elements requires a non-negative integer as exponent.
It has the signature `**: fe, int -> fe`.

In addition, the following comparison operators are allowed, the result is a boolean:
- `<`: less than
- `<=`: less or equal
- `==`: equal
- `!=`: not equal
- `>=`: greater or equal
- `>`: greater than

### Field Element

Type name: `fe`

Field elements are elements of a particular but unspecified prime field. The exact field is
chosen when powdr is run. The modulus of that field can be accessed via `std::field::modulus()`.

Field elements are the values stored in (fixed and witness) columns.
Arithmetic inside constraints (algebraic expressions) is also always finite field arithmetic.

The type `fe` implements `FromLiteral`, which means that literal numbers can be used in contexts where `fe` is expected.
If the literal number is not less than the field modulus, a runtime error is caused.

Field elements allow the following operators, where the result is always a field element:

- `+`: finite field addition
- `-`: finite field subtraction (also unary negation)
- `*`: finite field multiplication

There is also an exponentiation operator `**` on field elements. It requires the exponent
to be a non-negative integer and thus has the signature `**: fe, int -> fe`. If the exponent is negative, a runtime error is triggered. `0**0` is defined as `1`.

The following comparison operators exist for field elements, whose result is a boolean:

- `==`: equality comparison
- `!=`: inequality comparison

Since finite fields do not have an inherent order as integers do, if you want to
compare them using `<`, you have to first convert them to integers.

### String

Type name: `string`

String literals are written as `"string content"`. They are mainly used for debugging or
documentation purposes, since they cannot occur in constraints.

They allow the following operators:

- `+`: string concatenation

### Tuple

Type name: `(..., ..., ...)`

Tuples are complex types that are composed from other types, either zero or two or more. There is no tuple type with a single element (`(int)` is the same as `int`).
The empty tuple type is written as `()`.

Examples include `(int, int)` (a pair of integers) and `((fe[], int), ())`
(a tuple consisting of a tuple that contains an array of field elements and an integer
and an empty tuple).

Tuples values are constructed using parentheses: `(1, 2)` constructs the tuple that consists of
a one and a two.

Tuples do not allow any operators.

### Array

Type name: `_[]`

Arrays are statically or dynamically-sized collections of elements each of the same type
denoted for example as `int[]` (a dynamically-sized array of integers) or `int[2]` (an array of integers with static size two).
Array values can be constructed inline using `[1, 2]` (the array containing the
two elements one and two).

The built-in function `std::array::len` can be used to retrieve the length of an array (statically or dynamically sized)
and the elements of an array `a` can be accessed using `a[0]`, `a[1]`, etc.

The type checker currently only knows dynamically-sized arrays, which means that it does not compare the sizes of statically-sized array types.

Arrays allow the following operators:

- `+`: array concatenation
- `_[]`: array index access, the index needs to be a non-negative integer that is less than the length of the array, otherwise a runtime error is triggered


### Function

Type name: `T1, T2, ..., Tn -> T0`

Function type names are for example denoted as `int, fe -> int` or `-> int`.
Note that `(int, fe) -> int` is a function that takes a single tuple as parameter
while `int, fe -> int` takes two parameters of type integer and field element.

Functions can be constructed using the lambda expression notation.
For example `|x, y| x + y` returns a function that performs addition.
The lambda expression `|| 7` is a function that returns a constant (has no parameters).
Lambda functions can capture anything in their environment and thus form closures.

Functions allow the following operators:

- `_(...)`: function evaluation

Powdr-pil is usually side-effect free, but there are some built-in functions that have
side-effects:
These are `std::debug::print` and `std::check::panic` and all functions that call them.
Expressions are eagerly evaluated from left to right.

### Expression

Type name: `expr`

Expressions are the elements of the algebraic expressions used in constraints.

References to columns have type `expr` and `expr` also implements `FromLiteral`,
which means that literal numbers can be used in contexts where `expr` is expected.

Example:
```rust
let x: col;
let y: col;
let f: -> expr = || x + y;
let g = || 7;
f() = g();
```
The first two lines define the witness columns `x` and `y`.
The next two lines define the utility functions `f` and `g`.
The function `f` adds the two columns `x` and `y` symbolically - it essentially returns the expression `x + y`.
The last line is at statement level and it is expected that it evaluates to a constraint, in this case, a polynomial identity.
Because of that, `g` is inferred to have type `-> expr`, which is compatible with the literal `7`.

Since expressions are built from abstract column references, applying operators
does not perform any operations but instead constructs an abstract expression structure / syntax tree.

Expressions allow the following operators, which always construct new expressions:
- `+`: additive combination of expressions
- `-`: subtractive combination of expressions (also unary negation)
- `*`: multiplicative combination of expressions
- `**`: exponential combination of an expression with an integer constant
- `'`: reference to the next row of a column, can only be applied directly to columns and only once

The operator `=` on expressions constructs a constraint (see [../builtins#Constr]).

### Bottom Type

Type name: `!`

The bottom type essentially is the return type of a function that never returns, which currently only happens
if you call the `panic` function. The bottom type is compatible with any other type, which means
that you can call the `panic` function in any context.

### Enum Types

Enums are user-defined types that can hold different named alternatives plus data. An enum type has a (namespaced) name
that uniquely identifies it and is also used to reference the type.

Enums are declared in the following way:

```rust
enum EnumName {
    Variant1,
    Variant2(),
    Variant3(int),
    Variant4(int, int[], EnumName),
}
```

The variants must have unique names inside the enum and they can optionally take additional data.
Each variant declares a type constructor function that can be used to create a value of the enum:

```rust
let a = EnumName::Variant1;
let b = EnumName::Variant2();
let c = EnumName::Variant3(3);
let d = EnumName::Variant4(1, [2, 3], EnumName::Variant1);
```

Recursive enums are allowed.

Enums do not allow any operators.
