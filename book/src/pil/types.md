# Types

The powdr-pil language has the following types:

- `bool`
- `int` (integer)
- `fe` (field element)
- `string`
- tuple
- array
- function type
- expression
- constraint

Since powdr-pil does not yet have a compile-time type checker, it is only dynamically typed:
Values during evaluation have a (runtime) type and operations on them are performed according to this section.
If an expression (for example in the definition of a fixed column) references a column, that column
is assumed to have the type `int -> fe` (i.e. a function mapping integers (row indices) to field elements).

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

Integers in powdr-pil have unlimited size. Literal numbers are always integers.
Array index requires an integer and row indices (for example the input to a fixed
column defined through a function) are also integers.

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

Field elements are the values stored in (fixed, witness and intermediate) columns.
Arithmetic inside constraints (algebraic expressions) is also always finite field arithmetic.

Since literal numbers in the source code are integers, those have to be converted to field elements
using the `std::convert::fe` function. In some situations (see below), integers
(including literal numbers) are converted to field elements implicitly.

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

Due to the dynamic typing of powdr-pil, it is often not possible to exactly determine the type of a lambda expression.

Functions allow the following operators:

- `_(...)`: function evaluation

Powdr-pil is usually side-effect free, but there are some built-in functions that have
side-effects:
These are `std::debug::print` and `std::check::panic` and all functions that call them.
Expressions are eagerly evaluated from left to right.

### Expression

Type name: `expr`

Expressions are the elements of the algebraic expressions used in constraints.
Expressions cannot be explicitly constructed. At statement level / in constraint contexts,
references to columns are implicitly converted to expressions, as are integers and field elements.

Example:
```rust
let x;
let y;
let f = || x + y;
let g = || 7;
f() == g();
```
The first two lines define the witness columns `x` and `y`.
The next two lines define the utility functions `f` and `g`.
The last line is at statement level and it is expected that it evaluates to a constraint, in this case, a polynomial identity.
Because of that, `f()` is expected to evaluate to an expression, which means that the references
to `x` and `y` that appear when evaluating `f` are already turned into expressions.
The same is true about the `7` inside `g`.

If `f` is called in a different context (for example inside a function that defines a constant column),
then the column references `x` and `y` are interpreted to have the type `int -> fe` and thus
adding them results in a type error.

Since expressions are built from abstract column references, applying operators
does not perform any operations but instead constructs an abstract expression structure / syntax tree.

Expressions allow the following operators, which always construct new expressions:
- `+`: additive combination of expressions
- `-`: subtractive combination of expressions (also unary negation)
- `*`: multiplicative combination of expressions
- `**`: exponential combination of an expression with an integer constant
- `'`: reference to the next row of a column, can only be applied directly to columns and only once

The operator `==` on expressions constructs a constraint (see below);


### Constraints

Type name: `constr`

Any evaluation of an expression at statement level needs to result in a constraint,
or in an array of constraints.

Generally, constraints include polynomial identities, plookups, permutations and connection identities.

The only constraint currently constructible in the powdr-pil language are polynomial identities.
These can be constructed from expressions by applying the `==` operator as in `x == 7`.

Constraints do not allow any operators.


## Implicit Conversions

Usually, type conversions all have to be explicit using the built-in functions in the
`std::convert` module, but there are some situations in which implicit type conversions happen:

### Conversion to Expression

If an expression is evaluated starting from statement level (i.e. the result is expected to be
a constraint), references to columns are
converted to expressions. If the column is a fixed column, it can still be called as
a function with signature `int -> fe`.

If an expression is combined with another value using a binary operator, the other value
is converted to an expression according to the following rules:

- field elements are converted without modification
- non-negative integers less than the field modulus are converted to a field element and then to an expression
- all other values cause a type error

### Conversion to Field Elements

Fixed columns have the signature `int -> fe` and all return values of
such functions are implicitly converted to field elements according to the following rules:

- field elements are returned without modification
- non-negative integers less than the field modulus are converted to field elements
- all other values cause a type error