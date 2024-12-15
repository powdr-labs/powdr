# Expressions

Depending on the context, powdr allows more or less features for expressions.

Inside values for declarations, you can use a very flexible language which includes
many different operators, function calls, lambda functions, tuple types, statement blocks,
match statements and others.

In statements and expressions that are required to evaluate to constraints / polynomial identities, only a much more restrictive
language can be used. Expressions in that language are called Algebraic Expressions. While you can use
the full language everywhere, in the context of a constraint, the result after function evaluation
and constant propagation has to be an algebraic expression.

## Generic Expressions

The expression language allows the following operators, in order of increased precedence:

- lambda functions: ``|params| body``. Examples: ``|i| i`` (the identity), ``|a, b| a + b`` (sum)
- ``||`` - logical or
- ``&&`` - logical and
- ``<``, ``<=``, ``==``, ``!=``, ``>=``, ``>`` - comparisons and ``=`` - identity operator
- ``|`` - bitwise or
- ``^`` - bitwise xor
- ``&`` - bitwise and
- ``<<``, ``>>`` - left and right shift
- ``+``, ``-`` - addition and subtraction (binary operator)
- ``*``, ``/``, ``%`` - multiplication, division and modulo
- ``**`` - exponentiation
- ``-``, ``!`` - numerical and logical negation (unary operators, prefix)
- ``'`` - "next row" operator (suffix)
- ``[]``, ``()`` - array index access and function calls

Elementary expressions are
- number literals (integers)
- string literals, written in double quotes, e.g. ``"hello"``
- array literals written in square brackets, e.g. ``[1, 2, 3]``
- tuples, having at least two elements, e.g. `(1, "abc")`
- statement blocks (see below)
- match expressions (see below).
- if expressions (see below).

Parentheses are allowed at any point to force precedence.

### Lambda Functions

The only way to declare a function in pil is by assigning a lambda function to a symbol.

Example:

```rust
let x = |i| i + 1;
```

If you want to specify the types of parameters or return values explicitly, you have to do it
on the symbol, you cannot do it on the parameters:

```rust
let x: int -> int = |i| i + 1;
```

It is possible to use [patterns](./patterns.md) in the function parameters:

```rust
let y: (int, int), int -> int = |(i, j), _| i + j;
```

If you use patterns, they have to be irrefutable, which means that the pattern has to
be able to match any value of the given type.

### Statement Blocks

A ``{``-``}``-delimited block can be used everywhere where an expression is expected.

It has the form ``{ <statement> ; <statement> ; ... ; <expression> }``,
i.e. a sequence of statements followed by an expression.
The statements can either be expressions (``f();``, only inside [``constr``-functions](#constr-and-query-functions))
or let statements: ``let x = ...;`` / ``let x;``

The value of the statement block is the value of the final expression.

Example:

```rust
let plus_one_squared = |x| { let y = x + 1; y * y };
```

Let statements with value can be used everywhere, they just bind an expression to a local variable
and allow to avoid repeating the expression. You can use [patterns](./patterns.md) for the
left-hand side of let statements to destructure values.

Example:

```rust
let f = |i| (i / 2, i % 2);
let (quot, rem) = f(7);
```

The second let statement will create two local variables `x` and `y`. You can also ignore values using
the `_` pattern element. For details, please see the [patterns](./patterns.md) section.

Let statements without value (``let x;``) create a new witness column and are only allowed inside [``constr``-functions](#constr-and-query-functions).

Similarly, an expression at statement level (e.g. ``x * (x - 1) = 0;``) can be used to create new constraints that are added to the global constraint set
and this can only be done inside a [``constr``-functions](#constr-and-query-functions).

Note that you can always create constraints and return them from a function, even in [pure function](#constr-and-query-functions).

Example:

```rust
let constrain_to_bool: expr -> Constr = |x| x * (x - 1) = 0;
```


### Match Expressions

Match expressions take the form ``match <value> { <pattern 1> => <value 1>, <pattern 2> => <value 2>, _ => <default value> }``,
with an arbitrary number of match arms.

The semantics are that the first match arm where the pattern equals the value after the `match` keyword is evaluated.

Patterns can be used to destructure more complex data types and to capture values inside new local variables.
For more details, please see the [patterns](./patterns.md) section.

Example:

```rust
let fib = |i| match i {
    0 => 1,
    1 => 1,
    _ => fib(i - 2) + fib(i - 1),
};
```


### If Expressions

If expressions take the form ``if <condition> { <true value> } else { <false value> }``, where the "else" part is not optional.

If the condition evaluates to ``true``, then `<true value>` is evaluated, otherwise `<false value>` is.


Example:

```rust
let is_seven = |i| if i == 7 { 1 } else { 0 };
```

## Algebraic Expressions

For constraints (or functions called at a place where a constraint is expected), the expression syntax is limited:
After evaluating function calls and performing constant propagation, the resulting expression has to
be an "algebraic expression". These are restricted in the following way:

- You can freely use the operators  ``+``, ``-``,``*``.
- The operator ``**`` must have a number as exponent.
- The operator `[i]` must have a column name on the left-hand side and the index must be a number.
- The operator `'` must have a column or `[i]` on the left-hand-side.
- No other operators are allowed.

Arbitrary parentheses are allowed.

The following example illustrates how you can still use the generic language:

```rust
{{#include ../../../test_data/pil/book/generic_to_algebraic.pil}}
```

## Constr and Query Functions

Every function in PIL is either a pure, a `constr` or a `query` function. They are denoted by

- `|...| ...`
- `constr |...| ...`
- `query |...| ...`

Inside `constr` functions, it is possible to create new witness columns
and add constraints to the set of constraints (see the [Statement Blocks](#statement-blocks) section for details).

Inside `query` functions, it is possible to evaluate the value of a column on the "current" row
using the `std::prover::eval` function.

Both actions require a certain context to be available, which is not the case for example when
the values of a fixed column are computed.

A `query` function can only be used in the query or hint part of a witness column while `constr` functions
can only be evaluated in the constraint part of a namespace or machine.

You can define and call new `constr` functions inside a `constr` function and you can call and define
new `query` functions inside `query` functions, but as soon as you enter a pure function, this is not possible any more.

Examples:
    

```rust
// This function creates and returns a new witness column.
let new_wit = constr || { let x; x };
// Queries the current value of a column and returns its square.
let square_of = query |x| { let v = std::prover::eval(x); v * v };
// Creates a new witness column, constrains it to be boolean and returns it.
let new_bool = constr |x| { let x = new_wit(); x * (x - 1) = 0; x };
// This is a pure function that only returns a constraint, but does not add it
// to the global set of constraints.
let bool_constraint: expr -> Constr = |x| x * (x - 1) = 0;
```
