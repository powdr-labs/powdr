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
- ``<``, ``<=``, ``==``, ``!=``, ``>=``, ``>`` - comparisons
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
- match expressions (see below).
- if expressions (see below).

Parentheses are allowed at any point to force precedence.

### Match Expressions

Match expressions take the form ``match <value> { <pattern 1> => <value 1>, <pattern 2> => <value 2>, _ => <default value> }``,
with an arbitrary number of match arms.

The semantics are that the first match arm where the pattern equals the value after the `match` keyword is evaluated.
The "default" arm with the pattern `_` matches all values.

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
