# Patterns

Patterns are a way to destructure or match certain values. They are valid in `match` arms,
function parameters or left hand sides of let statements in blocks.

A pattern is built up from the following components:

- `_` - the "catch all" pattern that matches anything
- `x` - for an identifier `x`, matches anything and assigns the value to the new local variable of that name
- `k` - for a literal number `k`, matches the exact number, either as an `int` or a `fe`
- `-k` - for a literal number `k`, matches the exact negated number, either as an `int` or a `fe`
- `"text"` - for a string literal, matches the exact string literal as a `string`
- `(a, b, c)` - for a tuple, matches a tuple-typed value if all the components match
- `[a, b, c]` - for an array, matches array values of exactly the same length if all the components match
- `[a, .., b, c]` - matches an array that has an initial segment of `a` and ends in `b, c`. The omitted part can be empty.
- `X::Y(a, b)` - for an enum variant `X::Y`, matches that enum variant if all the enum fields match.

Patterns can be nested, which means that the components of tuple and array patterns
are themselves patterns.

Some examples:

```rust
// This pattern de-structures the first function parameter.
let f: (int, int), int -> int = |(a, b), c| (a + c, b);
// Matches a tuple, ignores the second component.
let (x, _) = f((6, 7), 3);
// The match statement typically uses patterns to check for certain values
// but it can also destructure and create new local variables valid inside
// the match arm.
let t = match (x, f((1, x), 2)) {
    (0, _) => 0,
    (1, _) => 7,
    (_, y) => y,
    _ => 9
};
let head: int[] -> int = |x| match x {
    // Matches the first element of a non-empty array and binds it to a local variable.
    [a, ..] => a,
    [] => std::check::panic("Called 'head' on empty array."),
};
```

> Note that PIL does not check that patterns in a match expression are exhaustive.

## (Ir-)refutability

A pattern is refutable if there is a value of the correct type that the pattern does not match.
An example is the pattern `7` since it does not match all integers, or the patten `[x, ..]`, because it
does not match the empty array.

Refutable patterns are fine in match arms, because if the pattern does not match, the evaluator will
just continue trying the next match arm, but they are disallowed in let statements and in function
parameters, because there we do not have the option of "trying the next arm".

Example:
```rust
let f: int -> int[] = |i| match i {
    // This is a refutable pattern, but it is fine
    // because we will try the next match arm.
    0 => [],
    _ => f(i - 1) + [i],
};
// This pattern does not match all `int[]`, because it requires a length
// of at least one.
let [x, ..] = f(8);
```

The following patterns are refutable:

- all integer literal patterns
- all string literal patterns
- enum variant patterns
- tuple patterns that have refutable components
- array patterns that are not `[..]`.

Variable patterns and `_` are always irrefutable.
