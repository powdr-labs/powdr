# Declarations

Powdr-pil allows the same syntax to declare various kinds of symbols. This includes
constants, fixed columns, witness columns and even macros. It deduces the symbol kind
by its type and the way the symbol is used.

Symbols can be declared using ``let <name>;`` and they can be declared and defined
using ``let <name> = <value>;``, where ``<value>`` is an expression.
This syntax can be used for constants, fixed columns, witness columns and even (higher-order)
functions that can transform expressions. The kind of symbol is deduced by its type and the
way the symbol is used:

- symbols without a value are witness columns,
- symbols evaluating to a number are constants,
- symbols defined as a function with a single parameter are fixed columns and
- everything else is a "generic symbol" that is not a column.

Examples:

```rust
// This defines a constant
let rows = 2**16;
// This defines a fixed column that contains the row number in each row.
let step = |i| i;
// Here, we have a witness column.
let x;
// This functions returns the square of its input (classified as a fixed column).
let square = |x| x*x;
// A recursive function, taking a function and an integer as parameter
let sum = |f, i| match i {
    0 => f(0),
    _ => f(i) + sum(f, i - 1)
};
// The same function as "square" above, but employing a trick to avoid it
// being classified as a column.
let square_non_column = (|| |x| x*x)();
```