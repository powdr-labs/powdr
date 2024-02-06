# Declarations

Powdr-pil allows the same syntax to declare various kinds of symbols. This includes
constants, fixed columns, witness columns and even higher-order functions. It deduces the symbol kind
from the type of the symbol and the way the symbol is used.

Symbols can be declared using ``let <name>;`` and they can be declared and defined
using ``let <name> = <value>;``, where ``<value>`` is an expression. The [type](./types.md) of the symbol
can be explicitly specified using ``let <name>: <type>;`` and ``let <name>: <type> = <value>;``.

This syntax can be used for constants, fixed columns, witness columns and even (higher-order)
functions that can transform expressions. The kind of symbol is deduced by its type and the
way the symbol is used:

- Symbols without a value are witness columns. Their type can be omitted. If it is given, it must be ``int -> fe`` or its shorthand ``col``.
- Symbols evaluating to a number or with type ``fe`` are constants.
- Symbols without type but with a value that is a function with a single parameter are fixed columns.
- Symbols defined with a value and type ``int -> fe`` or its shorthand ``col`` are also fixed columns.
- Everything else is a "generic symbol" that is not a column or constant.

Examples:


```rust
{{#include ../../../test_data/pil/book/declarations.pil:declarations}}
```