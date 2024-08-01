# Declarations

Powdr-pil allows the same syntax to declare various kinds of symbols. This includes
constants, fixed columns, witness columns and even higher-order functions. It deduces the symbol kind
from the type of the symbol and the way the symbol is used.

Symbols can be declared using ``let <name>;`` and they can be declared and defined
using ``let <name> = <value>;``, where ``<value>`` is an expression. The [type](./types.md) of the symbol
can be explicitly specified using ``let <name>: <type>;`` and ``let <name>: <type> = <value>;``.
Symbols with a generic type can be defined using ``let<TV1, TV1, ..> <name>: <type> = <value>;``,
where the `TV` are newly created type variables that can be used in the type.

This syntax can be used for constants, fixed columns, witness columns and even (higher-order)
functions that can transform expressions. The kind of symbol is deduced by its type and if
it is has a value or not:

- Symbols without a value are witness columns or arrays of witness columns. Their type can be omitted. If it is given, it must be ``col`` or ``col[k]``.
- Symbols defined with a value and type ``col`` (or ``col[k]``) are fixed columns (or arrays of fixed columns).
- Symbols defined with a value and type ``inter`` (or ``inter[k]``) are intermediate columns (or arrays of intermediate columns).
- Everything else is a "generic symbol" that is not a column.

Examples:


```rust
{{#include ../../../test_data/pil/book/declarations.pil:declarations}}
```

Name lookup is performed as follows:

Lookup is performed starting from the current namespace, going up to the root component by component
where the first match is used. If all lookups fail, a last attempt is done
inside the ``std::prelude`` namespace.