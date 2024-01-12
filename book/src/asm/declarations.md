# Declarations

Symbols can be defined via ``let <name> = <value>;``. The value is a generic [PIL-expression](../pil/expressions.md)
and its type is unconstrained (it can be a value, a function or even a higher-order function).

Other symbols available in the current module can be accessed by name, but it is also possible to specify
full relative paths in the form of e.g. ``super::super::module_name::symbol``.

Here are some examples of how to define and use symbols:

```
{{#include ../../../test_data/asm/book/declarations.asm}}
```