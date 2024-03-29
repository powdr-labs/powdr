# Declarations

Symbols can be defined via ``let <name> = <value>;``, or via ``let <name>: <type> = <value>;`` if you want to
specify the type explicitly. The value is an arbitrary [PIL-expression](../pil/expressions.md).
For details, see the [Declarations](../pil/declarations.md) section in the PIL part.

Other symbols available in the current module can be accessed by name, but it is also possible to specify
full relative paths in the form of e.g. ``super::super::module_name::symbol``.

Here are some examples of how to define and use symbols:

```
{{#include ../../../test_data/asm/book/declarations.asm}}
```