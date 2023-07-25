# Expressions

## Field element literals

Field element literals are signed elements of the prime field.

```
{{#include ../../../test_data/asm/book/function.asm:literals}}
```

## Registers and columns

Registers can be used as expressions, with the exception of assignment registers.
```
{{#include ../../../test_data/asm/book/function.asm:read_register}}
```

## Instructions

Instructions which return outputs can be used as expressions.
```
{{#include ../../../test_data/asm/book/function.asm:instruction}}
```