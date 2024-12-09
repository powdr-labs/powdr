# Expressions

## Field element literals

Field element literals are signed elements of the prime field.

```rust
{{#include ../../../test_data/asm/book/function.asm:literals}}
```

## Registers and columns

Registers can be used as expressions, with the exception of assignment registers.
```rust
{{#include ../../../test_data/asm/book/function.asm:read_register}}
```

## Instructions

Instructions which return outputs can be used as expressions.
```rust
{{#include ../../../test_data/asm/book/function.asm:instruction}}
```