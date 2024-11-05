# Functions

Functions are the entry points to a virtual machine. They can be called from another machine or from the outside.

In this section, we describe functions with this simple virtual machine:

```rust
{{#include ../../../test_data/asm/book/function.asm:all}}
```

## Function inputs and outputs

> Function inputs and outputs are not supported yet

## Statements

### Labels

Labels allow referring to a location in a function by name.

```rust
{{#include ../../../test_data/asm/book/function.asm:label}}
```

### Assignments

Assignments allow setting the values of some write registers to the values of some expressions [expression](#expressions) using assignment registers.

```rust
{{#include ../../../test_data/asm/book/function.asm:literals}}
```

If the right-hand side of the assignment is an instruction, assignment registers can be inferred and are optional:

```rust
{{#include ../../../test_data/asm/book/function.asm:instruction}}
```

This will be inferred to be the same as `A, B <=Y, Z= square_and_double(A);` from the definition of the instruction:

```rust
{{#include ../../../test_data/asm/book/function.asm:square_and_double}}
```

### Instructions

Instructions which do not return outputs can be used as statements.

```rust
{{#include ../../../test_data/asm/book/function.asm:instruction_statement}}
```