# Operations

Operations enable a constrained machine to expose behavior to the outside.
If a machine has a single operation, it can simply be declared with its name and parameters:
```rust
{{#include ../../../test_data/asm/book/operations_and_links.asm:one_operation}}
```
The parameters of the operation (inputs and outputs) must be columns declared in the machine.

If a machine exposes more than one operation, the machine itself needs an operation id column (`op_id` in the following).
Then, each operation needs to be declared with its own unique operation id:
```rust
{{#include ../../../test_data/asm/book/operations_and_links.asm:many_operations}}
```

The actual behavior of an operation is defined by the machine constraints on the columns used as inputs and outputs.
