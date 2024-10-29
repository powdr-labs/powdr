# Instructions

Instructions are declared as part of a powdr virtual machine.
Once defined, they can be called by any function in this machine.
An instruction is composed of:
- a name
- a set of inputs ([assignment registers](./registers.md) or labels)
- a set of outputs (assignment registers)
- a set of [powdr-pil](../pil/) constraints to activate when the instruction is called
- a set of links calling into functions/operations in submachines

# Local instructions

A local instruction is the simplest type of instruction.
It is called local because its behavior is defined by a set of constraints over registers and columns of the machine it is defined in.

```rust
{{#include ../../../test_data/asm/book/instructions.asm:local}}
```

# Instructions with links

Instructions may also delegate all or part of their implementation to functions/operations in submachines.
Each `link` in an instruction defines the inputs and outputs of a call to a specific function/operation in a submachine.

Assume we have a submachine with a single operation `add`:
```rust
{{#include ../../../test_data/asm/book/instructions.asm:submachine}}
```

An instruction calling into this operation can be declared as follows:
```rust
{{#include ../../../test_data/asm/book/instructions.asm:trivial}}
```

In the previous example, only assignment registers (instruction inputs and outputs) were used to call the submachine.
The following example shows more complex usage of `link` calls:
```rust
{{#include ../../../test_data/asm/book/instructions.asm:main}}
```

A single instruction can activate multiple `links`, and may also include a set of constraints.
Furthermore, each link can be activated conditionally, based on a given boolean flag:
```rust
{{#include ../../../test_data/asm/book/instructions2.asm:main}}
```

> Note that links cannot currently call functions from the same machine: they delegate computation to a submachine.

