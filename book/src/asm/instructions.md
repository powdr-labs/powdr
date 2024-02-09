# Instructions

Instructions are declared as part of a powdr virtual machine. Their inputs and outputs are [assignment registers](./registers.md) as well as labels. Once defined, they can be called by any function in this machine.

# Local instructions

A local instruction is the simplest type of instruction. It is called local because its behavior is defined using constraints over registers and columns of the machine it is defined in.

```
{{#include ../../../test_data/asm/book/instructions.asm:local}}
```

Instructions feature:
- a name
- some inputs
- some outputs
- a set of [powdr-pil](../pil/) constraints to activate when the instruction is called

# External instructions

An external instruction delegates its implementation to a function/operation from a submachine.
When called, the inputs and outputs of the declared instruction are mapped into a call to the submachine function.

Assume we have a submachine with a single operation `add`:
```
{{#include ../../../test_data/asm/book/instructions.asm:submachine}}
```

An external instruction calling into this operation can be declared as follows:
```
{{#include ../../../test_data/asm/book/instructions.asm:trivial}}
```
The left-hand side of the definition declares the local instruction and which assignment registers are used for its inputs and outputs.
The right-hand side of the definition specifies the external operation being called and how it should be called.
All assignment registers on the left-hand side must be used in the call to the external operation.
Additionally, regular registers can also be used in the call (can be seen as implicit inputs/outputs of the instruction).

In the previous example, parameters of the instruction match exactly with how the target operation should be called, and the right-hand parameters can thus be omitted.
The following example shows more complex usages of external instructions:
```
{{#include ../../../test_data/asm/book/instructions.asm:main}}
```

> Note that external instructions cannot currently link to functions of the same machine: they delegate computation to a submachine.

