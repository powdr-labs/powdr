# Registers

Registers are central to a machine. powdr supports a few types of registers:

## Program counter

Each machine can have at most one program counter. In the absence of a program counter, the machine is considered static, and no other register can be declared. The program counter is defined as follows:

```
reg pc[@pc]
```

At each step execution step, the program counter points to the [function](./functions.md) line to execute.
The program counter behaves like a [write register](#write-registers), with the exception that its value is incremented by default after each step.

## Write registers

Write registers are the default type for registers. They are declared as follows:

```
{{#include ../../../test_data/asm/book/write_register.asm:declaration}}
```

They hold a field element, are initialized as 0 at the beginning of a function and keep their value by default. They can be read from and written to.

```
{{#include ../../../test_data/asm/book/write_register.asm:component}}
```

## Assignment registers

Assignment registers are transient to an execution step: their value is not persisted across steps. They are required in order to pass inputs and receive outputs from instructions, as well as in assignments.
For example, if we want to assert that write register `A` is `0`, we can use the following instruction:
```
{{#include ../../../test_data/asm/book/assert_write_register.asm:component}}
```
However, if we want the instruction to accept any write register as input, we use an assignment register.
```
{{#include ../../../test_data/asm/book/assert_assignment_register.asm:component}}
```

## Read-only registers

Read-only registers are used for function inputs. However, powdr creates them automatically based on functions arguments, so that they do not need to be declared explicitly.

<<<<<<< HEAD
=======
We apply the following transformation
```
// Before:

{{#include ../../../test_data/asm/book/asm_function_before.asm}}

// After:

{{#include ../../../test_data/asm/book/asm_function_after.asm.ignore}}
```

>>>>>>> e0effa1 (implement static-to-static calls)
> Read-only registers are only mentioned for completeness here and are currently only used inside the compiler. We advise against using them.