# Functions

Machine functions are the entry points to a machine. They can be called from another machine or from the outside.

For static machines, functions simply indicate which columns should be exposed as inputs and outputs. They do not contain statements, since the business logic of all functions in a static machine is defined directly in constraints. We refer to the previous example [here](./machines.md#static-machines).

In the rest of this section, we describe functions in dynamic machines with the example of this simple machine:

```
{{#include ../../../test_data/asm/book/function.asm:all}}
```

## Function inputs and outputs

> For dynamic machines, function inputs and outputs are not supported yet

## Statements

### Labels

Labels allow referring to a location in a function by name.

```
{{#include ../../../test_data/asm/book/function.asm:label}}
```

### Assignments

Assignments allow setting the value of a write register to the value of an [expression](#expressions) using an assignment register.

```
{{#include ../../../test_data/asm/book/function.asm:instruction}}
```

One important requirement is for the assignment register of the assignment to be compatible with that of the expression. This is especially relevant for instructions: the assignment register of the instruction output must match that of the assignment. In this example, we use `Y` in the assignment as the output of `square` is `Y`:

```
{{#include ../../../test_data/asm/book/function.asm:square}}
```

### Instructions

Instructions which do not return outputs can be used as statements.

```
{{#include ../../../test_data/asm/book/function.asm:instruction_statement}}
```