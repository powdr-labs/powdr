# Machines

Machines are the first main concept in powdr asm. They can currently be of two types: dynamic or static.

## Dynamic machines

Dynamic machines are defined by:
- a degree, indicating the number of execution steps
- a set of registers, including a program counter
- an instruction set
- constraints
- a set of functions
- a set of submachines

> Dynamic machines are currently limited to having a single function called `main`

An example of a simple dynamic machine is the following:

```
{{#include ../../../test_data/asm/book/hello_world.asm}}
```

## Static machines

Static machines are a lower-level type of machine. They do not have registers, and instead rely on simple committed and fixed columns. They are used to implement hand-optimized computation.

They are defined by:
- a degree, indicating the number of execution steps
- a set of functions
- a function identifier column, used to make constraints conditional over which function is called
- a latch column, used to identify rows at which the machine can be accessed from the outside (where the inputs and outputs are passed)

An example of a simple static machine is the following:

```
{{#include ../../../test_data/asm/book/simple_static.asm}}
```

For more details on the constraints, check out the [pil](../pil) section of this book. Note that the parameters of the function are columns declared within the constraints block.

## Submachines

Machines can have submachines which they access by defining [external instructions](./instructions.md). They are declared as follows:

```
machine MySubmachine {
    ...
}

machine MyMachine {
    MySubmachine my_submachine;
}
```

> Currently only dynamic machines can have submachines, and these must be static