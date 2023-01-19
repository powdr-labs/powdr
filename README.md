# powdr
an extended polynomial identity language (PIL) in rust


## Ideas

This is a random list of ideas that help designing the language.
Most if this is heavily inspired by zkASM / PIL of the polygon/hermez team.

Main goal: Everything is written in the same language, if possible not even spread across multiple files.

### Constant Definitions

Define constants directly in the pil file:

```
pol constant BYTE = |i| i % 0xff;
```

Constants can also depend on each other:

```
pol constant A = |i| i & 0xffff;
pol constant B = |i| (i / 0x10000) & 0xffff;
pol constant SUM = |i| (A[i] + B[i]) & 0xffff;
pol constant OVERFLOW = |i| (A[i] + B[i]) >> 16;
```

By just declaring A and B to be of type u16, it might not be needed to define them?
There should be a way to create a "cross product" of constants somehow, so that the definition of A and B
above is trivial.
This could also help to combine two lookps into one.

### Types

Polynomials are typed (which adds a constraint automatically unless it can be proven that it is not needed due to a lookup):

```
pol commit isJump: bool; // creates constraint "isJump * (1-isJump) = 0;"
```

There can be user-defined types for enums or bitfields.


### Templates

The language should have as few built-in as possible. There should be ways to define functions or templates, for example the following:

```
fun<T> ite(c: bool, a: T, b: T) -> T = c * (a - b) + b;
```

There should be ways to handle complex types like arrays and structs efficiently.

### Instruction / Assembly language

The second layer of this langauge is to define an assembly-like language that helps in defining complex constants.

The zkASM language of polygon/hermez can be used as a basis, but with the following changes:

- The number of registers needs to be user-defined
- The way instructions are mapped to constraints has to be user-defined (i.e. the instructions themselves are user-defined)
- The execution process and the constraints generated from an instruction have to be defined at the same place.
- The correspondence between assembly identifiers and polynomials has to be direct.

Example from polygon-hermez zkASM:

```
$               :ADD, MSTORE(SP++), JMP(readCode)
```

The `$` means that the "input" is computed depending on the instruction on the right hand side.
To understand the data flow, the implicit inputs and outputs of the instructions must be known:

- `ADD` reads from registers `A` and `B` and outputs in the implicit default register. It also invokes a state machine.
- `MSTORE` writes to memory and reads from the implicit default register
- `JMP` only handles control-flow, but we could also have `JMPN` here which would read from the implicit default register

Combining these is fine as long as for every register, there is only one writing to that register.

It might be better to make the order of the instructions explicit (i.e. put them in separate statements) and rely on an
optimizer to combine instructions into a single step if they are not conflicting.

A better notation might be:

```
X := ADD(A, B);
MSTORE(SP++, X);
JMP(readCode);
```

If we assume we have an optimizer that combines instructions, we could also think about an optimizer that performs automatic register
compression, so that we could use an arbitrary number of registers in the source code, but specify a certain (smaller) number of
registers for the target architecture. It would not be possible to move registers to memory in that case, but the optimizer would just
report an error if the number of used registers is too large.

Further things to help that has to be done manually in assembly: Allow non-recursive calls to functions such that only the registers
needed by the caller are saved and restored from memory locations.

### High-level language

The third layer is a high-level language that has all the usual features of a regular programming language like loops,
functions and local variables. How to map it to a user-defined instruction set is not clear yet, but it would be nice
to at least relieve the user from having to assign registers, jump labels and so on.

It might be possible to define interlieve assembly code like we do with Solidity/Yul and then provide a set of simplification
rules specific to an instruction set.

