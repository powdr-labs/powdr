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

### High-level language

The third layer is a high-level language that has all the usual features of a regular programming language like loops,
functions and local variables. How to map it to a user-defined instruction set is not clear yet, but it would be nice
to at least relieve the user from having to assign registers, jump labels and so on.

It might be possible to define interlieve assembly code like we do with Solidity/Yul and then provide a set of simplification
rules specific to an instruction set.

