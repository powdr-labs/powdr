# powdr
an extended polynomial identity language (PIL) and zk-focused assembly in Rust.

WARNING: This is a proof-of concept implementation. It is missing many internal checks. DO NOT USE FOR PRODUCTION!

## Basic Concept

powdr is a toolkit that helps build zkVMs and similar proof systems.

It has two main components:

- A polynomial identity language that allows you to define polynomial constraints, lookups, etc.
- An extensible assembly language to perform dynamic executions.
  
Both frontend and backend are highly flexible.

As an example, powdr contains a frontend that enables you to write code in (no-std) Rust,
which gets compiled to RISCV, then to powdr-assembly and finally to PIL.

All stages of this execution are fully automatic, which means you do not need to write
additional code for witness generation. All witnesses are automatically inferred from the constraints.
Since the witnesses are inferred, powdr can ensure that the system is not underconstrained,
i.e. there are no additional unwanted witnesses.

All artifacts during this compilation process are human-readable. This means you can take a look at
the RISCV assembly file, the powdr assembly file and the PIL file.

The assembly language is designed to be extensible. This means that it does not have a single
native instruction. Instead, all instructions are user-defined and because of that,
it is easy to adapt powdr assembly to a different kind of VM.


### How to run the Rust-RISCV example

```sh
# Install the riscv target for the rust compiler
rustup target add riscv32imc-unknown-none-elf
# Run the compiler. It will generate files in /tmp/.
# -i specifies the prover witness input (see below)
cargo run --release rust riscv/tests/riscv_data/sum.rs -o /tmp -f -i 10,2,4,6 
```

The following example Rust file verifies that a supplied list of integers sums up to a specified value.
Note that this is the full and only input file you need for the whole process!
```rust
#![no_std]

use core::arch::asm;

#[no_mangle]
pub extern "C" fn main() -> ! {
    let mut buffer = [0u32; 100];
    let proposed_sum = get_prover_input(0);
    let len = get_prover_input(1) as usize;
    assert!(len > 0 && len < 100);
    for i in 0..len {
        buffer[i] = get_prover_input(2 + i as u32);
    }
    let sum: u32 = buffer[..len].iter().sum();
    assert!(sum == proposed_sum);
    loop {}
}

#[inline]
fn get_prover_input(index: u32) -> u32 {
    let mut value: u32;
    unsafe {
        asm!("ecall", lateout("a0") value, in("a0") index);
    }
    value
}
```

The function `get_prover_inputs` reads a number from the list supplied with `-i`.

This is just a first mechanism to provide access to the outside world.
The plan is to be able to call arbitrary user-defined ffi functions that will translate to prover queries,
and can then ask for e.g. the value of a storage slot at a certain address or the
root hash of a merkle tree.

### Notes on Efficiency

Currently, the code is extremely wasteful. It generates many unnecessary columns.
The idea is to first see if automatic witness generation is possible in general.
If this is confirmed, various optimizer stages will be built to reduce the
column (and row) count automatically.

## Ideas

This is a random list of ideas that help designing the language.
Most if this is heavily inspired by zkASM / PIL of the polygon/hermez team.

Main goal: Everything is written in the same language, if possible not even spread across multiple files.

### Constant Definitions

Define constants directly in the pil file:

```
pol constant BYTE(i) { i % 0xff };
```

Constants can also depend on each other:

```
pol constant A(i) { i & 0xffff };
pol constant B(i) { (i / 0x10000) & 0xffff };
pol constant SUM(i) { (A[i] + B[i]) & 0xffff };
pol constant OVERFLOW(i) { (A[i] + B[i]) >> 16 };
```

By just declaring A and B to be of type u16, it might not be needed to define them?
There should be a way to create a "cross product" of constants somehow, so that the definition of A and B
above is trivial.
This could also help to combine two lookps into one.

#### Cross-Product Brainstorming

It might come in handy to not explicitly define all the constant polynomials but instead implicitly define them
in the lookup:

```
(op, a, b, c) in OP: {ADD, SUB, MUL} x A: u16 x B: u16 x C: u16 where match OP {
    ADD => A + B == C,
    SUB => A - B == C,
    MUL => A * B == C,
};
```

The lookup is composed of
```
LEFT in RIGHT where FUN;
```
Where `LEFT` is a tuple of committed polynomials, `RIGHT` is an `x`-product of tuples of constant polynomials, expressions or variable declarations
and `FUN` is a function of the variables that returns `bool`.

The semantics is as follows: If there is more than one factor, then all factors has to have finite size such that
the product of the sizes is less than the maximal polynomials size. New constant polynomials are constructed,
so that there is at least one row for all combinations of rows in the factor (cross product).
If `FUN` is present, then all rows where the function returns `false` are removed.

In the example above, we first construct four constant polynomials. These will not be used in the end, but a
"stretched" version of them, but it will become clearer if you think of them like tables in a database
and the lookup constructs a query. The first is `OP` - it just has three rows: `ADD`, `SUB`, `MUL`.
The second is `A` which has one row for each value between `0` and `2**16-1`. The constant polynomials for `B`
and `C` are identical to `A`.
The cross-product then first constructs polynomials of size `3 * 65536 * 65536 * 65536` such that the four-tuple
contains all combinations of rows. The function finally reduces the polynomials to size `3 * 65536 * 65536`,
because onyl one value of `C` is valid for each `OP`-`A`-`B`-combination.

### The "Polynomial" Terminology

While the terminology makes sense looking at the final encoding, polynomials should probably be called something
else, since they do not really resemble polynomials. The confusion is more apparent when you allow in-line definitions.

The keyword `col` can be used as an alias for `pol`. The qualifiers `commit` and `constant` for polynomials
(but not for number constants) can be replaced by `witness` and `fixed`, respectively:

```
constant %N = 16;

namespace Fibonacci(%N);
    col fixed ISLAST(i);
    col witness x, y;
```

### Types

Polynomials are typed (which adds a constraint automatically unless it can be proven that it is not needed due to a lookup):

```
pol commit isJump: bool; // creates constraint "isJump * (1-isJump) = 0;"
```

There can be user-defined types for enums or bitfields.

### Underconstrained systems and Nondeterminism

Ideally, the language should not allow under constrained systems in the high
level case.  Take the state machine (SM) abstraction, for example. Some
polynomials are left underconstrained because the code that uses the SM
constrains it on that side (1). In other cases, lookups are responsible for
constraining the polynomials (2).

#### Pre-conditions

In (1), the user should state in the SM that those polynomials have
pre-conditions that match all applications of that SM, and convince the
compiler that the system is not underconstrained.

### Post-conditions

Similarly, in (2), users should also show when the combintation of lookups have
certain properties.


### Templates

The language should have as few built-in as possible. There should be ways to define functions or templates, for example the following:

```
fn<T> ite(c: bool, a: T, b: T) -> T = c * (a - b) + b;
```

There should be ways to handle complex types like arrays and structs efficiently:

```
fn<T> mul(a: T[], b: T[]) -> T[] = [a[i] * b[i] | i: 0..a.len()];
```

We will stick as much to Rust as possible for now. This means there is a trait for the multiplication operator that we define.

### Macros

As a "quick and dirty" hack, we implemented syntactic macros for now:

```
macro ite(C, A, B) { C * A + (1 - C) * B }
```

Macros can evaluate to zero or more statements (constraints / identities) and
zero or one expression.
The statements are terminated by `;` and the last element is the expression.
Macros can of course also invoke other macros:

```
macro bool(X) { X * (1 - X) = 0; }
macro ite(C, A, B) { bool(C); C * A + (1 - C) * B }
```

In the example above, `bool` evaluates to one polynomial identity constraint and no expression.
The macro `ite` adds the identity constraint generated through the invocation of `bool`
to the list and evaluates to an expression at the same time.

If a macro is used in statement context, it cannot have an expression and
if it is used in expression context, it must have an expression (but can also have statements).

The optimizer will of course ensure that redundant constraints are removed
(be it because the are just duplicated or because they are already implied by lookups).

### Instruction / Assembly language

The second layer of this langauge is to define an assembly-like language that helps in defining complex constants.

A more detailed description of the current plans can be found in [notes_asm.md](notes_asm.md).

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
X <= ADD(A, B);
MSTORE(SP++, X);
JMP(readCode);
```

If we assume we have an optimizer that combines instructions, we could also think about an optimizer that performs automatic register
compression, so that we could use an arbitrary number of registers in the source code, but specify a certain (smaller) number of
registers for the target architecture. It would not be possible to move registers to memory in that case, but the optimizer would just
report an error if the number of used registers is too large.

Further things to help that has to be done manually in assembly: Allow non-recursive calls to functions such that only the registers
needed by the caller are saved and restored from memory locations.

Counters about which state machine is invoked how often should be automatically maintained.

#### Syntax and Defining Instructions

A powdr-asm file is a list of statements and labels. The main built-in concept is those of the program counter (`pc`).
It is possible to define functions (those usually relate to state machines defined in PIL) and instructions
(which typically modify control-flow). It is possible to perform certain computations on registers, but those will be
carried out in the base field and on each register element separately (TODO improve this).

Here is an example program followed by the definitions of the functions and instructions:
```
  A <= mload(B)
  A <= add(A, B)
repeat:
  Z <= eq(B, 0)
  jmpi Z out
  A <= mul(A, 2)
  B <= sub(B, 1)
  jmp repeat
out:
```

And here are the definitions of the instructions (and some others) - they probably need to be put in a different file or at least at the start of the file:
```
instr jmp l: label { pc' = l }
instr jmpi c: bool, l: label { pc' = c * l + (1 - c) * pc }
instr call l: label { rr' = pc + 1; pc' = l }
instr ret { pc' = rr }
fun eq(A, B) -> C { C <= binary(0, A, B) }
fun add(A, B) -> C { C <= binary(1, A, B) }
fun sub(A, B) -> C { C <= binary(2, A, B) }
fun mul(A, B) -> C { C <= binary(3, A, B) }
fun binary(op, A, B) -> C {
    {op, A, B, C} is {Binary.op, Binary.A, Binary.B, Binary.C}
}
```

During compilation, each instruction is turned into a flag (a bit inside a larger value) and the definition of the instructions are turned
into something like the following constraints (they will of course be optimized further) and lookups:
```
pc' = jmp * jmp_arg1 + jmpi * (c * jmpi_arg2 + (1 - jmpi_arg1) * pc) + regular * (pc + 1);
binary {op, A, B, C} in {Binary.op, Binary.A, Binary.B, Binary.C};
binaryCounter' = binaryCounter + binary;
```
The constants for the program are filled accordingly and there is a second set of committed polynomials that
corresponds to the execution and they are matched with a lookup. The information above is everything the prover
needs to fill the committed polynomials.


TODO: How to connect to state machines? Is it really possible to have arbitrary inputs or should we assume the inputs
to be in certain registers? The same is true for instructions - it might be much more efficient to have them access fixed registers.
One main benefit is that it allows to squash together multiple instructions that use different registers.

Partial answer to the second question: Instructions could have "immediate arguments" in the sense that
the arguments are not totally free expressions but taken from a fixed list of values / registers.
These could also be seen as overloads of the instructions: Add an arbitrary value to the value in register A (fixed register),
load from memory pointed to by the stack pointer and increase it (`B = mload SP++`) or not (`B = mload SP`).

### High-level language

The third layer is a high-level language that has all the usual features of a regular programming language like loops,
functions and local variables. How to map it to a user-defined instruction set is not clear yet, but it would be nice
to at least relieve the user from having to assign registers, jump labels and so on.

It might be possible to define interlieve assembly code like we do with Solidity/Yul and then provide a set of simplification
rules specific to an instruction set.

### Project structure

For an overview of the project structure, run:
```
cargo doc --workspace --no-deps --open
```
