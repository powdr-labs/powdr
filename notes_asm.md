# Some (maybe inconsistent for now) Notes about Assembly

## Simplified Example

field: 2**64 - 2**23 + 1

// The "@line" means that the register pc is matched with "line" in the lookup.
// The "pc' = pc + 1" is the default assignment if it is not changed by an instruction.
reg pc(@line): pc' = pc + 1
// If no default assignment is given, it is "A' = A"
reg A[8] // default: : A' = A
reg Z[8] //: Z' = Z
reg B[8] //: B' = B
reg rr; // return address (only non-nested calls)

// Restriction: Only single primed register on the LHS, no primed registers on the RHS.
instr jmp l: label { pc' = l }
instr jmpi c: bool, l: label { pc' = c * l + (1 - c) * pc }
instr call l: label { rr' = pc + 1, pc' = l }
instr ret { pc' = rr }

fun eq(a, b) -> c { c <=Z= binary(0, a, b) }
fun add(a, b) -> c { c <=Z= binary(1, a, b) }
fun sub(a, b) -> c { c <=Z= binary(2, a, b) }
fun mul(a, b) -> c { c <=Z= binary(3, a, b) }
fun binary(op, a, c) -> c {
  Binary(op, <=X= a, <=Y= b, c <=Z=)
}


  A <= mload(B)
  A <= add(A, B)
  A <= mul(A, B)
repeat:
  Z <= eq(B, 0)
  jmpi Z out
  A <= mul(A, 2)
  B <= sub(B, 1)
  jmp repeat
out:

------------------------------------------

Assembly language:
- instructions
- labels
- register assignments (with function calls on the RHS)

### Labels

Labels are just representatives for their line numbers.

### instructions

Each instruction has a flag (constant poly) that is set to true in the line (it can be bits all in the same column).
This flag is used with the assignments in the definition of the instruction.

instr jmpi c: bool, l: label { pc' = c * l + (1 - c) * pc }

All instructions that assign to `pc'` get combined to a single constraint, together with these flags:

pc' = jmp * jmp_arg1 + jmpi * (c * jmpi_arg2 + (1 - jmpi_arg1) * pc) + regular * (pc + 1);

The "_arg" expressions are further costant polynomials that contain the arguments
per line. An optimizer might combine them if possible.

Arguments to instructions that are dynamic (i.e. depend on committed variables, i.e. registers)
like jmpi_arg1 (the condition) cannot be constant polynomials. They are intermediate polynomials
that are assigned similar to the way described below. These arguments should be avoided
due to their cost, since they need one additional flag for each register they are assigned from.

### Register Assignments

Assignments use `<=` syntax, for example `A <= B + 2`. This is actually a shorthand for
`A <=X= B + 2`, where `X` is the name of the single register that was declared using `reg X implicit assign;`
(TODO find better name).

These assignments work as follows: The LHS of `<=X=` can contain a register (maybe multiple, separated by
comma, but also no registers at all). The RHS can contain an affine expression (including function calls) in registers,
and can also be empty.

You can think of the expression on the RHS being evaluated and assigned to the registers on the
LHS. This is done in a single step, so no register can be both on the LHS and on the RHS (not even
hidden inside called functions).

For each register `A` that appears anywhere in the program on the LHS of a `<=X=`-assignment,
a constant flag polynomial `write_X_A` is created that is set to true in all the lines where `A` appears on the LHS.
Similar for the RHS, a constant polynomial `read_X_A` is created, that can hold larger numbers as
well, so that `A <=X= 2*B` is possible (the optimizer can degrade it to a flag if possible).

If we also have literal constants, another constant poly `const_X` is created that holds the `8`
for a line of the form `A <=X= 2*B`.

For each such assignment register, a constraint of the following form is created:

X = read_X_A * A + read_X_B * B + ... + const_X;

And for every register on the LHS of a X-assigment, we have:

A' = write_A * X

in addition to the update-constraints we already have for A. Of course, if A is on the LHS
of an assignment, any other update to A conflicts and is reported by the compiler.


Warn about: This is finite field arithmetic and does component-wise multiplication if the registers are arrays.

### Functions / State Machine Calls

Functions can be defined using the `fun` keyword:

fun eq(a, b) -> c { c <=Z= binary(0, a, b) }
fun add(a, b) -> c { c <=Z= binary(1, a, b) }
fun sub(a, b) -> c { c <=Z= binary(2, a, b) }
fun mul(a, b) -> c { c <=Z= binary(3, a, b) }
fun binary(op, a, c) -> c {
  Binary(op, <=X= a, <=Y= b, c <=Z=)
}

This looks a bit intimidating, but their use is actually much easier:

A <=Z= add(B, B + 3)

This specific example is complicated, because we want the opcodes
to be as flexible as possible and thus each of the parameters and return values
uses different assignment registers. The `a`, `b` and `c` are only formal
parameters and function calls could be seen as string-replacements. In fact,
all function calls have to be inlined in a way.

The line `Binary(op, <=X= a, <=Y= b, c <=Z=)` constitutes a plookup to a
state machine called `Binary`. The state machine needs explicitly defined
latching flag polynomial and public input/output polynomials.
The generated lookup looks as follows:

plookup ROM.is_binary {op, X, Y, Z} in Binary.latch {Binary.op, Binary.X, Binary.Y, Binary.Z}

You see that it just uses the assignment registers in the lookup. The assignments in the
chain of function calls is combined as follows:

A <=Z= add(B, B + 3)

is resolved to

A <=Z= <=Z= <=Z= 
<=X= B
<=Y= B + 3
op = 0
Binary(op, X, Y, Z)

The multiple assignments through Z are simplified:

A <=Z=
<=X= B
<=Y= B + 3
op = 0
Binary(op, X, Y, Z)

And this is of course all done in a single step.

It goes without saying that this way to do it is extremely expensive
because it uses three assignment registers. It might be better to use
fixed registers to read from, as follows:

fun add_A_B -> c { c <=X= binary_A_B(0) }
fun sub_A_B -> c { c <=X= binary_A_B(1) }
...
fun binary_A_B(op) -> c {
  Binary(op, A, B, c <=X=)
}

### Memory

Memory access is usually done as follows:

There are two kinds of instructions:
value = read addr
write addr value

The trace generates two tables that are permutations of each other that contain the following columns:
step, op, addr, value

step is the step (not the pc), op is {read, write}, addr is the address where a read/write occurs and value is the value read / written.

The trace generates the first table, sorted by step count, constraints ensure that it is in
line with actual execution and register values.

The second table is sorted by address (then by step).
It has the following constraints:
- check that it is properly sorted
- value can only change if address changes or op is "write"


The constraints, the sorting and the permutation check can be expressed in a pil file.

The main features we need to generate the first table are:
- access to a global step value (probably makes sense anyway)
- access to the actual memory values (for "read").

So we need access to hash maps that are available during execution stage.
We need to make sure that this value is only used for committed values.

An example of memory instructions would be as follows:

reg X implicit;

A <=X= B + 2
A <=X= mload(B) // A <=X= C <=X= mload(B) -> C is just formal parameter and can be removed.
mstore(A + 7, 9)

fun mload(A) -> C { <=addr= A, C <=X=, memop(false) }
fun mstore(A, V) { <=addr= A, <=X= V, memop(true) }

// performs memory op on address from register A
fun memop(isWrite: bool) {
  // Perform a lookup to a state machine called "Mem" (defined in a PIL file).
  // This compiles to a plookup identity.
  // The implied selector on the left hand side is the flag that is
  // active in all steps where "memop" is called.
  // The implied selector on the right hand side is the "latching"
  // selector defined in the state machine.
  Mem(addr: A, step: Global.STEP, isWrite: isWrite, value: X)
}





-----------------------------------------
generated:

pc' = jmp * jmp_arg1 + jmpi * (c * jmpi_arg2 + (1 - jmpi_arg1) * pc) + regular * (pc + 1);
binary {op, A, B, C} in {Binary.op, Binary.A, Binary.B, Binary.C};
binaryCounter' = binaryCounter + binary;



reg pc: pc' = pc + 1
reg A: A' = A

repeat:
  A = B + A
  jmp repeat

instr jmp l: label { pc' = l }

{pc, instru} in {ROM.line, ROM.instru}

namespace ROM:
  pol constant jmp_l;
  pol reg0 = inA * A + inB * B
  A' = reg0 * outA
  B' = reg0 * outB

  // compiled from instr jmp l: label { pc' = l }
  pc' = isJMP ? jmp_l : pc + 1;
