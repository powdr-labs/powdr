# Powdr Assembly (Powdr-Asm)

Powdr-Asm is used to define instruction sets and write programs that depend on
control flow, with the aim of generating ZKPs about them.
Powdr-Asm follows the execution trace table model shared by many proof systems.
In fact, Powdr-Asm extends Polygon Hermez' [PIL and
zkASM](https://zkevm.polygon.technology/docs/category/polynomial-identity-language).
In such a model, the execution is represented by a two-dimensional fixed-size
table, where columns represent the state (for example, registers or
memory) and each row represents a step in time for a given execution.
Constraints specify how the state evolves from any step to the next.

As an example, let's apply that model to a program that computes
the factorial of a number. Our state consists of two registers:

- CNT - a counter
- ACC - the answer accumulator

The first row represents the initial state of our program, that is, `PC =
0`, `CNT = <input>`, `ACC = 1`.
From then on, we compute the subsequent rows with the following pseudocode:

START:
  Is CNT = 0?
  If `yes` go to `END`
  ACC := ACC * CNT
  CNT := CNT - 1
  Go to START
END:
  ACC equals `factorial(input)`

We can compute the following execution trace with 5 as input:

| CNT | ACC |
|-----|-----|
| 5   | 1   |
| 4   | 5   |
| 3   | 20  |
| 2   | 60  |
| 1   | 120 |
| 0   | **120** |

Note that the height of the table limits the length of the trace.

## ISA

There are three constructs that can be used to define an instruction set:

- Registers
- Macros
- Instructions

### Registers

Registers are basic state components that can store a single number.
There are three types of registers:

- Program Counter (PC). Used for control flow.
- Assignment registers. Used by parametric instructions and for assignments.
- General registers. Used by program logic.

The registers are declared in the following way:

```asm
reg pc[@pc];
reg X[<=];
reg A;
```

In the snippet above, the `@pc` annotation means that `pc` is implicitly
incremented by one for instructions that do not explicitly assign it.  The `<=` annotation
means `X` is an assignment register and can be used by parametric instructions
and for assignments.
Finally, `A` is a general purpose register and can be used by program logic.

### Instructions

Instructions are predicates that define constraints that are enforced only when the
instruction is invoked. The constraints may refer to registers on the execution
trace line that it is invoked and/or the next line.
If the next line version of a register is used, for example `X'`, it must be in
the LHS of the expression, where the RHS contains no next registers.
If an instruction has formal parameters, they must be either literals or
assignment registers.

```asm
instr ADD X, Y -> Z { X + Y = Z }
```

The above instruction can be read as "If `Z` is the same as `X + Y`, then `ADD
X Y` returns `Z`".
As an analogy, this is similar to Prolog predicates.


### Macros

Macros can be used to build expressions which are inlined at the call site.

```asm
macro if_then_else(condition, true_value, false_value) {
    condition * true_value + (1 - condition) * false_value
};
```

The macro above can be used to simplify if-then-else arithmetic expressions.

### Factorial, again

We now have enough constructs to create a simple ISA and implement our
Factorial program.

```asm
pil {
    col witness XInv;
    col witness XIsZero;
    XIsZero * (1 - XIsZero) = 0;
    XIsZero = 1 - X * XInv;
    XIsZero * X = 0;
}

macro if_then_else(condition, true_value, false_value) {
    condition * true_value + (1 - condition) * false_value
};

macro jump_to(target) { pc' = target; };

macro jump_to_if(condition, target) {
    jump_to(if_then_else(condition, target, pc + 1));
};

reg pc[@pc];
reg X[<=];
reg CNT;
reg ACC;

instr jmpz X, l: label { jump_to_if(XIsZero, l) }
instr jmp l: label { jump_to(l) }
instr dec_CNT { CNT' = CNT - 1 }
instr assert_zero X { XIsZero = 1 }

CNT <=X= ${ ("input", 0) };

start::
 jmpz CNT, end;
 ACC <=X= ACC + ${ ("input", CNT) };
 dec_CNT;
 jmp start;

end::
```

The beginning of the snippet above is a necessary gadget that computes whether
`X` is 0 at a certain point.

The macros, registers and instructions define our ISA, and can be used to write
any programs that only need these features.

Finally we have the program itself, which can be either manually written or
automatically compiled from some higher level language.
