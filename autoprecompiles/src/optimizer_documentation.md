# The Autoprecompiles Optimizer

## Terminology

### Field Elements

Throughout this document, we will be working in a finite field of prime order `p`. Often, we use operators or concepts that are only defined
in the integers. In this case, we use the natural number representation
of the field element, i.e. the unique integer `x` such that `0 <= x < p`
where the field operations are defined as `x + y = (x + y) mod p`
and `x * y = (x * y) mod p` for field elements `x` and `y`.

This way, we can also make statements about a field element being
_less than_ another field element, even if this would not make sense
inside the finite field. Sometimes, field elements are also interpreted
as signed integers instead of natural numbers, but this will be clarified.

### Constraint System

The optimizer is operating on an abstraction of a chip we call
_Constraint System_, which consists of a set of _Algebraic Constraints_
and _Bus Interactions_.

### Algebraic Constraint

An _Algebraic Constraint_ consists of an _Algebraic Expression_, i.e.
an expression involving the operators `+` and `*` on _Variables_ and
_Constants_ from the implied finite field. The idea is that the constraint
forces the expression to be zero and thus we write it as an equation
`<expr> = 0` (but also `<lhs> = <rhs>` if more convenient, by which
we mean `<rhs> - <lhs> = 0`).

An Algebraic Constraint is _satisfied_ by an assignment of the variables
if it evaluates to zero under this assignment.

Example: `x * (x - 1) = 0` is an algebraic constraint that forces
the variable `x` to be either zero or one, meaning that an
assignment only satisfies this constraint if it has `x = 0` or `x = 1`.

### Range Constraint

The task of the optimizer is hugely simplified by the concept of
_Range Constraints_. In an abstract way, a _Range Constraint_ is
the set of possible values for a specific algebraic expression.
The solver can derive new Range Constraints from Algebraic Constraints
but they are also used to allow a uniform abstraction of Bus Interactions.

In the example above, `x * (x - 1) = 0`, we can derive a Range Constraint
of `{0, 1}` for the variable `x`.

In our concrete implementation, we do not represent the exact set
of allowed values. Instead, we use an under-approximation, i.e.
a superset of the set of allowed values. This means we can only use
the information if the Range Constraints says that a value is not
allowed. This also makes sense since we are looking at Algebraic
Constraints in isolation and there can always be another Algebraic
Constraint that further restricts the set of allowed values.

The approximation we use for Range Constraints is a combination
of a _Wrapping Interval_ and a _Bitmask_.

A _Wrapping Interval_ is a pair of field elements `min` and `max`.
If `min <= max` (as seen in the natural numbers), the Wrapping
Interval allows a value `x` if and only if `min <= x <= max`.
This is the non-wrapping case.
If `min > max`, the Wrapping Interval allows a value `x`
if and only if `x >= min` or `x <= max`. This is the wrapping case.

The reason we allow these wrapping intervals is that we can compute
the Range Constraint interval of an expression `x + k` for any constant `k`
from the Range Constraint interval of `x` without losing information.

A _Bitmask_ is a natural number `bitmask` that is interpreted as a
bitmask for the natural number representation of field elements.
It allows a value `x` if and only if `x & bitmask == x`, i.e. all
bits that are set in `x` are also set in `bitmask`.
Note that the bitmask can never disallow the value zero.

A _Range Constraint_ allows a value if and only if both the bitmask
and the wrapping interval allow it.

We use `RC(x)` to denote the Range Constraint of an algebraic
expression `x`.

TODO It is not clear if this is the currently best-known range constraint,
the theoretical optimum or the one we can just directly compute from `x`.

### Bus Interaction

The concept of _Bus Interaction_ is a bit more complicated. The concrete
semantics of a bus interaction depends on the environment, i.e. the
zkVM we are operating inside and the chips it has.

A _Bus Interaction_ consists of a _Bus ID_, the _Multiplicity_ and
a _Payload_. The _Bus ID_ is an Algebraic Expression and specifies
which bus to interact with. The _Multiplicity_ is an Algebraic Expression
and in most cases it should evaluate either to 1 or -1. The _Payload_ is
the data that is sent to the bus or received from the bus and is a list
of Algebraic Expressions.

A Bus is _balanced_ if across the whole system and for all payloads,
the sum of the multiplicities is zero. Intuitively, with a multiplicity
of 1 we can send some payload and we receive it on the other end
with a multiplicity of -1.

The autoprecompiles optimizer will mostly work with an abstraction
of bus interactions that are specifically implemented for each concrete
bus type, but these implementations also usually fall into categories,
so it should not be difficult to implement this abstraction for a new bus
or system.

### Bus Interaction Abstraction

For the optimizer to be able to handle Bus Interactions, we need to
implement the following methods:

- `is_stateful`: For a given Bus ID (a field element), returns if the bus
  with the given ID is stateful or not. If a bus interaction is not stateful,
  if it only affects the payload passed to it and no other elements of the system.
  A memory bus or the execution bridge are examples of a stateful busses,
  while range constraint busses, busses modeling arithmetic operations or
  lookup tables are not stateful.

- `handle_bus_interaction`: Takes a Bus Interaction where its items are represented
  by Range Constraints instead of expressions or field elements and returns
  an equivalent Bus Interaction where the Range Constraints are possibly
  tightened. A correct implementation would be one that always returns
  its input (or also just fully unconstrained Range Constraints), but the tighter
  the Range Constraints are, the more powerful the optimizer will be.

As an example, let us assume we are modeling a bus that implements a byte range
constraint, i.e. a bus that takes a single payload item and enforces that it is
in the range `0..=255`. The bus is not stateful since it does not depend on any
parts of the system. A simple (and also best possible) implementation of
`handle_bus_interaction` would be to always return the `0xff`-mask Range Constraint
for the payload and ignore the input. Even if the input Range Constraint is something
like `200..=300`, the solver will combine the two Range Constraints and derive
`200..=255` as the new Range Constraint for the payload.

Another example would be an XOR-bus that takes three payload items `a, b, c`
and ensures that all of them are bytes and `a ^ b = c`. This bus is also not stateful.
Here, one would implement `handle_bus_interaction` by returning the three byte constraints
for the payload items if the input has no restrictiens. But if the value of the same bit
in two inputs is known, we can compute the value of the same bit in the third input.
Since our Range Constraints do not model individual bits well enough, it is sufficient
to only compute the third item if two of them are fully known.

We will see later how we can fully optimize away XOR bus interactions using just this
abstraction.

### Memory Bus

TODO Continue with the abstraction using Range Constraints.

## Combining Range Constraints


## Optimization Steps

The called functions are

```
optimize_exec_bus
loop:
    solver_based_optimization
    remove_trivial_constraints
    remove_free_variables
    remove_disconnected_columns
    trivial_simplifications
    optimize_memory
    LowDegreeBusInteractionOptimizer
inliner::replace_constrained_witness_columns
optimize_range_constraints
trivial_simplifications
```

in addition, in the solver we have to explain:
- linearizing
- boolean extraction
- solving algebraic constraints
 - simple equivalence
 - splitting into multiple constraints
 - solving itself
- handling bus interactions
- quadratic equivalence detection
- exhaustive search
- equal zero check

### Constraint System Solver

The Constraint System Solver is the core of the optimizer. It is created from a Constraint System, but
it does not directly modify the Constraint System. Instead it acts as an information base about the
variables in the Constraint System. It can provide tight Range Constraints for variables or expressions,
which include the special case of variables being constant. The optimizer uses the Constraint System Solver
to substitute such constant variables. It can also determine if two Algebraic Expressions are always different,
which is crucial for memory optimization to solve the aliasing problem.

#### Linearizing

#### Boolean Extraction

#### Simple Variable Equivalence

`try_to_simple_equivalence`

#### Splitting Algebraic Constraints Into Multiple Constraints

#### Solving Algebraic Constraints

##### Affine Constraints

##### Quadratic Constraints

#### Handling Bus Interactions

#### Quadratic Equivalence Detection

#### Exhaustive Search

#### Equal Zero Check

