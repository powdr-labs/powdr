## The Autoprecompiles Optimizer

### Terminology

#### Field Elements

Throughout this document, we will be working in a finite field of prime order `p`. Often, we use operators or concept that are only defined
in the integers. In this case, we use the natural number representation
of the field element, i.e. the unique integer `x` such that `0 <= x < p`
where the field operations are defined as `x + y = (x + y) mod p`
and `x * y = (x * y) mod p` for field elements `x` and `y`.

This way, we can also make statements about a field element being
_less than_ another field element, even if this would not make sense
inside the finite field. Sometimes, field elements are also interpreted
as signed integers instead of natural numbers, but this will be clarified.

#### Constraint System

The optimizer is operating on an abstraction of a chip we call
_Constraint System_, which consists of a set of _Algebraic Constraints_
and _Bus Interactions_.

#### Algebraic Constraint

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

#### Range Constraint

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

#### Bus Interaction

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

TODO Continue with the abstraction using Range Constraints.