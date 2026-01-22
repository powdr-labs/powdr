# The Autoprecompiles Optimizer

## Terminology

### Field Elements

Throughout this document, we will be working in a finite field of prime order `p`.
Often, we use operators or concepts that are only defined
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
and _Bus Interactions_. Both of them contain expressions involving variables.
A Constraint System is _satisfied_ by an assignment
of its variables if the assignment satisfies all Algebraic Constraints
and Bus Interactions in the system.

The purpose of the optimizer is to simplify a Constraint System
into a Constraint System that has the same satisfying assignments.
This is not the exact definition of correctness for the optimizer because
it is also allowed to remove variables and introduce new ones, but
it is a good guideline for now until we have all the definitions.

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
assignment satisfies this constraint if and only if it has `x = 0` or `x = 1`.

### Range Constraint

The task of the optimizer is hugely simplified by the concept of
_Range Constraints_. Range Constraints allow us to combine the effects of
different Algebraic Constraints (and Bus Interactions) on the same variable.
In an abstract way, a _Range Constraint_ is just
a restriction on values and we can say that a value _satisfies_ a Range Constraint
or not. We also say that a Range Constraint _allows_ a value if that value satisfies it.
We can connect Range Constraints and variables (a Range Constraint _on_ a
variable) and say that an assignment of a variable `v` _satisfies_ a Range Constraint
`r` on `v` if the value assigned to `v` satisfies `r`.
A Range Constraint `r` on a variable `v` is _valid_ in a Constraint System if any
satisfying assignment of the Constraint System also satisfies `r`.

During optimization, we derive Range Constraints for expressions and variables
from Algebraic Constraints and Bus Interactions and use them to simplify
the Constraint System. We also use Range Constraints for a uniform abstraction
of Bus Interactions as we will see in a later section.

As an example, let us consider the Constraint System consisting of the
Algebraic Constraint `x * (x - 1) = 0`. From this Algebraic Constraint the optimizer
will synthesize a Range Constraint `r1` on `x` that only allows the values `0` and `1`.
The Range Constraint is valid in the Constraint System because, as we saw at the end of
the previous section, any satisfying assignment for the Algebraic Constraint
must have `x = 0` or `x = 1`. Note that a Range Constraint that allows all values
in the field is always valid, but not very useful.

Now assume we extend the Constraint System by an additional constraint
`(x - 2) * (x - 1) = 0`. The Range Constraint `r1` on `x` is still valid in the extended
system because additional constraints can only reduce the set of satisfying assignments.
If we look at the second constraint in isolation, we can get a Range Constraint `r2` on `x`
that allows exactly the values `1` and `2`. Both `r1` and `r2` are valid in the extended
system, and so is their intersection, which only allows the value `1`.

From this simple example, one can already see the power of these Range Constraints.
In a later section we will talk about the various computations that can be performed
on Range Constraints including the intersection.

#### Concrete Implementation of Range Constraints

The abstract concept of Range Constraints is implemented in the optimizer by a combination
of a _Wrapping Interval_ and a _Bitmask_.

A _Wrapping Interval_ is a pair of field elements `min` and `max`.
A value `x` is allowed by the Wrapping Interval if and only if it is
part of the sequence `min`, `min + 1`, `min + 2`, ..., `max`.
Note that this sequence wraps around the prime `p` of the field.

The following is an equivalent definition:
If `min <= max` (as seen in the natural numbers), the Wrapping
Interval allows a value `x` if and only if `min <= x <= max`
(the non-wrapping case).
If `min > max`, the Wrapping Interval allows a value `x`
if and only if `x >= min` or `x <= max` (the wrapping case).

The reason we allow these wrapping intervals is that we can compute
the Range Constraint interval of an expression `x + k` for any constant `k`
from the Range Constraint interval of `x` without losing information.

A _Bitmask_ is a natural number `bitmask` that is interpreted as a
bitmask for the natural number representation of field elements.
It allows a value `x` if and only if `x & bitmask == x`, i.e. all
bits that are set in `x` are also set in `bitmask`.
Note that in particular, the bitmask can never disallow the value zero.

A _Range Constraint_ allows a value if and only if both the bitmask
and the wrapping interval allow it.

### Bus Interaction

The concept of _Bus Interaction_ is a bit more complicated. The concrete
semantics of a bus interaction depends on the environment, i.e. the
zkVM we are operating inside and the chips it has.

A _Bus Interaction_ consists of a _Bus ID_, a _Multiplicity_ and
a _Payload_. The _Bus ID_ is an Algebraic Expression and specifies
which bus to interact with. The _Multiplicity_ is an Algebraic Expression
and in most cases it should evaluate either to 1 or -1. The _Payload_ is
the data that is sent to the bus or received from the bus and is a list
of Algebraic Expressions.

Usually, one can think of a Bus Interaction to constrain the items in
the payload as a tuple. For example, if you have an XOR bus, then
a Bus Interaction with payload `(a, b, c)` ensures that
`c = a ^ b`. In a bus interaction, there is no intrinsic concept of
inputs and outputs (even though some buses can be seen like that).

In the example of the XOR bus, it is perfectly fine to use
`(a, b, 0xff)` and thus ensure that (on the lower most byte),
`b` is the bitwise negation of `a`.

Buses can only be properly described across a system of chips or
constraint systems. What we want to achieve is that all the buses
are balanced:

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
  it only affects the payload passed to it and no other elements of the system.
  A memory bus or the execution bridge are examples of stateful busses,
  while range constraint busses, busses modeling arithmetic operations or
  lookup tables are not stateful.

- `handle_bus_interaction`: Takes a Bus Interaction where its items are represented
  by Range Constraints instead of expressions. It returns
  a Bus Interaction with Range Constraints such that all payloads that satisfy
  the input Range Constraints and the bus semantics also satisfy the output
  Range Constraints. An implementation that always returns its inputs
  (or also just fully unconstrained Range Constraints) would be correct, but
  of course you should return Range Constraints that are as tight
  as possible such that the optimizer gets the most out of it.

As an example, let us assume we are modeling a bus that implements a byte
constraint, i.e. a bus that takes a single payload item and enforces that it is
in the range `0..=255`. The bus is not stateful since it does not depend on nor affects any
other parts of the system. A simple correct implementation of
`handle_bus_interaction` would be to always return a `0xff`-mask Range Constraint
for the payload and ignore the input. It is correct because any assignment that
satisfies the bus semantics must have the payload in the range `0..=255`.
Even though this implementation ignores the input Range Constraints, is also the best
possible, since even if the input Range Constraint
is something like `200..=300`, the optimizer will not forget it but
instead combine it with the one returned by `handle_bus_interaction` and derive
`200..=255` as the new Range Constraint for the payload.

Another example is an XOR-bus that takes three payload items `a, b, c`
and ensures that all of them are bytes and `a ^ b = c`. This bus is also not stateful.
Here, one would implement `handle_bus_interaction` by returning the three byte constraints
for the payload items if the input has no restrictions. If two inputs are fully
determined (i.e. only a single value satisfies the Range Constraints),
we can compute the third and return that as a Range Constraint.

We will see later how we can fully optimize away XOR bus interactions using just this
abstraction.

### Memory Bus

TODO Continue with the abstraction using Range Constraints.

## Combining Range Constraints


## Grouped Expressions

The main data structure used for algebraic expressions is the _Grouped Expression_.
A Grouped Expression consists of a constant term, a list of linear terms (a list of pairs of a non-zero coefficient
and a variable) and a list of quadratic terms (a list of pairs of Grouped Expressions).

The variables in the linear terms are unique and the coefficients are required
to be non-zero. The uniqueness is enforced by using a map data type.
This makes it easy to compare, add and subtract affine expressions, which do not
have quadratic terms.

It also provides a normal form for affine Algebraic Constraints if we require 
the coefficient of the first variable (according to some fixed order on the 
variables) to be one. Note that an Algebraic Constraint can be multiplied 
by a nonzero factor without changing the semantics.

Addition and subtraction of Grouped Expressions are implemented to remove linear terms that cancel each other out,
and they perform some checks also in the quadratic terms, but this part is not complete for performance reasons.

## Equivalence Notion

### Introduction and Example

We start with some informal intuition and an example.

We call two Constraint Systems _equivalent_ if every satisfying assignment for one system
can be extended to a satisfying assignment for the other system and every such extension
leads to the same payloads and multiplicities for all _stateful_ bus interactions in both systems.

As an example, consider the two systems

System A:
```
x = 8
x + y + z = 12
BusInteraction { bus_id = 2, multiplicity = 1, payload = [x, y, z] }
w * (w - 1) = 0
```

System B:
```
y + z = 4
BusInteraction { bus_id = 2, multiplicity = 1, payload = [8, y, z] }
```

Let us assume that the bus with ID 2 is stateful and allows all combinations of values between 0 and 100 (inclusive).
Note that the variables `y` and `z` are not uniquely determined in either system, so the stateful bus
acts both as input and output for the system. TODO can that be or do we need one send and one receive?

Note that System B is obtained from System A by substituting `x = 8` and removing `w`.

All satisfying assignments of System A must have `x = 8` and either `w = 0` or `w = 1`.
Such an assignment also satisfies System B and it produces the same values for the stateful bus interaction.

The converse is a bit more complicated: Satisfying assignments of system B only assign the variables
`y` and `z`. So we need to extend the assignment for System A into a satisfying one. For `x`, the only
choice we have is `x = 8`, but there are two ways to extend the assignment with regards to `w` such that
it is still satisfying, `w = 0` or `w = 1`. Since both ways to extend the assignment
produce the same values in the stateful bus interaction, the systems are equivalent.

### Abstract Equivalence Definition

Now let's proceed formally.

Let $S = (C, B)$ be a system, defined over a vector of variables, $w$. Let
$C$ be the constraints of the system: a formula over $w$. It includes the
algebraic constraints, stateless buses, and any constraints enforced by stateful
buses on their interactions. Let $B = ( (d_i, m_i))_{i=1}^{|B|}$ be the stateful
bus interactions. It is a fixed-length sequence, indexed by $i$. Each
interaction is a pair. The first component, $d_i$, is the data, a fixed-length
list of field terms, so its type is $\mathbb{F}^+$ (sequences of positive
length). Assume the bus ID is represented as the first entry in $d_i$, for
simplicity. The second component of an interaction is $m_i$, the multiplicity,
which is a field term.

The bus interactions will be aggregated into a special kind of multiset. We
refer to a map from $\mathbb{F}^+ \to \mathbb{F}$ as a “field multiset” (aka
“multiset”). This name reflects an interpretation of the map as a multiset in
which each key in the map appears with multiplicity equal to its value. Note
that these multisets can be added pointwise. That is, for multisets $m$ and
$m'$, their sum $m + m'$ maps each key $k$ to $m(k) + m'(k)$. We interpret a bus
interactions as a multiset with one key and the specified multiplicity. That is,
we define $\textsf{toMs}(d, m)$ to be the field multiset that sends $d$ to $m$
and all other keys to $0$. Then, we define $\Sigma(B)$ to be $\sum_i
\textsf{toMs}(d_i, m_i)$

Now we can define equivalence, which has two conditions. Assume two systems
$S = (C, B)$ and $S' = (C', B')$ in variable $w$ and $w'$, respectively. The $S$
is the input to powdr and $S'$ is the output. powdr also outputs a function $E$
that maps $w$ to $w'$. Most of the variables in $w'$ have the same name as some
variable in $w$---they takes its value. Other variables have an entry in the
"derived variables", which explains how to compute them from $w$.

The first condition is **completeness**, which says that when $S$ is satisfiable,
$E$ gives a satisfying assignment for $S'$ with the same effects (stateful bus
interactions). Formally:

$$\forall w, \forall s, C(w) \wedge \Sigma(B(w)) = s
\implies C'(E(w)) \wedge \Sigma(B'(E(w))) = s$$

The second condition is **soundness**, which says that when $S'$ is satisfiable, $S$
is too, and with the same effects. Formally, there should exists an efficient
$I(w') \to w$ such that:

$$\forall w', \forall s, C'(w') \wedge \Sigma(B'(w')) = s
\implies C(I(w')) \wedge \Sigma(B(I(w'))) = s$$

### Worked example

We will give equivalent two systems, as examples.

The first system, $S = (B, C)$ is a slightly more complex version of the
informal example above, with $b$ in place of $w$.

> $d_0 = (2, x, y, z), m_0 = 1$
>
> $d_1 = (2, x, y, z), m_1 = b$
>
> $d_2 = (2, 8, y, z), m_2 = -b$
>
> $C = (x = 8 \wedge x + y + z = 12 \wedge b(b-1) = 0)$

The second system $S' = (B', C')$ is:

> $d'_0 = (2, 8, y', z'), m'_0 = 1$
>
> $C' = (y' + z' = 4)$

Algorithmically, one optimizes $S$ into $S'$ by the following transformations:

1. Since $x = 8$, substitute $8$ for $x$.
2. Now, we have $d_1 = d_2$, and $m_1 = -m_2$, so remove both bus
   interactions--they have equal data and their multiplicities sum to 0.
3. $b$ appears in no bus interactions, and in no algebraic constraints with
   other variables. Moreover, the constraints it does appear in are satisfiable.
   Remove them.

Now, we prove that these systems are equivalent under the prior definition. That
is, we prove soundness and completeness.

#### Soundness

$I$ is defined as follows: $x \gets 8, y \gets y', z \gets z', b \gets 0$.

Now, we must show that

$$\forall w', \forall s, C'(w') \wedge \Sigma(B'(w')) = s
\implies C(I(w')) \wedge \Sigma(B(I(w'))) = s$$

Our proof will actually match this equivalent statement instead:

$$\forall w', \forall w, \forall s, C'(w') \wedge \Sigma(B'(w')) = s \wedge w = I(w')
\implies C(w) \wedge \Sigma(B(w)) = s$$

Proof:

* Fix $w' = (y', z')$.
* Fix $w = (x, y, z, b)$.
* Fix $s$.
* To show the $\implies$, assume
  * $w = I(w')$; that is:
    * $x = 8$
    * $y = y'$
    * $z = z'$
    * $b = 0$
  * $y' + z' = 4$
  * $s = \mathsf{toMs}((2, 8, y', z'), 1)$
* And now we need to show each of the following goals:
  * $x = 8$, since it is part of $C(w)$
    * we already have this
  * $x + y + z = 12$, since it is also part of $C(w)$
    * we have this since we have $x=8, y=y', z=z', y'+z'=4$
  * $b(b-1) = 0$, since it is also part of $C(w)$
    * we have this since $b=0$
  * $s = \mathsf{toMs}((2, x, y, z), 1) + \mathsf{toMs}((2, x, y, z), b) + \mathsf{toMs}((2, 8, y, z), -b)$
    * we have
        $s = \mathsf{toMs}((2, 8, y', z'), 1)$
    * which is
        $s = \mathsf{toMs}((2, 8, y, z), 1)$
    * since $x=0$, we have
        $s = \mathsf{toMs}((2, x, y, z), 1)$
    * since 0 multiplicities are an identity for $+$, we have
        $s = \mathsf{toMs}((2, x, y, z), 1) + \mathsf{toMs}((2, x, y, z), 0) + \mathsf{toMs}((2, 8, y, z), 0)$
    * since $b=0$, we have or goal:
        $s = \mathsf{toMs}((2, x, y, z), 1) + \mathsf{toMs}((2, x, y, z), b) + \mathsf{toMs}((2, 8, y, z), -b)$

#### Completeness

$E$ is defined as $y' \gets y, z' \gets z$.

We must show that

$$\forall w, \forall s, C(w) \wedge \Sigma(B(w)) = s
\implies C'(E(w)) \wedge \Sigma(B'(E(w))) = s$$

Again, we will instead show the equivalent

$$\forall w, \forall w', \forall s, C(w) \wedge \Sigma(B(w)) = s \wedge w' = E(w)
\implies C'(w') \wedge \Sigma(B'(w')) = s$$

* Fix $w = (x, y, z, b)$.
* Fix $w' = (y', z')$.
* Fix $s$.
* To show the $\implies$, assume
  * $w'=E(w)$, that is:
    * $y' = y$
    * $z' = z$
  * $x = 8$
  * $x + y + z = 12$
  * $s = \mathsf{toMs}((2, x, y, z), 1) + \mathsf{toMs}((2, x, y, z), b) + \mathsf{toMs}((2, 8, y, z), -b)$
* And now we need to show each of the following goals:
  * $x' + y' = 4$
    * we have this from $x' = x, y' = y, x = 8, x + y + z = 12$
  * $s = \mathsf{toMs}((2, 8, y', z'), 1)$
    * we have:
      $s = \mathsf{toMs}((2, x, y, z), 1) + \mathsf{toMs}((2, x, y, z), b) + \mathsf{toMs}((2, 8, y, z), -b)$
    * since $x = 8$, we have:
      $s = \mathsf{toMs}((2, 8, y, z), 1) + \mathsf{toMs}((2, 8, y, z), b) + \mathsf{toMs}((2, 8, y, z), -b)$
    * by additive inverse for multiset multiplicities we have:
      $s = \mathsf{toMs}((2, 8, y, z), 1)$
    * by $y'=y,x'=x$, we have our goal:
      $s = \mathsf{toMs}((2, 8, y', z'), 1)$

### Connection to prior definitions from the literature

Our definition is an instantiation of Ozdemir et al.'s definition of ZKP
compiler correctness from the paper ["Bounded Verification for
Finite-Field-Blasting in a Compiler for Zero Knowledge Proofs"][1]. Start from
their Definition 1. To see this, set:

* their $w$ and $w'$ to our $w$ and $w'$,
* their $x$ and $x'$ to our $s$ (both are $s$),
* their $\phi(x,w)$ to our $C(w) \wedge \Sigma(B(w)) = s$,
* their $\phi'(x',w')$ to our $C'(w') \wedge \Sigma(B'(w')) = s$,
* their $\mathsf{Ext}_x(x)$ to the identity function from $s$ to itself,
* their $\mathsf{Ext}_w(x, w)$ to our $E$, and
* their $\mathsf{Inv}(x', i')$ to our $I$.

This alignment bodes very well for our definition. Ozdemir et al. proved that a
ZKP compiler that is correct by their definition can securely compose with a
zkSNARK for the compiler's output language to give a zkSNARK for the compiler's
input language. We would hope to show a similar result using our definition. But
our result, would also need to account for the zkVM's design. Our result would
say something like (secure zkSNARK for plonkish constraints) + (correct zkVM) +
(correct powdr) = (secure zkSNARK for RISC-V).

### Connections to Georg's definition

Our definition strengthens Georg's slightly. In his soundness definition,
$I$ and $E$ are de-skolemized (their outputs are existentially quantified). This
is equivalent to removing the requirement that $I$ and $E$ be efficient. An
inefficient $E$ really wouldn't work, because then you can't compute the witness
$w'$. Fortunately, powdr outputs $E$ (encoded in the variable derivations). An
inefficient $I$ means that powdr would compose with a zkSNARG, but not a
zkSNARK. That is, it no longer applies to knowledge soundness, just to
existential soundness.

### Constraints

In the foregoing, we noted that stateless bus interactions and algebraic
constraints are represented by $C$. Now, we discuss $C$ in more detail.

The algebraic constraints are just QF_FF predicates over the variables $w$. More
specifically, they are $\mathbb{F}$ equalities over terms constructed with $+$
and $\times$ in $\mathbb{F}$.

The are a few different bus interactions, that contribute to $C$:

* TODO

### Requirements that are not yet formalized.

The definition above is a living object. There are requirements for powdr that
we have not yet formalized, and there may be some that we are not yet aware of.
Most of these are likely weird invariants that OpenVM implicitly assumes in its
own definition of correctness.

Currently, we know of one unformalized requirement:

* Under all satisfying assignments, a constraints system must ensure that the
  different between the execution step counter in its final execution bus send
  and its initial execution bus receive is at most the total number of bus
  interactions. This requirement is used to prevent overflows related to the
  step counter and the bus multiplicities. Powdr is currently violating this
  requirement[2]. But also, this requirement is not tight. Many looser
  requirements could also prevent overflow. And, powdr might be able to be
  changed to respect it.

  We expect that it will be easy to very a requirement like this one once we
  figure out exactly what we need to verify.

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

[1]: https://eprint.iacr.org/2023/778.pdf
[2]: https://github.com/powdr-labs/powdr/issues/3542
