# Links

Links enable a constrained machine to call into another machine.
They are defined by a call to an operation, where inputs and outputs are expressions.
An optional boolean flag restricts the rows in which the link is active.
Links without a boolean flag are active in every row.
```
{{#include ../../../test_data/asm/book/operations_and_links.asm:links}}
```
If a boolean flag is given, the link is only active in rows where the flag evaluates to `1`.
Whenever a link is active, the columns mapped as inputs and outputs are constrained by the operation implementation.
The following example demonstrates how to use links with flags.
```
col fixed odd_row = [0,1]*;

link if odd_row => z = submachine.foo(x, y); // active on odd rows only
link if (1 - odd_row) => z = submachine.bar(x, y); // active on even rows only
```
