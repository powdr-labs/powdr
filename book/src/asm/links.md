# Links

Links enable a constrained machine to call into another machine.
They are defined by a boolean flag and a call to the operation, where inputs and outputs are expressions.
```
{{#include ../../../test_data/asm/book/operations_and_links.asm:links}}
```
A link is only active in rows where the boolean flag is `1` (all lines in the above example).
Whenever it is active, the columns mapped as inputs and outputs are constrained by the operation implementation.

