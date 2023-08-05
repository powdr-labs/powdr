# Links

Links enable a constrained machine to call into another machine.

```
{{#include ../../../test_data/asm/book/operations_and_links.asm:links}}
```

They are defined by:
- a boolean flag which must be on for the link to be active
- parameters to pass to the other machine, in the form of columns defined in the current machine
- an operation or function in the machine we're calling