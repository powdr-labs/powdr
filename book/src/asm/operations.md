# Operations

Operations enable a constrained machine to expose behavior to the outside.

```
{{#include ../../../test_data/asm/book/operations_and_links.asm:operations}}
```

They are defined by:
- a value for the operation id. When calling this operation, the operation id of this machine is set to this value.
- parameters in the form of columns defined in the current machine

The actual behavior of the operation is defined freely as constraints.