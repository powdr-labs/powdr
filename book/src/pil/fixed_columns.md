# Fixed columns

powdr PIL requires the definition of fixed columns at the time of declaration.

For example:

```
{{#include ../../../test_data/pil/fixed_columns.pil:declare_and_define}}
```

A number of mechanisms are supported to declare fixed columns. Let `N` be the total length of the column we're defining.

## Values with repetitions

powdr PIL supports a basic language to define the value of constant columns using:
- arrays, for example `[1, 2, 3]`
- repetition, for example `[1, 2]*`
- concatenation, for example `[1, 2] + [3, 4]`

These mechanisms can be combined, as long as a single repetition is used per column definition.

```
{{#include ../../../test_data/pil/fixed_columns.pil:repetitions}}
```

## Mappings

A column can be seen as a mapping from integers to field elements. In this context, different functions are supported:

```
{{#include ../../../test_data/pil/fixed_columns.pil:mapping}}
```

> Note that conversion from integer to field element is currently implicit, as seen in the first example above.