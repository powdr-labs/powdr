# Macros

powdr-pil exposes a macro system which can generate arbitrary powdr-pil code.

## Definition

Let's define some macros which generate powdr-pil expressions:

```
{{#include ../../../test_data/pil/fib_macro.pil:expression_macro_definitions}}
```

In particular, we can generate constraints inside macros:

```
{{#include ../../../test_data/pil/fib_macro.pil:constraint_macro_definitions}}
```

## Usage

> Macros currently have global scope

Usage of the defined macros happens as expected in powdr-pil code:

```
{{#include ../../../test_data/pil/fib_macro.pil:expression_macro_usage}}
```

Generating constraints:

```
{{#include ../../../test_data/pil/fib_macro.pil:constraint_macro_usage}}
```

