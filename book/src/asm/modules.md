# Modules

powdr exposes a module system to help organise and reuse code.

```rust
{{#include ../../../test_data/asm/book/modules.asm}}
```

Note that a module can't be called `std`, as this name is reserved for the powdr standard library.

Similar to Rust, any reference that cannot be resolved is looked up once more in `std::prelude`.
This module exposes basic types and values such as `Option`, `true` and `false`.
This means that you can use `Option` anywhere without prefix.