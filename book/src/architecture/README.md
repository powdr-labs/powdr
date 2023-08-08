# Architecture

powdr applies a number of steps in order to reduce a powdr asm program into a PIL file.

We provide a high level overview of these steps.

```ascii
            ┌────────────┐           ┌──────────┐
            │            │           │          │
 powdr asm  │            │ AIR graph │          │  PIL
───────────►│  compiler  ├───────────┤  linker  ├──────►
            │            │           │          │
            │            │           │          │
            └────────────┘           └──────────┘
```
