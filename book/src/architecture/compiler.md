# Compiler

In this section, we explain how the powdr compiler reduces a program made of virtual and constrained machines to a set of AIRs.

## Virtual machine reduction

The first step is to reduce virtual machines to constrained machines. This step is run on all machines, it simply does not affect constrained machines.
As a result of this step, for each machine:
- [Local instructions](../asm/instructions.md#local-instructions) are reduced to constraints
- [External instructions](../asm/instructions.md#external-instructions) are reduced to links
- [Functions](../asm/functions.md) are reduced to [operations](../asm/operations.md)

## Block enforcement

Block enforcement applies on constrained machines. It makes sure that the `operation_id` is constant within each machine block.

## AIR generation

At this point, all machines contain only:
- an optional degree
- constraints
- links
- operations

This makes each of them directly convertible into an AIR.