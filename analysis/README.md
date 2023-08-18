# Analysis

This crate is where most of the compilation pipeline lives. It takes a parse tree and returns a tree of constrained machines.

## Definitions

We define two types of machines: virtual machines and constrained machines. Constrained machines are the lower level kind of machine. They have a notion of blocks through a latch and a function_id. Virtual machines are a higher level type of machines. For each machine type, we provide the elements which only appear exclusively in that type.

|                         | Virtual | Constrained |
|-------------------------|---------|-------------|
| pc                      | yes     | no          |
| latch                   | no      | yes         |
| function_id             | no      | yes         |
| functions bodies        | yes     | no          |
| internal instructions   | yes     | no          |

The pipeline accepts both kinds of machines, and they are represented by the same type `Machine`. Some steps are specific to virtual machines. They can still be applied to constrained machines and must have no effect. In the process, virtual machines get reduced to constrained machines by encoding their high-level elements into constrained machines elements.

## Pipeline

### Type checking

Type checking takes a parse tree and returns a tree of machines. The output type aims at being as strict as possible.

### Virtual machine reduction

Virtual machine reduction turns virtual machines into constrained machines. It has no effect on contrained machines.

#### Inference

Inference infers the value of assignment registers.

#### Batcher

The batcher groups statements within each function into batches which can be executed in the same execution step.

#### ASM to PIL

ASM to PIL has two steps: ROM generation and reduction to constrained.

##### ROM generation

Rom generation generates a single ROM for each virtual machine using the following process:
- Find the maximum number of inputs among all functions. Introduce as many input registers. Do the same for outputs, introducing output registers
- Replace references to the function arguments by references to these input registers.
- Pad all return statements with zeroes up to the number of output registers.
- Inline all functions, adding a label before each one.
- Add an infinite loop also behind a label
- Enable non-deterministically jumping to one of these labels by setting `function_id` to the index of the label

This process introduces instructions and registers, and returns the modified machine along with the ROM.

##### VM to constrained

Once we have the ROM for a machine, we reduce it to constraints. Some parts of this process are specific to our dispatcher implementation:
- All input registers are unconstrained when `_reset` is on, allowing to pass inputs.
- `return` is treated as an instruction even though it is not declared as such

As a result, we obtain a constrained machine for each original virtual machine. The `function_id` and `latch` are set using respectively the location of the machine functions inside the ROM and the instruction flag of `return`.

### Block enforcer

This step takes constrained machines and enforces that the `function_id` can only change if the `latch` is on. This defines blocks of computation which can be created based on the functions exposed by each machine.

### Airgen

Airgen takes machines which are only left with constraints and external instructions and instanciates them as a tree of AIR objects. Objects can point to each other using links, which encode the interaction between different machines.

### Linker

The linker takes a tree of machines and instanciates specific structures to make proofs about. In our current implementation, it simply collates all AIR objects next to each other in a single table.

