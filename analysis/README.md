# Analysis

This crate is where most of the compilation pipeline lives. It takes a parse tree and returns a tree of constrained machines.

## Virtual and constrained machines

| Machine               | Virtual | Constrained |
|-----------------------|---------|-------------|
| pc                    | yes     | no          |
| latch                 | no      | yes         |
| function_id           | no      | yes         |
| functions bodies      | yes     | no          |
| internal instructions | yes     | no          |

## Pipeline

### Type checking

Type checking takes a parse tree and returns a tree of machines. The output type aims at being as strict as possible.

### Virtual machine reduction

Virtual machine reduction turns virtual machines into constrained machines. It has no effect on machines which are already constrained.

#### Inference

Inference infers the value of assignment registers.

#### Batcher

The batcher groups statements within each function into batches which can be executed in the same execution step.

#### ASM to PIL

ASM to PIL has two steps: ROM generation and ROM reduction to fixed columns

##### ROM generation

Rom generation generates a single ROM for each virtual machine using the following process:
- Find the maximum number of inputs among all functions. Introduce as many input registers. Do the same for outputs, introducing output registers
- Replace references to the function arguments by references to these input registers.
- Pad all return statements with zeroes up to the number of output registers.
- Inline all functions, adding a label before each one.
- Add an infinite loop also behind a label
- Enable non-deterministically jumping to one of these labels by setting `function_id` to the index of the label

This process introduces instructions and registers, and returns the modified machine along with the ROM.

##### ROM to fixed

Once we have the ROM for a machine, we reduce it to constraints. Some parts of this process are specific to our dispatcher implementation:
- All input registers are unconstrained when `_reset` is on, allowing to pass inputs.
- `return` is treated as an instruction even though it is not declared as such

