# Linker

A linker is used to turn an [AIR tree](./compiler.md#air-generation) into a single PIL file.
The linking process operates in the following way:

1. Create an empty PIL file
2. Start from the main AIR. Let `main_degree_range` be its degree.
3. For each AIR
    1. Create a new namespace in the PIL file
        2a. If `degrees-mode` is `vadcop`, set the namespace degree to that of the AIR
        2b. If `degrees-mode` is `monolithic`, set the namespace degree to `main_degree_range`
    3. Add the constraints to the namespace
    4. Turn the links into lookups and permutations and add them to the namespace

The result is a monolithic AIR where:
- each machine instance is a namespace
- each namespace defines its own degree range