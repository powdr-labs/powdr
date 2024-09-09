# Linker

A linker is used to turn an [AIR tree](./compiler.md#air-generation) into a single PIL file.
The linking process operates in the following way:

1. Create an empty PIL file
2. Start from the main AIR. Let `optional_main_degree` be its optional degree.
3. For each AIR
    1. Create a new namespace in the PIL file
    2. If a degree is defined, set it as the namespace degree. If no degree is defined, set the namespace degree to `optional_main_degree`
    3. Add the constraints to the namespace
    4. Turn the links into lookups and add them to the namespace

The result is a monolithic AIR where:
- each machine instance is a namespace
- each namespace defines its own optional degree