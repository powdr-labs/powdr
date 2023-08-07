# Linker

powdr machine instances are compiled into separate AIRs, where each consists of:
- an optional degree
- a list of PIL statements
- a list of links to other machine instances

A linker is used to turn this tree of AIRs into a structure we can generate proofs about. This currently takes the form of a single PIL file.
The linking process operates in the following way:

1. Create an empty PIL file
2. Start from the main AIR. If it defines a degree, we call it `main_degree`. If it does not, set it to the default of `1024`.
3. For each AIR
    1. Create a new namespace in the PIL file
    2. If a degree is defined, check that it matches `main_degree`. If no degree is defined, set the degree to `main_degree`.
    3. Add the constraints to the namespace
    4. Turn the links into lookups and add them to the namespace

The result is a monolithic AIR where each machine instance is a namespace, where all namespaces have the same degree and links between instances are guaranteed by lookup identities.

> More flexible approaches to the linking process will be explored in the future, such as allowing for machine instances of different degrees.