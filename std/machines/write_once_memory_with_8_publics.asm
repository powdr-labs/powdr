use std::array;

// Very simple write-once memory that lets you store any field element at
// an address from 0 to 7. Any value can be written only once, writing two
// different values to the same address fails. If an uninitialized cell is
// read, the returned value is unconstrained.
// A typical use-case would be to pass the `value` column as an "external"
// witness. This way the prover can provide some input vector and the program
// can read the same input multiple times.
machine WriteOnceMemoryWith8Publics with
	latch: LATCH,
	degree: 8,
{

    // Accesses the memory cell at the given address. This can be used
    // both for reading and writing, e.g.:
    //   instr mload X -> Y link => memory.access(X, Y);
    //   instr mstore X, Y -> link => memory.access(X, Y);
    operation access ADDR, value ->;

    let LATCH = 1;

    let ADDR: col = |i| i;
    let value;

    public hash_0 = value(0);
    public hash_1 = value(1);
    public hash_2 = value(2);
    public hash_3 = value(3);
    public hash_4 = value(4);
    public hash_5 = value(5);
    public hash_6 = value(6);
    public hash_7 = value(7);

    // Public references
    let EXPOSE: col[8] = array::new(8, |i| |row| if row % 8 == i { 1 } else { 0 });

    EXPOSE[0] * (hash_0 - value) = 0;
    EXPOSE[1] * (hash_1 - value) = 0;
    EXPOSE[2] * (hash_2 - value) = 0;
    EXPOSE[3] * (hash_3 - value) = 0;
    EXPOSE[4] * (hash_4 - value) = 0;
    EXPOSE[5] * (hash_5 - value) = 0;
    EXPOSE[6] * (hash_6 - value) = 0;
    EXPOSE[7] * (hash_7 - value) = 0;
}
