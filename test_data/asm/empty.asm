// this cannot quite be empty yet, as we rely on fixed columns to determine the degree. TODO: change this because it's odd
// also, for halo2 to generate a proof, we need at least one constraint.
machine Empty {
    col fixed A(i) { i };
    col witness w;
    w = w * w;
}