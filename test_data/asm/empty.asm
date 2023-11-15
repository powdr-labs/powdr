// this cannot quite be empty yet, as we rely on at least one constraint existing for halo2 to generate a proof. TODO: change this
machine Empty {
    col witness w;
    w = w * w;
}