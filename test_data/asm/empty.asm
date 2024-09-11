// this cannot quite be empty yet, as we rely on at least one constraint existing for halo2 to generate a proof. TODO: change this
machine Empty with degree: 4 {
    let w;
    w = w * w;
}