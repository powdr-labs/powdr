reg pc[@pc];
reg X[<=];
reg A0;
reg A1;
reg A2;
reg A3;
reg A4;
reg A5;
reg A6;
reg A7;
reg B0;
reg B1;
reg B2;
reg B3;

pil {
    pol commit in0, in1, in2, in3, in4, in5, in6, in7, hashType, cap1, cap2, cap3;
    pol commit hash0, hash1, hash2, hash3;

    pol commit is_used;

    hash0 = in0;
    hash1 = in1;
    hash2 = in2;
    hash3 = in3;
}


instr poseidon {
    {
        A0, A1, A2, A3, A4, A5, A6, A7,
        0, 0, 0, 0,
        B0, B1, B2, B3
    }
    in is_used {
        in0, in1, in2, in3, in4, in5, in6, in7,
        hashType, cap1, cap2, cap3,
        hash0, hash1, hash2, hash3
    }
}
instr loop { pc' = pc }

// Set input
A0 <=X= 0;
A1 <=X= 1;
A2 <=X= 2;
A3 <=X= 3;
A4 <=X= 4;
A5 <=X= 5;
A6 <=X= 6;
A7 <=X= 7;

poseidon;


loop;