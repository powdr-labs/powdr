use std::machines::hash::poseidon_gl::PoseidonGL;

// 2^17 rows -> ~2^16 Poseidon hashes
machine Main with degree: 131072 {
    reg pc[@pc];
    reg X0[<=];
    reg X1[<=];
    reg X2[<=];
    reg X3[<=];
    reg X4[<=];
    reg A;
    reg B;
    reg C;
    reg D;

    // 2^16 Poseidon hashes -> 2^21 rows in Poseidon machine
    PoseidonGL poseidon(2097152, 2097152);

    instr jmp l: label { pc' = l }

    // Hash some constants (except the first element, to trick the loop detection)
    instr poseidon X4 -> X0, X1, X2, X3
        link ~> (X0, X1, X2, X3) = poseidon.poseidon_permutation(X4, 235763497586, 9827635653498, 112870, 289273673480943876, 230295874986745876, 6254867324987, 2087, 0, 0, 0, 0);

    function main {

        loop:
        A, B, C, D <== poseidon(A);
        jmp loop;

        return;
    }
}
