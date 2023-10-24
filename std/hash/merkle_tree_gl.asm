use std::hash::poseidon_gl::PoseidonGL;

machine MerkleTreeGL(LATCH, operation_id) {

    PoseidonGL poseidon;
    // TODO: Can't parse next reference in link outputs?
    // link (1 - LATCH) hash_old1, hash_old2, hash_old3, hash_old4, sibling1, sibling2, sibling3, sibling4, cap1, cap2, cap3, cap4 -> hash_old1', hash_old2', hash_old3', hash_old4' = poseidon.poseidon_permutation;

    col witness hash_res1, hash_res2, hash_res3, hash_res4;
    link 1 hash_old1, hash_old2, hash_old3, hash_old4, sibling1, sibling2, sibling3, sibling4, cap1, cap2, cap3, cap4 -> hash_res1, hash_res2, hash_res3, hash_res4 = poseidon.poseidon_permutation;
    
    (1 - LATCH) * (hash_res1 - hash_old1') = 0;
    (1 - LATCH) * (hash_res2 - hash_old2') = 0;
    (1 - LATCH) * (hash_res3 - hash_old3') = 0;
    (1 - LATCH) * (hash_res4 - hash_old4') = 0;

    
    operation update<0> key, old_value1, old_value2, old_value3, old_value4 -> hash_old1, hash_old2, hash_old3, hash_old4;

    col witness operation_id;

    let DEPTH = 10;

    let LATCH = |i| match (i % DEPTH) {DEPTH - 1 => 1, _ => 0};
    let cap1 = |i| 0;
    let cap2 = |i| 0;
    let cap3 = |i| 0;
    let cap4 = |i| 0;

    let sibling1 = |i| i % DEPTH;
    let sibling2 = |i| i % DEPTH + 1;
    let sibling3 = |i| i % DEPTH + 2;
    let sibling4 = |i| i % DEPTH + 3;

    col witness old_value1, old_value2, old_value3, old_value4;

    col witness hash_old1, hash_old2, hash_old3, hash_old4;
    //col witness hash_old1(i) query match (LATCH) { 1 => ("hint", 4) };
    //col witness hash_old2(i) query match (LATCH) { 1 => ("hint", 4) };
    //col witness hash_old3(i) query match (LATCH) { 1 => ("hint", 4) };
    //col witness hash_old4(i) query match (LATCH) { 1 => ("hint", 4) };

    LATCH * (hash_old1' - old_value1') = 0;
    LATCH * (hash_old2' - old_value2') = 0;
    LATCH * (hash_old3' - old_value3') = 0;
    LATCH * (hash_old4' - old_value4') = 0;
    
    col witness key;
    col witness key_bit;

    key_bit * (1 - key_bit) = 0;
    key' = (1 - LATCH) * (2 * key) + key_bit;

    (1 - LATCH) * (old_value1' - old_value1) = 0;
    (1 - LATCH) * (old_value2' - old_value2) = 0;
    (1 - LATCH) * (old_value3' - old_value3) = 0;
    (1 - LATCH) * (old_value4' - old_value4) = 0;


}


machine Main {
    degree 256;

    reg pc[@pc];
    reg X1[<=];
    reg X2[<=];
    reg X3[<=];
    reg X4[<=];
    reg X5[<=];
    reg X6[<=];
    reg X7[<=];
    reg X8[<=];
    reg X9[<=];
    reg A;
    reg B;
    reg C;
    reg D;

    MerkleTreeGL merkle_tree;

    instr update X1, X2, X3, X4, X5 -> X6, X7, X8, X9 = merkle_tree.update

    instr assert_eq X1, X2 {
        X1 = X2
    }

    function main {

        A, B, C, D <== update(745, 1, 2, 3, 4);

        return;
    }
}