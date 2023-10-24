use std::hash::poseidon_gl::PoseidonGL;

machine MerkleTreeGL(LATCH, operation_id) {

    PoseidonGL poseidon;

    // TODO: Can't parse next reference in link outputs?
    // link (1 - LATCH) old_hash1, old_hash2, old_hash3, old_hash4, sibling1, sibling2, sibling3, sibling4, cap1, cap2, cap3, cap4 -> old_hash1', old_hash2', old_hash3', old_hash4' = poseidon.poseidon_permutation;

    // Workaround, add extra columns and copy over results
    col witness hash_res_old1, hash_res_old2, hash_res_old3, hash_res_old4;
    (1 - LATCH) * (hash_res_old1 - old_hash1') = 0;
    (1 - LATCH) * (hash_res_old2 - old_hash2') = 0;
    (1 - LATCH) * (hash_res_old3 - old_hash3') = 0;
    (1 - LATCH) * (hash_res_old4 - old_hash4') = 0;
    link 1 old_left1, old_left2, old_left3, old_left4, old_right1, old_right2, old_right3, old_right4, cap1, cap2, cap3, cap4 -> hash_res_old1, hash_res_old2, hash_res_old3, hash_res_old4 = poseidon.poseidon_permutation;
    

    col witness hash_res_new1, hash_res_new2, hash_res_new3, hash_res_new4;
    (1 - LATCH) * (hash_res_new1 - new_hash1') = 0;
    (1 - LATCH) * (hash_res_new2 - new_hash2') = 0;
    (1 - LATCH) * (hash_res_new3 - new_hash3') = 0;
    (1 - LATCH) * (hash_res_new4 - new_hash4') = 0;
    link 1 new_left1, new_left2, new_left3, new_left4, new_right1, new_right2, new_right3, new_right4, cap1, cap2, cap3, cap4 -> hash_res_new1, hash_res_new2, hash_res_new3, hash_res_new4 = poseidon.poseidon_permutation;
    

    
    operation update<0> key, old_value1, old_value2, old_value3, old_value4, new_value1, new_value2, new_value3, new_value4 -> old_hash1, old_hash2, old_hash3, old_hash4, new_hash1, new_hash2, new_hash3, new_hash4;

    col witness operation_id;

    let DEPTH = 10;

    let LATCH = |i| match (i % DEPTH) {DEPTH - 1 => 1, _ => 0};

    // TODO: Can we hard-code constants in a link?
    let cap1 = |i| 0;
    let cap2 = |i| 0;
    let cap3 = |i| 0;
    let cap4 = |i| 0;

    let sibling1 = |i| i % DEPTH;
    let sibling2 = |i| i % DEPTH + 1;
    let sibling3 = |i| i % DEPTH + 2;
    let sibling4 = |i| i % DEPTH + 3;

    col witness old_value1, old_value2, old_value3, old_value4;
    col witness new_value1, new_value2, new_value3, new_value4;

    col witness old_hash1, old_hash2, old_hash3, old_hash4;
    col witness new_hash1, new_hash2, new_hash3, new_hash4;

    col old_left1 = key_bit * sibling1 + (1 - key_bit) * old_hash1;
    col old_left2 = key_bit * sibling2 + (1 - key_bit) * old_hash2;
    col old_left3 = key_bit * sibling3 + (1 - key_bit) * old_hash3;
    col old_left4 = key_bit * sibling4 + (1 - key_bit) * old_hash4;
    col old_right1 = key_bit * old_hash1 + (1 - key_bit) * sibling1;
    col old_right2 = key_bit * old_hash2 + (1 - key_bit) * sibling2;
    col old_right3 = key_bit * old_hash3 + (1 - key_bit) * sibling3;
    col old_right4 = key_bit * old_hash4 + (1 - key_bit) * sibling4;

    col new_left1 = key_bit * sibling1 + (1 - key_bit) * new_hash1;
    col new_left2 = key_bit * sibling2 + (1 - key_bit) * new_hash2;
    col new_left3 = key_bit * sibling3 + (1 - key_bit) * new_hash3;
    col new_left4 = key_bit * sibling4 + (1 - key_bit) * new_hash4;
    col new_right1 = key_bit * new_hash1 + (1 - key_bit) * sibling1;
    col new_right2 = key_bit * new_hash2 + (1 - key_bit) * sibling2;
    col new_right3 = key_bit * new_hash3 + (1 - key_bit) * sibling3;
    col new_right4 = key_bit * new_hash4 + (1 - key_bit) * sibling4;


    LATCH * (old_hash1' - old_value1') = 0;
    LATCH * (old_hash2' - old_value2') = 0;
    LATCH * (old_hash3' - old_value3') = 0;
    LATCH * (old_hash4' - old_value4') = 0;

    LATCH * (new_hash1' - new_value1') = 0;
    LATCH * (new_hash2' - new_value2') = 0;
    LATCH * (new_hash3' - new_value3') = 0;
    LATCH * (new_hash4' - new_value4') = 0;
    
    col witness key;
    col witness key_bit;

    key_bit * (1 - key_bit) = 0;
    key' = (1 - LATCH) * (2 * key) + key_bit;

    (1 - LATCH) * (old_value1' - old_value1) = 0;
    (1 - LATCH) * (old_value2' - old_value2) = 0;
    (1 - LATCH) * (old_value3' - old_value3) = 0;
    (1 - LATCH) * (old_value4' - old_value4) = 0;

    (1 - LATCH) * (new_value1' - new_value1) = 0;
    (1 - LATCH) * (new_value2' - new_value2) = 0;
    (1 - LATCH) * (new_value3' - new_value3) = 0;
    (1 - LATCH) * (new_value4' - new_value4) = 0;


}


machine Main {
    degree 16384;

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
    reg X10[<=];
    reg X11[<=];
    reg X12[<=];
    reg X13[<=];
    reg X14[<=];
    reg X15[<=];
    reg X16[<=];
    reg X17[<=];
    reg A1;
    reg A2;
    reg A3;
    reg A4;
    reg A5;
    reg A6;
    reg A7;
    reg A8;

    MerkleTreeGL merkle_tree;

    instr update X1, X2, X3, X4, X5, X6, X7, X8, X9 -> X10, X11, X12, X13, X14, X15, X16, X17  = merkle_tree.update

    instr assert_eq X1, X2 {
        X1 = X2
    }

    function main {

        A1, A2, A3, A4, A5, A6, A7, A8 <== update(745, 1, 2, 3, 4, 5, 6, 7, 8);

        return;
    }
}