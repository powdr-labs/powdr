use std::hash::poseidon_bn254::PoseidonBN254;
use std::array;

machine Main {
    degree 512;

    reg pc[@pc];
    reg X0[<=];
    reg X1[<=];
    reg X2[<=];
    reg X3[<=];
    reg A;

    // Memory
    col witness m_addr;
    col witness m_step;
    col witness m_change;
    col witness m_value;
    col witness m_is_write, m_is_write_poseidon;
    col witness m_is_read, m_is_read_poseidon;

    // Makes sure all m_is_* columns are boolean and at most one is active
    let ONE_HOT_OR_NONE: col[4] = array::new(4, |i| |row| if row % 5 == i { 1 } else { 0 });
    {m_is_write, m_is_write_poseidon, m_is_read, m_is_read_poseidon } in { ONE_HOT_OR_NONE[0], ONE_HOT_OR_NONE[1], ONE_HOT_OR_NONE[2], ONE_HOT_OR_NONE[3] };

    col fixed POSITIVE(i) { i + 1 };
    col fixed LAST  = [0]* + [1];
    col fixed STEP(i) { i };

    m_change * (1 - m_change) = 0;
    (m_addr' - m_addr) * (1 - m_change) = 0;
    (1 - LAST) { m_change * (m_addr' - m_addr) + (1 - m_change) * (m_step' - m_step) } in POSITIVE;
    (1 - m_change) * LAST = 0;
    (1 - m_is_write' - m_is_write_poseidon') * (1 - m_change) * (m_value' - m_value) = 0;
    (1 - m_is_write' - m_is_write_poseidon') * m_change * m_value' = 0;

    instr mstore X0, X1 { { X0, STEP, X1 } is m_is_write { m_addr, m_step, m_value } }
    instr mload X0 -> X1 { { X0, STEP, X1 } is m_is_read { m_addr, m_step, m_value } }

    PoseidonBN254 poseidon;

    instr poseidon X0, X1 -> = poseidon.poseidon_permutation;

    instr assert_eq X0, X1 {
        X0 = X1
    }

    function main {

        // Test vector for poseidonperm_x5_254_3 from:
        // https://extgit.iaik.tugraz.at/krypto/hadeshash/-/blob/master/code/test_vectors.txt
        
        // Memory addresses need to be "aligned" (i.e., divisible by 4)
        mstore 40, 0;
        mstore 44, 1;
        mstore 48, 2;
        poseidon 40, 400;
        A <== mload(400);
        // This fails unless the Poseidon machine writes the correct result to memory, which requires PIL modification.
        assert_eq A, 0x115cc0f5e7d690413df64c6b9662e9cf2a3617f2743245519e19607a4417189a;

        // Validated by modifying and running `code/poseidonperm_x5_254_3.sage` from the same repository.
        // A <== poseidon(0, 0, 0);
        // assert_eq A, 0x2098f5fb9e239eab3ceac3f27b81e481dc3124d55ffed523a839ee8446b64864;
        // A <== poseidon(-1, -2, -3);
        // assert_eq A, 0x15492e60e5ae9f3d254f2d44650795c4cac1c924981fb7ca8645a7790971b70c;
        // A <== poseidon(A + 1, A + 2, A + 3);
        // assert_eq A, 0x188ada144ed909426b0396e967a82e26d739652cff288d13306279d91f29010c;

        return;
    }
}