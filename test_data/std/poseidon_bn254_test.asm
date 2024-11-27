use std::machines::hash::poseidon_bn254::PoseidonBN254;

let main_degree: int = 512;
let poseidon_degree: int = 512;

machine Main with degree: main_degree {
    reg pc[@pc];
    reg X0[<=];
    reg X1[<=];
    reg X2[<=];
    reg X3[<=];
    reg A;

    PoseidonBN254 poseidon(poseidon_degree, poseidon_degree);

    instr poseidon X0, X1, X2 -> X3 link ~> X3 = poseidon.poseidon_permutation(X0, X1, X2);

    instr assert_eq X0, X1 {
        X0 = X1
    }

    function main {

        // Test vector for poseidonperm_x5_254_3 from:
        // https://extgit.iaik.tugraz.at/krypto/hadeshash/-/blob/master/code/test_vectors.txt
        A <== poseidon(0, 1, 2);
        assert_eq A, 0x115cc0f5e7d690413df64c6b9662e9cf2a3617f2743245519e19607a4417189a;

        // Validated by modifying and running `code/poseidonperm_x5_254_3.sage` from the same repository.
        A <== poseidon(0, 0, 0);
        assert_eq A, 0x2098f5fb9e239eab3ceac3f27b81e481dc3124d55ffed523a839ee8446b64864;
        A <== poseidon(-1, -2, -3);
        assert_eq A, 0x15492e60e5ae9f3d254f2d44650795c4cac1c924981fb7ca8645a7790971b70c;
        A <== poseidon(A + 1, A + 2, A + 3);
        assert_eq A, 0x188ada144ed909426b0396e967a82e26d739652cff288d13306279d91f29010c;

        return;
    }
}
