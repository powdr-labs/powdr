use std::array;
use std::check::assert;
use std::utils::unchanged_until;
use std::utils::force_bool;
use std::utils::sum;
use std::convert::expr;
use std::machines::memory::Memory;
use std::machines::split::split_bb::SplitBB;

// Implements the Poseidon2 permutation for the BabyBear.
//
// Apparently it can be used to hash arbitrary sized data by using the
// Merkle–Damgård construction, or it can be used as a compression function
// for building a Merkle tree.
//
// As it stands, it can not be used in a Sponge construction, because we don't
// output the entire state.
machine PoseidonGLMemory(mem: Memory, split_BB: SplitBB) with
    latch: CKL_0,
    operation_id: operation_id,
    // Allow this machine to be connected via a permutation
    call_selectors: sel,
{
    // The input data is passed via a memory pointer: the machine will read STATE_SIZE
    // field elements from memory, in pairs of 16-bit limbs for BabyBear.
    //
    // Similarly, the output data is written to memory at the provided pointer.
    //
    // Reads happen at the provided time step; writes happen at the next time step.
    operation poseidon_permutation<0> input_addr, output_addr, time_step ->;
    col witness operation_id;

    // Poseidon2 parameters, compatible with our powdr-plonky3 implementation.
    //
    // The the number of rounds to get 128-bit security was taken from here:
    // https://github.com/Plonky3/Plonky3/blob/2df15fd05e2181b31b39525361aef0213fc76144/poseidon2/src/round_numbers.rs#L42

    // S-box degree (this constant is actually not used, because we have to break the exponentiation into steps of at most degree 3).
    let SBOX_DEGREE: int = 7;

    // Number of field elements in the state
    let STATE_SIZE: int = 16;

    // Number of output elements
    // (TODO: to use the Sponge construction, the entire state should be output)
    let OUTPUT_SIZE: int = 8;

    // Half the number of external rounds (half of external rounds happen before and half after the internal rounds).
    let HALF_EXTERNAL_ROUNDS: int = 4;

    // Number of internal rounds
    let INTERNAL_ROUNDS: int = 13;

    // External round MDS matrix
    let MDS = [
        [4, 6, 2, 2, 2, 3, 1, 1, 2, 3, 1, 1, 2, 3, 1, 1],
        [2, 4, 6, 2, 1, 2, 3, 1, 1, 2, 3, 1, 1, 2, 3, 1],
        [2, 2, 4, 6, 1, 1, 2, 3, 1, 1, 2, 3, 1, 1, 2, 3],
        [6, 2, 2, 4, 3, 1, 1, 2, 3, 1, 1, 2, 3, 1, 1, 2],
        [2, 3, 1, 1, 4, 6, 2, 2, 2, 3, 1, 1, 2, 3, 1, 1],
        [1, 2, 3, 1, 2, 4, 6, 2, 1, 2, 3, 1, 1, 2, 3, 1],
        [1, 1, 2, 3, 2, 2, 4, 6, 1, 1, 2, 3, 1, 1, 2, 3],
        [3, 1, 1, 2, 6, 2, 2, 4, 3, 1, 1, 2, 3, 1, 1, 2],
        [2, 3, 1, 1, 2, 3, 1, 1, 4, 6, 2, 2, 2, 3, 1, 1],
        [1, 2, 3, 1, 1, 2, 3, 1, 2, 4, 6, 2, 1, 2, 3, 1],
        [1, 1, 2, 3, 1, 1, 2, 3, 2, 2, 4, 6, 1, 1, 2, 3],
        [3, 1, 1, 2, 3, 1, 1, 2, 6, 2, 2, 4, 3, 1, 1, 2],
        [2, 3, 1, 1, 2, 3, 1, 1, 2, 3, 1, 1, 4, 6, 2, 2],
        [1, 2, 3, 1, 1, 2, 3, 1, 1, 2, 3, 1, 2, 4, 6, 2],
        [1, 1, 2, 3, 1, 1, 2, 3, 1, 1, 2, 3, 2, 2, 4, 6],
        [3, 1, 1, 2, 3, 1, 1, 2, 3, 1, 1, 2, 6, 2, 2, 4]
    ];

    // Diagonal of the internal round diffusion matrix
    let DIFF_DIAGONAL = [-2, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 32768];

    // A multiplier for our diffusion matrix. Not in the original Poseidon2 paper,
    // but needed to match the choice of matrix in the Plonky3 implementation.
    // (They decided to use the Montgomery form of the matrix.)
    let DIFF_MULTIPLIER = 268435454;

    // External round constants, one STATE_SIZE array for each round
    let EXTERNAL_ROUND_CONSTANTS = [
        [781065863, 1704334099, 1614250469, 858342508, 1331255579, 94027721, 1633402383, 1774536800, 967783090, 1429869924, 37790139, 1067472776, 1703182141, 1722007170, 826573738, 1380955441],
        [1173986918, 427450465, 703550610, 214947471, 810976863, 1569294983, 1294224805, 40193270, 858808123, 1982585188, 797628021, 273000383, 570536182, 1015052027, 1622799895, 1845434468],
        [393329457, 870203221, 56318764, 1364908618, 929735258, 410647527, 1272874215, 1250307830, 1985094168, 1183107810, 290944485, 1431023892, 1514015400, 150034509, 1932176786, 113929158],
        [314648554, 412945090, 1799565197, 1437543685, 210037341, 267254220, 1123299502, 1012046526, 1811748296, 1082880104, 452117508, 591556198, 26422375, 928482204, 1782339126, 471400423],
        [1715755484, 1620279079, 898856400, 1060851389, 1774418870, 1523201093, 9015542, 500181102, 1011868729, 1943785875, 410764106, 1856107565, 1977593067, 1362094997, 1586847440, 1751322463],
        [1820671903, 712390866, 1344285673, 1301479607, 1447437124, 1817620797, 796225227, 1958608680, 1934746594, 688362361, 1897565392, 242159596, 1362690728, 1540780945, 309719651, 1780905031],
        [1403665294, 1889289665, 1998617149, 1455767632, 497240095, 309963516, 1683981810, 1877298991, 868046153, 890940275, 283303262, 145680600, 1105472003, 1676373559, 940577289, 233213338],
        [369884595, 39502463, 1425277724, 951005540, 1216021342, 381524560, 1062589222, 1537626390, 347091819, 781614254, 1465862749, 611525604, 1661958720, 1585470899, 726892227, 1080833156],
    ];

    // Internal round constants, one for each round
    let INTERNAL_ROUND_CONSTANTS = [
        24257283,
        674575296,
        1088287909,
        1109797649,
        1389124060,
        1378384487,
        973925592,
        675566589,
        772033245,
        402697045,
        386924216,
        310894738,
        1235941928,
    ];

    // The linear layer of the external round.
    //
    // Doesn't have to be a complete matrix multiplication, as the last round discards
    // part of the state, so we can skip the corresponding rows in the matrix.
    let apply_mds = constr |input, output_len|{
        let dot_product = |v1, v2| array::sum(array::new(v1, v2, |v1_i, v2_i| v1_i * v2_i));
        array::map(array::sub_array(MDS, 0, output_len), |row| dot_product(row, input))
    };

    let external_round = constr |c_idx, input, output| {
        // Add constants
        let step_a = array::zip(input, EXTERNAL_ROUND_CONSTANTS[c_idx], |v, c| v + c);

        // Apply S-box
        col witness x3[STATE_SIZE];
        array::zip(x3, array::map(step_a, |v| v * v * v), |x3, expected| x3 = expected);
        col witness x7[STATE_SIZE];
        array::zip(x7, array::zip(x3, step_a, |x3, v| x3 * x3 * v), |x7, expected| x7 = expected);

        // Multiply with MDS Matrix
        output = apply_mds(x7, array::len(output));
    };

    let internal_round = constr |c_idx, input, output| {
        // Add constant (weird, I thought the entire state was used here,
        // but this is how Plonky3 does it).
        let step_a = input[0] + INTERNAL_ROUND_CONSTANTS[c_idx];

        // Apply S-box
        col witness x3;
        x3 = step_a * step_a * step_a;
        col witness x7;
        x7 = x3 * x3 * step_a;

        // Multiply with the diffusion matrix
        let line_sum = x7 + array::sum(array::sub_array(input, 1, STATE_SIZE - 1));
        output[0] = (line_sum + DIFF_DIAGONAL[0]) * x7 * DIFF_MULTIPLIER;
        
        )
        array::zip(
            array::zip(
                array::sub_array(input, 1, STATE_SIZE - 1),
                array::sub_array(output, 1, STATE_SIZE - 1),
                |in_out| in_out
            ),
            array::sub_array(DIFF_DIAGONAL, 1, STATE_SIZE - 1),
            |(in_v, out_v), diff| out_v = (line_sum + diff) * in_v * DIFF_MULTIPLIER;
        );
    }

    // Load all the inputs in the first time step
    col witness input[STATE_SIZE];
    array::map_enumerated(input, constr |i, val| {
        col witness word_low, word_high;
        [word_high, word_low] in mem.mload(input_addr + 4 * i, time_step);
        input[i] = word_low + word_high * 2**16;
    });

    // The rounds:
    col witness rounds[1 + 2*HALF_EXTERNAL_ROUNDS + INTERNAL_ROUNDS][STATE_SIZE];

    // Perform the inital MDS step
    rounds[0] = apply_mds(input, STATE_SIZE);

    // Perform the first half of the external rounds
    array::map_enumerated(
        array::zip(
            array::sub_array(rounds, 0, HALF_EXTERNAL_ROUNDS),
            array::sub_array(rounds, 1, HALF_EXTERNAL_ROUNDS),
            |in_out| in_out,
        ),
        constr |i, (in, out)| external_round(i, in, out)
    );

    // Perform the internal rounds
    array::map_enumerated(
        array::zip(
            array::sub_array(rounds, HALF_EXTERNAL_ROUNDS, INTERNAL_ROUNDS),
            array::sub_array(rounds, HALF_EXTERNAL_ROUNDS + 1, INTERNAL_ROUNDS),
            |in_out| in_out,
        ),
        constr |i, (in, out)| internal_round(i, in, out)
    );

    // Perform the second half of the external rounds
    let second_external_start = HALF_EXTERNAL_ROUNDS + INTERNAL_ROUNDS;
    array::map_enumerated(
        array::zip(
            array::sub_array(rounds, second_external_start, HALF_EXTERNAL_ROUNDS),
            array::sub_array(rounds, second_external_start + 1, HALF_EXTERNAL_ROUNDS),
            |in_out| in_out,
        ),
        constr |i, (in, out)| external_round(i + HALF_EXTERNAL_ROUNDS, in, out)
    );

    // Write the output in the second time step
    array::map_enumerated(rounds[second_external_start + HALF_EXTERNAL_ROUNDS], constr |i, val| {
        col witness word_low, word_high;
        [word_low, word_high] in split_bb.split(val);
        [] = mem.mstore(output_addr + 4 * i, time_step + 1, word_high, word_low); //what is the syntax here???
    });
}
