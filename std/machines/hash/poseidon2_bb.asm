use std::array;
use std::check::assert;
use std::utils::unchanged_until;
use std::utils::force_bool;
use std::utils::sum;
use std::convert::expr;
use std::machines::small_field::memory::Memory;
use std::machines::small_field::pointer_arith::address_array_elems;
use std::machines::split::split_bb::SplitBB;
use super::poseidon2_common::pow_7;
use super::poseidon2_common::poseidon2;

// Implements the Poseidon2 permutation for BabyBear field.
//
// It can be used to hash arbitrary sized data with sponge construction, or
// it can be used as a compression function for building a Merkle tree.
machine Poseidon2BB(mem: Memory, split_BB: SplitBB) with
    latch: latch,
    // Allow this machine to be connected via a permutation
    call_selectors: sel,
{
    // Is this a used row?
    let is_used = array::sum(sel);
    force_bool(is_used);

    // The input data is passed via a memory pointer: the machine will read STATE_SIZE
    // field elements from memory, in pairs of 16-bit limbs for BabyBear.
    //
    // Similarly, the output data is written to memory at the provided pointer.
    //
    // Reads happen at the provided time step; writes happen at the next time step.
    operation poseidon2_permutation
        input_addr_high[0], input_addr_low[0],
        output_addr_high[0], output_addr_low[0],
        time_step ->;

    let latch = 1;

    let time_step;

    // Poseidon2 parameters, compatible with our powdr-plonky3 implementation.
    //
    // The number of rounds to get 128-bit security was taken from here:
    // https://github.com/Plonky3/Plonky3/blob/2df15fd05e2181b31b39525361aef0213fc76144/poseidon2/src/round_numbers.rs#L42

    // Number of field elements in the state
    let STATE_SIZE: int = 16;

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
    // (They decided to use a scaled form of the matrix, to facilitate operations in montgomery form.)
    let DIFF_MULTIPLIER = 943718400;

    // External round constants, one STATE_SIZE array for each round
    let EXTERNAL_ROUND_CONSTANTS = [
        [781065863, 1704334099, 1614250469, 858342508, 1331255579, 94027721, 1633402383, 1774536800, 967783090, 1429869924, 37790139, 1067472776, 1703182141, 1722007170, 826573738, 1380955441],
        [1173986918, 427450465, 703550610, 214947471, 810976863, 1569294983, 1294224805, 40193270, 858808123, 1982585188, 797628021, 273000383, 570536182, 1015052027, 1622799895, 1845434468],
        [393329457, 870203221, 56318764, 1364908618, 929735258, 410647527, 1272874215, 1250307830, 1985094168, 1183107810, 290944485, 1431023892, 1514015400, 150034509, 1932176786, 113929158],
        [314648554, 412945090, 1799565197, 1437543685, 210037341, 267254220, 1123299502, 1012046526, 1811748296, 1082880104, 452117508, 591556198, 26422375, 928482204, 1782339126, 471400423],
        [1715755484, 1620279079, 898856400, 1060851389, 1774418870, 1523201093, 9015542, 500181102, 1011868729, 1943785875, 410764106, 1856107565, 1977593067, 1362094997, 1586847440, 1751322463],
        [1820671903, 712390866, 1344285673, 1301479607, 1447437124, 1817620797, 796225227, 1958608680, 1934746594, 688362361, 1897565392, 242159596, 1362690728, 1540780945, 309719651, 1780905031],
        [1403665294, 1889289665, 1998617149, 1455767632, 497240095, 309963516, 1683981810, 1877298991, 868046153, 890940275, 283303262, 145680600, 1105472003, 1676373559, 940577289, 233213338],
        [369884595, 39502463, 1425277724, 951005540, 1216021342, 381524560, 1062589222, 1537626390, 347091819, 781614254, 1465862749, 611525604, 1661958720, 1585470899, 726892227, 1080833156]
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
        1235941928
    ];

    // Calculate the addresses and load all the inputs into the first time step
    let input_addr_high: col[STATE_SIZE];
    let input_addr_low: col[STATE_SIZE];
    address_array_elems(input_addr_high, input_addr_low);

    let input_low: col[STATE_SIZE];
    let input_high: col[STATE_SIZE];
    // TODO: when link is available inside functions, we can turn this into array operations.
    link if is_used ~> (input_high[0], input_low[0]) = mem.mload(input_addr_high[0], input_addr_low[0], time_step);
    link if is_used ~> (input_high[1], input_low[1]) = mem.mload(input_addr_high[1], input_addr_low[1], time_step);
    link if is_used ~> (input_high[2], input_low[2]) = mem.mload(input_addr_high[2], input_addr_low[2], time_step);
    link if is_used ~> (input_high[3], input_low[3]) = mem.mload(input_addr_high[3], input_addr_low[3], time_step);
    link if is_used ~> (input_high[4], input_low[4]) = mem.mload(input_addr_high[4], input_addr_low[4], time_step);
    link if is_used ~> (input_high[5], input_low[5]) = mem.mload(input_addr_high[5], input_addr_low[5], time_step);
    link if is_used ~> (input_high[6], input_low[6]) = mem.mload(input_addr_high[6], input_addr_low[6], time_step);
    link if is_used ~> (input_high[7], input_low[7]) = mem.mload(input_addr_high[7], input_addr_low[7], time_step);
    link if is_used ~> (input_high[8], input_low[8]) = mem.mload(input_addr_high[8], input_addr_low[8], time_step);
    link if is_used ~> (input_high[9], input_low[9]) = mem.mload(input_addr_high[9], input_addr_low[9], time_step);
    link if is_used ~> (input_high[10], input_low[10]) = mem.mload(input_addr_high[10], input_addr_low[10], time_step);
    link if is_used ~> (input_high[11], input_low[11]) = mem.mload(input_addr_high[11], input_addr_low[11], time_step);
    link if is_used ~> (input_high[12], input_low[12]) = mem.mload(input_addr_high[12], input_addr_low[12], time_step);
    link if is_used ~> (input_high[13], input_low[13]) = mem.mload(input_addr_high[13], input_addr_low[13], time_step);
    link if is_used ~> (input_high[14], input_low[14]) = mem.mload(input_addr_high[14], input_addr_low[14], time_step);
    link if is_used ~> (input_high[15], input_low[15]) = mem.mload(input_addr_high[15], input_addr_low[15], time_step);

    // Assemble the two limbs of the input
    let input = array::zip(input_low, input_high, |low, high| low + 0x10000 * high);

    // Generate the Poseidon2 permutation
    let output = poseidon2(
        STATE_SIZE,
        HALF_EXTERNAL_ROUNDS,
        INTERNAL_ROUNDS,
        MDS,
        EXTERNAL_ROUND_CONSTANTS,
        DIFF_DIAGONAL,
        DIFF_MULTIPLIER,
        INTERNAL_ROUND_CONSTANTS,
        pow_7,
        input
    );

    // Split the output into high and low limbs
    let output_low: col[STATE_SIZE];
    let output_high: col[STATE_SIZE];
    // TODO: turn this into array operations
    link if is_used ~> (output_low[0], output_high[0]) = split_BB.split(output[0]);
    link if is_used ~> (output_low[1], output_high[1]) = split_BB.split(output[1]);
    link if is_used ~> (output_low[2], output_high[2]) = split_BB.split(output[2]);
    link if is_used ~> (output_low[3], output_high[3]) = split_BB.split(output[3]);
    link if is_used ~> (output_low[4], output_high[4]) = split_BB.split(output[4]);
    link if is_used ~> (output_low[5], output_high[5]) = split_BB.split(output[5]);
    link if is_used ~> (output_low[6], output_high[6]) = split_BB.split(output[6]);
    link if is_used ~> (output_low[7], output_high[7]) = split_BB.split(output[7]);
    link if is_used ~> (output_low[8], output_high[8]) = split_BB.split(output[8]);
    link if is_used ~> (output_low[9], output_high[9]) = split_BB.split(output[9]);
    link if is_used ~> (output_low[10], output_high[10]) = split_BB.split(output[10]);
    link if is_used ~> (output_low[11], output_high[11]) = split_BB.split(output[11]);
    link if is_used ~> (output_low[12], output_high[12]) = split_BB.split(output[12]);
    link if is_used ~> (output_low[13], output_high[13]) = split_BB.split(output[13]);
    link if is_used ~> (output_low[14], output_high[14]) = split_BB.split(output[14]);
    link if is_used ~> (output_low[15], output_high[15]) = split_BB.split(output[15]);

    // Write the output to memory at the next time step
    let output_addr_high: col[STATE_SIZE];
    let output_addr_low: col[STATE_SIZE];
    address_array_elems(output_addr_high, output_addr_low);
    // TODO: turn this into array operations
    link if is_used ~>
        mem.mstore(output_addr_high[0], output_addr_low[0], time_step + 1, output_high[0], output_low[0]);
    link if is_used ~>
        mem.mstore(output_addr_high[1], output_addr_low[1], time_step + 1, output_high[1], output_low[1]);
    link if is_used ~>
        mem.mstore(output_addr_high[2], output_addr_low[2], time_step + 1, output_high[2], output_low[2]);
    link if is_used ~>
        mem.mstore(output_addr_high[3], output_addr_low[3], time_step + 1, output_high[3], output_low[3]);
    link if is_used ~>
        mem.mstore(output_addr_high[4], output_addr_low[4], time_step + 1, output_high[4], output_low[4]);
    link if is_used ~>
        mem.mstore(output_addr_high[5], output_addr_low[5], time_step + 1, output_high[5], output_low[5]);
    link if is_used ~>
        mem.mstore(output_addr_high[6], output_addr_low[6], time_step + 1, output_high[6], output_low[6]);
    link if is_used ~>
        mem.mstore(output_addr_high[7], output_addr_low[7], time_step + 1, output_high[7], output_low[7]);
    link if is_used ~>
        mem.mstore(output_addr_high[8], output_addr_low[8], time_step + 1, output_high[8], output_low[8]);
    link if is_used ~>
        mem.mstore(output_addr_high[9], output_addr_low[9], time_step + 1, output_high[9], output_low[9]);
    link if is_used ~>
        mem.mstore(output_addr_high[10], output_addr_low[10], time_step + 1, output_high[10], output_low[10]);
    link if is_used ~>
        mem.mstore(output_addr_high[11], output_addr_low[11], time_step + 1, output_high[11], output_low[11]);
    link if is_used ~>
        mem.mstore(output_addr_high[12], output_addr_low[12], time_step + 1, output_high[12], output_low[12]);
    link if is_used ~>
        mem.mstore(output_addr_high[13], output_addr_low[13], time_step + 1, output_high[13], output_low[13]);
    link if is_used ~>
        mem.mstore(output_addr_high[14], output_addr_low[14], time_step + 1, output_high[14], output_low[14]);
    link if is_used ~>
        mem.mstore(output_addr_high[15], output_addr_low[15], time_step + 1, output_high[15], output_low[15]);
}
