use std::array;
use std::check::assert;
use std::utils::unchanged_until;
use std::utils::force_bool;
use std::utils::sum;
use std::convert::expr;
use std::machines::small_field::memory::Memory;
use std::machines::small_field::pointer_arith::increment_ptr;
use std::machines::split::split_bb::SplitBB;

// Implements the Poseidon permutation for the BabyBear field with the following parameters:
// - 16 field elements in the state
// - 8 field elements of output
// - 8 full rounds
// - 16 partial rounds
// - S-Box: x^7
//
// The number of rounds to get 128-bit security was taken from here:
// https://github.com/Plonky3/Plonky3/blob/2df15fd05e2181b31b39525361aef0213fc76144/poseidon2/src/round_numbers.rs#L42
//
// This version of the Poseidon machine receives memory pointers and interacts
// with memory directly to fetch its inputs and write its outputs.
machine PoseidonBB(mem: Memory, split_bb: SplitBB) with
    latch: CLK_0,
    // Allow this machine to be connected via a permutation
    call_selectors: sel,
{

    // Compresses 16 to 8 field elements by applying the Poseidon permutation and
    // returning the first 8 elements of the state vector.
    //
    // The input data is passed via a memory pointer: the machine will read 16 field
    // elements from memory (32 16-bits half-words).
    //
    // Similarly, the output data is written to memory at the provided pointer as
    // 16 16-bit machine half-words representing 8 field elements in little-endian format
    // (in canonical form).
    //
    // Reads happen at the provided time step; writes happen at the next time step.
    operation poseidon_permutation
        input_addr_high, input_addr_low,
        output_addr_high, output_addr_low,
        time_step ->;

    // Number of field elements in the state
    let STATE_SIZE: int = 16;
    // Number of output elements
    // (TODO: to use the sponge construct, the entire state should be output)
    let OUTPUT_SIZE: int = 8;
    // Number of full rounds (half of them before and half of them after the partial rounds)
    let FULL_ROUNDS: int = 8;
    // Number of partial rounds
    // For 128-bit security, Poseidon2 requires 13 partial rounds. But we need more here
    // To have enough rows for memory read / write.
    let PARTIAL_ROUNDS: int = 16;
    let ROWS_PER_HASH = FULL_ROUNDS + PARTIAL_ROUNDS + 1;


    // ------------- Begin memory read / write ---------------

    // Get an intermediate column that indicates that we're in an
    // actual block, not a default block. Its value is constant
    // within the block.
    let used = array::sum(sel);
    array::map(sel, |s| unchanged_until(s, LAST));
    std::utils::force_bool(used);

    // Repeat the input state in the whole block
    let input: col[STATE_SIZE];
    array::map(input, |c| unchanged_until(c, LAST));
    array::zip(input, state, |i, s| CLK[0] * (i - s) = 0);

    // Repeat the time step and input / output address in the whole block
    let time_step;
    let input_addr_high;
    let input_addr_low;
    let output_addr_high;
    let output_addr_low;
    unchanged_until(time_step, LAST);
    unchanged_until(input_addr_high, LAST);
    unchanged_until(input_addr_low, LAST);
    unchanged_until(output_addr_high, LAST);
    unchanged_until(output_addr_low, LAST);

    // Increment the address in every row but the one it is set.
    let is_addr_set = CLK[0] + CLK[STATE_SIZE];
    let high_addr;
    let low_addr;

    // Set the initial address to read from
    CLK[0] * (high_addr - input_addr_high) = 0;
    CLK[0] * (low_addr - input_addr_low) = 0;

    // Set the initial address to write to
    CLK[STATE_SIZE] * (high_addr - output_addr_high) = 0;
    CLK[STATE_SIZE] * (low_addr - output_addr_low) = 0;

    // Increment the address by 4 in every row but the ones it is set.
    std::array::map(
        increment_ptr(4, high_addr, low_addr, high_addr', low_addr'),
        |c| std::constraints::make_conditional(c, (1 - is_addr_set'))
    );

    // One-hot encoding of the row number (for the first <STATE_SIZE + OUTPUT_SIZE> rows)
    assert(STATE_SIZE + OUTPUT_SIZE < ROWS_PER_HASH, || "Not enough rows to do memory read / write");
    let CLK: col[STATE_SIZE + OUTPUT_SIZE] = array::new(STATE_SIZE + OUTPUT_SIZE, |i| |row| if row % ROWS_PER_HASH == i { 1 } else { 0 });
    let CLK_0 = CLK[0];

    let word_high;
    let word_low;

    // Do a memory read in each of the first STATE_SIZE rows, getting the two 16-bit limbs
    // that makes up for the input field element. 
    // For input i, we expect the field element at address input_addr + 4 * i.
    // TODO: This could be an intermediate polynomial, but for some reason estark-starky
    // fails then, so we keep it as a witness for now
    let do_mload;
    do_mload = used * sum(STATE_SIZE, |i| CLK[i]);
    let input_index = sum(STATE_SIZE, |i| expr(i) * CLK[i]);

    link if do_mload ~> (word_high, word_low) = mem.mload(high_addr, low_addr, time_step);

    // Combine the low and high limbs and write it into `input`
    let current_input = sum(STATE_SIZE, |i| CLK[i] * input[i]);
    do_mload * (word_low + word_high * 2**16 - current_input) = 0;

    // Do a memory write in each of the next OUTPUT_SIZE rows.
    // For output i, we write the two limbs of field element at address output_addr + 4 * i.

    let do_mstore = used * sum(OUTPUT_SIZE, |i| CLK[i + STATE_SIZE]);
    let output_index = sum(OUTPUT_SIZE, |i| expr(i) * CLK[i + STATE_SIZE]);

    // TODO: This translates to two additional permutations. But because they go to the same machine
    // as the mloads above *and* never happen at the same time, they could actually be combined with
    // the mload permutations. But there is currently no way to express this.
    link if do_mstore ~> mem.mstore(high_addr, low_addr, time_step + 1, word_high, word_low);

    // Make sure that in row i + STATE_SIZE, word_low and word_high correspond to output i
    let current_output = sum(OUTPUT_SIZE, |i| CLK[i + STATE_SIZE] * output[i]);
    link if do_mstore ~> (word_low, word_high) = split_bb.split(current_output);


    // ------------- End memory read / write ---------------

    pol constant LASTBLOCK(i) { if i % ROWS_PER_HASH == ROWS_PER_HASH - 1 { 1 } else { 0 } };
    // Like LASTBLOCK, but also 1 in the last row of the table
    // Specified this way because we can't access the degree in the match statement
    pol constant LAST = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1]* + [1];

    // Whether the current round is a partial round
    pol constant PARTIAL = [0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0]*;
    
    // The round constants, generated in gen_poseidon_bb_constants.
    pol constant C_0 = [781065863, 1704334099, 1614250469, 858342508, 1331255579, 94027721, 1633402383, 1774536800, 967783090, 1429869924, 37790139, 1067472776, 1703182141, 1722007170, 826573738, 1380955441, 1173986918, 427450465, 703550610, 214947471, 810976863, 1569294983, 1294224805, 40193270, 0]*;
    pol constant C_1 = [858808123, 1982585188, 797628021, 273000383, 570536182, 1015052027, 1622799895, 1845434468, 393329457, 870203221, 56318764, 1364908618, 929735258, 410647527, 1272874215, 1250307830, 1985094168, 1183107810, 290944485, 1431023892, 1514015400, 150034509, 1932176786, 113929158, 0]*;
    pol constant C_2 = [314648554, 412945090, 1799565197, 1437543685, 210037341, 267254220, 1123299502, 1012046526, 1811748296, 1082880104, 452117508, 591556198, 26422375, 928482204, 1782339126, 471400423, 1715755484, 1620279079, 898856400, 1060851389, 1774418870, 1523201093, 9015542, 500181102, 0]*;
    pol constant C_3 = [1011868729, 1943785875, 410764106, 1856107565, 1977593067, 1362094997, 1586847440, 1751322463, 1820671903, 712390866, 1344285673, 1301479607, 1447437124, 1817620797, 796225227, 1958608680, 1934746594, 688362361, 1897565392, 242159596, 1362690728, 1540780945, 309719651, 1780905031, 0]*;
    pol constant C_4 = [1403665294, 1889289665, 1998617149, 1455767632, 497240095, 309963516, 1683981810, 1877298991, 868046153, 890940275, 283303262, 145680600, 1105472003, 1676373559, 940577289, 233213338, 369884595, 39502463, 1425277724, 951005540, 1216021342, 381524560, 1062589222, 1537626390, 0]*;
    pol constant C_5 = [347091819, 781614254, 1465862749, 611525604, 1661958720, 1585470899, 726892227, 1080833156, 1801120382, 894459206, 1625007661, 80924995, 1806184511, 1319925328, 1921831187, 167046462, 542522046, 1727482804, 37015631, 532910846, 137187551, 324474051, 95873217, 1089984426, 0]*;
    pol constant C_6 = [1599468792, 1574163835, 384459982, 549724551, 582315897, 1358895180, 2000441089, 1413250263, 1046049762, 1314718422, 1858824814, 1325641465, 1915668633, 730880289, 537405228, 1062171431, 1879294470, 1321716416, 1089899507, 1229043173, 569341304, 775041806, 618952009, 1526482468, 0]*;
    pol constant C_7 = [747187630, 1533270493, 1436636925, 1831058620, 391834452, 686629664, 1394514513, 1145369710, 169339882, 455736318, 1351361801, 1805320344, 254925967, 663478677, 1143058407, 201743468, 333292135, 436977712, 145171880, 1638797588, 869807707, 1479708171, 247945700, 636657680, 0]*;
    pol constant C_8 = [1800655809, 534849574, 763774749, 1725226986, 176019677, 1310666129, 913374432, 243131964, 812872171, 709216305, 976546120, 451648869, 1245580916, 992812111, 1359656445, 1815138554, 802353999, 1524870119, 1378751224, 696990883, 864419883, 375427563, 614468479, 79512304, 0]*;
    pol constant C_9 = [870844598, 116915927, 1133474855, 1116066513, 1926233652, 579529444, 1766547080, 15858194, 415206813, 677640980, 1536525063, 1784986021, 966527788, 1228848893, 389281904, 1981557026, 1420369881, 906667212, 1116363768, 324768492, 1792012694, 1843911887, 1628334558, 1936064854, 0]*;
    pol constant C_10 = [1647721312, 1333680848, 730089459, 1705577021, 1024884485, 1655307890, 246479249, 1238364731, 799847037, 728207769, 872552252, 1119955619, 791943038, 367506252, 607459606, 1942205388, 1411828801, 296376720, 514768068, 531696616, 25737860, 277396396, 1963102133, 48397325, 0]*;
    pol constant C_11 = [460780171, 1775289355, 308042941, 1313995736, 15784852, 993571256, 1659156273, 1483917495, 1249965883, 7408280, 572633063, 503972473, 84554475, 1353398130, 956485577, 755538766, 978125791, 1998923402, 1375172785, 425319830, 919012147, 862263146, 530984479, 479978121, 0]*;
    pol constant C_12 = [1735007748, 1983552037, 1532863621, 94738396, 992120364, 1970564083, 1944479098, 180642514, 981543785, 673892942, 747791441, 370435416, 472286683, 1631728050, 1200210219, 657060459, 1329399143, 1481360832, 33311276, 1356620894, 285201111, 1896118057, 1178549185, 414442762, 0]*;
    pol constant C_13 = [336001371, 905646944, 1288306270, 385833350, 3792692, 1072385714, 1194261137, 226255882, 368055032, 163075143, 574924181, 1162051737, 589145361, 75983087, 89570374, 1565702224, 315099133, 1862294673, 715849472, 25972299, 1529251108, 698696249, 932100232, 445083324, 0]*;
    pol constant C_14 = [530891828, 1933977884, 1592450725, 552820301, 205373871, 1615779048, 1712966350, 229391213, 351551872, 1331607174, 898936160, 1928892857, 569661577, 1299759989, 1201375435, 275384085, 184666356, 117155355, 1408099503, 508336153, 312454082, 347215394, 124141245, 1249197578, 0]*;
    pol constant C_15 = [502875698, 94566700, 944962399, 1947668840, 320428096, 1723325098, 1947574712, 834474017, 898159590, 661091227, 579623035, 327778019, 475193480, 1623272042, 784175618, 1827385979, 488670676, 423974878, 622252982, 1852893657, 796911113, 283290113, 883353761, 968383311, 0]*;
    let C = [C_0, C_1, C_2, C_3, C_4, C_5, C_6, C_7, C_8, C_9, C_10, C_11, C_12, C_13, C_14, C_15];

    // State of the Poseidon permutation (8 rate elements and 4 capacity elements)
    let state: col[STATE_SIZE];

    // The first OUTPUT_SIZE elements of the *final* state
    // (constrained to be constant within the block and equal to parts of the state in the last row)
    let output: col[OUTPUT_SIZE];

    // Add round constants
    // TODO should these be intermediate?
    let a = array::zip(state, C, |state, C| state + C);

    // Compute S-Boxes (x^7) (using a degree bound of 3)
    let x3: col[STATE_SIZE];
    array::zip(x3, array::map(a, |a| a * a * a), |x3, expected| x3 = expected);
    let x7: col[STATE_SIZE];
    array::zip(x7, array::zip(x3, a, |x3, a| x3 * x3 * a), |x7, expected| x7 = expected);

    // Apply S-Boxes on the first element and otherwise if it is a full round.
    let b: expr[] = array::new(STATE_SIZE, |i| if i == 0 {
        x7[i]
    } else {
        PARTIAL * (a[i] - x7[i]) + x7[i]
    });

    // The MDS matrix from Plonk3 implementation
    let M = [
        [1, 1, 51, 1, 11, 17, 2, 1, 101, 63, 15, 2, 67, 22, 13, 3],
        [3, 1, 1, 51, 1, 11, 17, 2, 1, 101, 63, 15, 2, 67, 22, 13],
        [13, 3, 1, 1, 51, 1, 11, 17, 2, 1, 101, 63, 15, 2, 67, 22],
        [22, 13, 3, 1, 1, 51, 1, 11, 17, 2, 1, 101, 63, 15, 2, 67],
        [67, 22, 13, 3, 1, 1, 51, 1, 11, 17, 2, 1, 101, 63, 15, 2],
        [2, 67, 22, 13, 3, 1, 1, 51, 1, 11, 17, 2, 1, 101, 63, 15],
        [15, 2, 67, 22, 13, 3, 1, 1, 51, 1, 11, 17, 2, 1, 101, 63],
        [63, 15, 2, 67, 22, 13, 3, 1, 1, 51, 1, 11, 17, 2, 1, 101],
        [101, 63, 15, 2, 67, 22, 13, 3, 1, 1, 51, 1, 11, 17, 2, 1],
        [1, 101, 63, 15, 2, 67, 22, 13, 3, 1, 1, 51, 1, 11, 17, 2],
        [2, 1, 101, 63, 15, 2, 67, 22, 13, 3, 1, 1, 51, 1, 11, 17],
        [17, 2, 1, 101, 63, 15, 2, 67, 22, 13, 3, 1, 1, 51, 1, 11],
        [11, 17, 2, 1, 101, 63, 15, 2, 67, 22, 13, 3, 1, 1, 51, 1],
        [1, 11, 17, 2, 1, 101, 63, 15, 2, 67, 22, 13, 3, 1, 1, 51],
        [51, 1, 11, 17, 2, 1, 101, 63, 15, 2, 67, 22, 13, 3, 1, 1],
        [1, 51, 1, 11, 17, 2, 1, 101, 63, 15, 2, 67, 22, 13, 3, 1]
    ];

    // Multiply with MDS Matrix
    let dot_product = |v1, v2| array::sum(array::zip(v1, v2, |v1_i, v2_i| v1_i * v2_i));
    let c = array::map(M, |M_row_i| dot_product(M_row_i, b));

    // Copy c to state in the next row
    array::zip(state, c, |state, c| (state' - c) * (1-LAST) = 0);
    // '

    // In the last row, the first OUTPUT_SIZE elements of the state should equal output
    let output_state = array::sub_array(state, 0, OUTPUT_SIZE);
    array::zip(output, output_state, |output, state| LASTBLOCK * (output - state) = 0);

    // The output should stay constant in the block
    array::map(output, |c| unchanged_until(c, LAST));
}
