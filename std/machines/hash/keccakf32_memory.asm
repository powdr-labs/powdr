use std::array;
use std::utils;
use std::utils::unchanged_until;
use std::utils::force_bool;
use std::convert::expr;
use std::convert::int;
use std::convert::fe;
use std::prelude::set_hint;
use std::prelude::Query;
use std::prover::eval;
use std::prover::compute_from_multi;
use std::machines::large_field::memory::Memory;

machine Keccakf32Memory(mem: Memory) with
    latch: final_step,
    call_selectors: sel,
{
    /*
    ------------- Begin memory read / write ---------------
    Additional columns compared to the non-memory version:
    - 1 column for user input address (of first byte of input).
    - 1 column for user output address (of first byte of output).
    - 1 column for time step.
    Overall, given that there are 2,600+ columns in the non-memory version, this isn't a huge cost
    Methodology description:
    1. The latch with the input and output addresses + time step is in the last row of each block.
    2. User input address is copied to the first row.
    3. Input addresses for all bytes are calculated from user input address in the first row.
    4. Load all input bytes from memory to the preimage columns.
    5. Keccak is computed from top to bottom.
    6. Output addresses for all bytes are calculated from user output address in the last row.
    7. Store all output bytes from keccak computation columns to memory.
    Essentially, we conduct all memory reads in the first row and all memory writes in the last row.
    Our current methodology performs all memory reads at once in the first row, then immediately does the keccak computation,
    and finally performs all memory writes at once in the last row, and thus only requires one pass with auto witgen.
    Though note that input address need to be first copied from the last row to the first row.
    */

    operation keccakf32_memory input_addr, output_addr, time_step ->;

    // Get an intermediate column that indicates that we're in an
    // actual block, not a default block. Its value is constant
    // within the block.
    let used = array::sum(sel);
    array::map(sel, |s| unchanged_until(s, final_step + is_last));
    std::utils::force_bool(used);
    let first_step_used: expr = used * first_step;
    let final_step_used: expr = used * final_step;

    // Repeat the time step and input address in the whole block.
    col witness time_step;
    unchanged_until(time_step, final_step + is_last);

    // Input address for the first byte of input array from the user.
    // Copied from user input in the last row to the first row.
    col witness input_addr;
    unchanged_until(input_addr, final_step + is_last);

    // Output address for the first byte of output array from the user.
    // Used in the last row directly from user input.
    col witness output_addr;

    // Load memory
    // Specifically, this keccakf32 machine accepts little endian inputs in memory.
    link if first_step_used ~> preimage[0] = mem.mload(input_addr, time_step);
    link if first_step_used ~> preimage[1] = mem.mload(input_addr + 4, time_step);
    link if first_step_used ~> preimage[2] = mem.mload(input_addr + 8, time_step);
    link if first_step_used ~> preimage[3] = mem.mload(input_addr + 12, time_step);
    link if first_step_used ~> preimage[4] = mem.mload(input_addr + 16, time_step);
    link if first_step_used ~> preimage[5] = mem.mload(input_addr + 20, time_step);
    link if first_step_used ~> preimage[6] = mem.mload(input_addr + 24, time_step);
    link if first_step_used ~> preimage[7] = mem.mload(input_addr + 28, time_step);
    link if first_step_used ~> preimage[8] = mem.mload(input_addr + 32, time_step);
    link if first_step_used ~> preimage[9] = mem.mload(input_addr + 36, time_step);
    link if first_step_used ~> preimage[10] = mem.mload(input_addr + 40, time_step);
    link if first_step_used ~> preimage[11] = mem.mload(input_addr + 44, time_step);
    link if first_step_used ~> preimage[12] = mem.mload(input_addr + 48, time_step);
    link if first_step_used ~> preimage[13] = mem.mload(input_addr + 52, time_step);
    link if first_step_used ~> preimage[14] = mem.mload(input_addr + 56, time_step);
    link if first_step_used ~> preimage[15] = mem.mload(input_addr + 60, time_step);
    link if first_step_used ~> preimage[16] = mem.mload(input_addr + 64, time_step);
    link if first_step_used ~> preimage[17] = mem.mload(input_addr + 68, time_step);
    link if first_step_used ~> preimage[18] = mem.mload(input_addr + 72, time_step);
    link if first_step_used ~> preimage[19] = mem.mload(input_addr + 76, time_step);
    link if first_step_used ~> preimage[20] = mem.mload(input_addr + 80, time_step);
    link if first_step_used ~> preimage[21] = mem.mload(input_addr + 84, time_step);
    link if first_step_used ~> preimage[22] = mem.mload(input_addr + 88, time_step);
    link if first_step_used ~> preimage[23] = mem.mload(input_addr + 92, time_step);
    link if first_step_used ~> preimage[24] = mem.mload(input_addr + 96, time_step);
    link if first_step_used ~> preimage[25] = mem.mload(input_addr + 100, time_step);
    link if first_step_used ~> preimage[26] = mem.mload(input_addr + 104, time_step);
    link if first_step_used ~> preimage[27] = mem.mload(input_addr + 108, time_step);
    link if first_step_used ~> preimage[28] = mem.mload(input_addr + 112, time_step);
    link if first_step_used ~> preimage[29] = mem.mload(input_addr + 116, time_step);
    link if first_step_used ~> preimage[30] = mem.mload(input_addr + 120, time_step);
    link if first_step_used ~> preimage[31] = mem.mload(input_addr + 124, time_step);
    link if first_step_used ~> preimage[32] = mem.mload(input_addr + 128, time_step);
    link if first_step_used ~> preimage[33] = mem.mload(input_addr + 132, time_step);
    link if first_step_used ~> preimage[34] = mem.mload(input_addr + 136, time_step);
    link if first_step_used ~> preimage[35] = mem.mload(input_addr + 140, time_step);
    link if first_step_used ~> preimage[36] = mem.mload(input_addr + 144, time_step);
    link if first_step_used ~> preimage[37] = mem.mload(input_addr + 148, time_step);
    link if first_step_used ~> preimage[38] = mem.mload(input_addr + 152, time_step);
    link if first_step_used ~> preimage[39] = mem.mload(input_addr + 156, time_step);
    link if first_step_used ~> preimage[40] = mem.mload(input_addr + 160, time_step);
    link if first_step_used ~> preimage[41] = mem.mload(input_addr + 164, time_step);
    link if first_step_used ~> preimage[42] = mem.mload(input_addr + 168, time_step);
    link if first_step_used ~> preimage[43] = mem.mload(input_addr + 172, time_step);
    link if first_step_used ~> preimage[44] = mem.mload(input_addr + 176, time_step);
    link if first_step_used ~> preimage[45] = mem.mload(input_addr + 180, time_step);
    link if first_step_used ~> preimage[46] = mem.mload(input_addr + 184, time_step);
    link if first_step_used ~> preimage[47] = mem.mload(input_addr + 188, time_step);
    link if first_step_used ~> preimage[48] = mem.mload(input_addr + 192, time_step);
    link if first_step_used ~> preimage[49] = mem.mload(input_addr + 196, time_step);

    // Expects input of 25 64-bit numbers decomposed to 25 chunks of 2 32-bit little endian limbs. 
    // The output is a_prime_prime_prime_0_0_limbs for the first 2 and a_prime_prime for the rest.

    // Write memory. This machine produces little endian outputs in memory.
    link if final_step_used ~> mem.mstore(output_addr, time_step + 1, a_prime_prime_prime_0_0_limbs[0]);
    link if final_step_used ~> mem.mstore(output_addr + 4, time_step + 1, a_prime_prime_prime_0_0_limbs[1]);
    link if final_step_used ~> mem.mstore(output_addr + 8, time_step + 1, a_prime_prime[2]);
    link if final_step_used ~> mem.mstore(output_addr + 12, time_step + 1, a_prime_prime[3]);
    link if final_step_used ~> mem.mstore(output_addr + 16, time_step + 1, a_prime_prime[4]);
    link if final_step_used ~> mem.mstore(output_addr + 20, time_step + 1, a_prime_prime[5]);
    link if final_step_used ~> mem.mstore(output_addr + 24, time_step + 1, a_prime_prime[6]);
    link if final_step_used ~> mem.mstore(output_addr + 28, time_step + 1, a_prime_prime[7]);
    link if final_step_used ~> mem.mstore(output_addr + 32, time_step + 1, a_prime_prime[8]);
    link if final_step_used ~> mem.mstore(output_addr + 36, time_step + 1, a_prime_prime[9]);
    link if final_step_used ~> mem.mstore(output_addr + 40, time_step + 1, a_prime_prime[10]);
    link if final_step_used ~> mem.mstore(output_addr + 44, time_step + 1, a_prime_prime[11]);
    link if final_step_used ~> mem.mstore(output_addr + 48, time_step + 1, a_prime_prime[12]);
    link if final_step_used ~> mem.mstore(output_addr + 52, time_step + 1, a_prime_prime[13]);
    link if final_step_used ~> mem.mstore(output_addr + 56, time_step + 1, a_prime_prime[14]);
    link if final_step_used ~> mem.mstore(output_addr + 60, time_step + 1, a_prime_prime[15]);
    link if final_step_used ~> mem.mstore(output_addr + 64, time_step + 1, a_prime_prime[16]);
    link if final_step_used ~> mem.mstore(output_addr + 68, time_step + 1, a_prime_prime[17]);
    link if final_step_used ~> mem.mstore(output_addr + 72, time_step + 1, a_prime_prime[18]);
    link if final_step_used ~> mem.mstore(output_addr + 76, time_step + 1, a_prime_prime[19]);
    link if final_step_used ~> mem.mstore(output_addr + 80, time_step + 1, a_prime_prime[20]);
    link if final_step_used ~> mem.mstore(output_addr + 84, time_step + 1, a_prime_prime[21]);
    link if final_step_used ~> mem.mstore(output_addr + 88, time_step + 1, a_prime_prime[22]);
    link if final_step_used ~> mem.mstore(output_addr + 92, time_step + 1, a_prime_prime[23]);
    link if final_step_used ~> mem.mstore(output_addr + 96, time_step + 1, a_prime_prime[24]);
    link if final_step_used ~> mem.mstore(output_addr + 100, time_step + 1, a_prime_prime[25]);
    link if final_step_used ~> mem.mstore(output_addr + 104, time_step + 1, a_prime_prime[26]);
    link if final_step_used ~> mem.mstore(output_addr + 108, time_step + 1, a_prime_prime[27]);
    link if final_step_used ~> mem.mstore(output_addr + 112, time_step + 1, a_prime_prime[28]);
    link if final_step_used ~> mem.mstore(output_addr + 116, time_step + 1, a_prime_prime[29]);
    link if final_step_used ~> mem.mstore(output_addr + 120, time_step + 1, a_prime_prime[30]);
    link if final_step_used ~> mem.mstore(output_addr + 124, time_step + 1, a_prime_prime[31]);
    link if final_step_used ~> mem.mstore(output_addr + 128, time_step + 1, a_prime_prime[32]);
    link if final_step_used ~> mem.mstore(output_addr + 132, time_step + 1, a_prime_prime[33]);
    link if final_step_used ~> mem.mstore(output_addr + 136, time_step + 1, a_prime_prime[34]);
    link if final_step_used ~> mem.mstore(output_addr + 140, time_step + 1, a_prime_prime[35]);
    link if final_step_used ~> mem.mstore(output_addr + 144, time_step + 1, a_prime_prime[36]);
    link if final_step_used ~> mem.mstore(output_addr + 148, time_step + 1, a_prime_prime[37]);
    link if final_step_used ~> mem.mstore(output_addr + 152, time_step + 1, a_prime_prime[38]);
    link if final_step_used ~> mem.mstore(output_addr + 156, time_step + 1, a_prime_prime[39]);
    link if final_step_used ~> mem.mstore(output_addr + 160, time_step + 1, a_prime_prime[40]);
    link if final_step_used ~> mem.mstore(output_addr + 164, time_step + 1, a_prime_prime[41]);
    link if final_step_used ~> mem.mstore(output_addr + 168, time_step + 1, a_prime_prime[42]);
    link if final_step_used ~> mem.mstore(output_addr + 172, time_step + 1, a_prime_prime[43]);
    link if final_step_used ~> mem.mstore(output_addr + 176, time_step + 1, a_prime_prime[44]);
    link if final_step_used ~> mem.mstore(output_addr + 180, time_step + 1, a_prime_prime[45]);
    link if final_step_used ~> mem.mstore(output_addr + 184, time_step + 1, a_prime_prime[46]);
    link if final_step_used ~> mem.mstore(output_addr + 188, time_step + 1, a_prime_prime[47]);
    link if final_step_used ~> mem.mstore(output_addr + 192, time_step + 1, a_prime_prime[48]);
    link if final_step_used ~> mem.mstore(output_addr + 196, time_step + 1, a_prime_prime[49]);
    // ------------- End memory read / write ---------------

    // Adapted from Plonky3 implementation of Keccak: https://github.com/Plonky3/Plonky3/tree/main/keccak-air/src

    std::check::require_field_bits(32, || "The field modulus should be at least 2^32 - 1 to work in the keccakf32 machine.");

    let NUM_ROUNDS: int = 24;

    // pub struct KeccakCols<T> {
    //     /// The `i`th value is set to 1 if we are in the `i`th round, otherwise 0.
    //     pub step_flags: [T; NUM_ROUNDS],

    //     /// A register which indicates if a row should be exported, i.e. included in a multiset equality
    //     /// argument. Should be 1 only for certain rows which are final steps, i.e. with
    //     /// `step_flags[23] = 1`.
    //     pub export: T,

    //     /// Permutation inputs, stored in y-major order.
    //     pub preimage: [[[T; U64_LIMBS]; 5]; 5],

    //     pub a: [[[T; U64_LIMBS]; 5]; 5],

    //     /// ```ignore
    //     /// C[x] = xor(A[x, 0], A[x, 1], A[x, 2], A[x, 3], A[x, 4])
    //     /// ```
    //     pub c: [[T; 64]; 5],

    //     /// ```ignore
    //     /// C'[x, z] = xor(C[x, z], C[x - 1, z], C[x + 1, z - 1])
    //     /// ```
    //     pub c_prime: [[T; 64]; 5],

    //     // Note: D is inlined, not stored in the witness.
    //     /// ```ignore
    //     /// A'[x, y] = xor(A[x, y], D[x])
    //     ///          = xor(A[x, y], C[x - 1], ROT(C[x + 1], 1))
    //     /// ```
    //     pub a_prime: [[[T; 64]; 5]; 5],

    //     /// ```ignore
    //     /// A''[x, y] = xor(B[x, y], andn(B[x + 1, y], B[x + 2, y])).
    //     /// ```
    //     pub a_prime_prime: [[[T; U64_LIMBS]; 5]; 5],

    //     /// The bits of `A''[0, 0]`.
    //     pub a_prime_prime_0_0_bits: [T; 64],

    //     /// ```ignore
    //     /// A'''[0, 0, z] = A''[0, 0, z] ^ RC[k, z]
    //     /// ```
    //     pub a_prime_prime_prime_0_0_limbs: [T; U64_LIMBS],
    // }

    col witness preimage[5 * 5 * 2];
    col witness a[5 * 5 * 2];
    col witness c[5 * 64];
    array::map(c, |i| force_bool(i));
    col witness c_prime[5 * 64];
    col witness a_prime[5 * 5 * 64];
    array::map(a_prime, |i| force_bool(i));
    col witness a_prime_prime[5 * 5 * 2];
    col witness a_prime_prime_0_0_bits[64];
    array::map(a_prime_prime_0_0_bits, |i| force_bool(i));
    col witness a_prime_prime_prime_0_0_limbs[2];

    // Initially, the first step flag should be 1 while the others should be 0.
    // builder.when_first_row().assert_one(local.step_flags[0]);
    // for i in 1..NUM_ROUNDS {
    //     builder.when_first_row().assert_zero(local.step_flags[i]);
    // }
    // for i in 0..NUM_ROUNDS {
    //     let current_round_flag = local.step_flags[i];
    //     let next_round_flag = next.step_flags[(i + 1) % NUM_ROUNDS];
    //     builder
    //         .when_transition()
    //         .assert_eq(next_round_flag, current_round_flag);
    // }

    let step_flags: col[NUM_ROUNDS] = array::new(NUM_ROUNDS, |i| |row| if row % NUM_ROUNDS == i { 1 } else { 0 } );

    // let main = builder.main();
    // let (local, next) = (main.row_slice(0), main.row_slice(1));
    // let local: &KeccakCols<AB::Var> = (*local).borrow();
    // let next: &KeccakCols<AB::Var> = (*next).borrow();

    // let first_step = local.step_flags[0];
    // let final_step = local.step_flags[NUM_ROUNDS - 1];
    // let not_final_step = AB::Expr::one() - final_step;

    let first_step: expr = step_flags[0]; // Aliasing instead of defining a new fixed column.
    let final_step: expr = step_flags[NUM_ROUNDS - 1];
    col fixed is_last = [0]* + [1];

    // // If this is the first step, the input A must match the preimage.
    // for y in 0..5 {
    //     for x in 0..5 {
    //         for limb in 0..U64_LIMBS {
    //             builder
    //                 .when(first_step)
    //                 .assert_eq(local.preimage[y][x][limb], local.a[y][x][limb]);
    //         }
    //     }
    // }

    array::zip(preimage, a, |p_i, a_i| first_step * (p_i - a_i) = 0);

    // // The export flag must be 0 or 1.
    // builder.assert_bool(local.export);

    // force_bool(export);

    // // If this is not the final step, the export flag must be off.
    // builder
    //     .when(not_final_step.clone())
    //     .assert_zero(local.export);

    // not_final_step * export = 0;

    // // If this is not the final step, the local and next preimages must match.
    // for y in 0..5 {
    //     for x in 0..5 {
    //         for limb in 0..U64_LIMBS {
    //             builder
    //                 .when(not_final_step.clone())
    //                 .when_transition()
    //                 .assert_eq(local.preimage[y][x][limb], next.preimage[y][x][limb]);
    //         }
    //     }
    // }

    array::map(preimage, |p| unchanged_until(p, final_step + is_last));

    // for x in 0..5 {
    //     for z in 0..64 {
    //         builder.assert_bool(local.c[x][z]);
    //         let xor = xor3_gen::<AB::Expr>(
    //             local.c[x][z].into(),
    //             local.c[(x + 4) % 5][z].into(),
    //             local.c[(x + 1) % 5][(z + 63) % 64].into(),
    //         );
    //         let c_prime = local.c_prime[x][z];
    //         builder.assert_eq(c_prime, xor);
    //     }
    // }
    
    let andn: expr, expr -> expr = |a, b| (1 - a) * b;
    let xor: expr, expr -> expr = |a, b| a + b - 2*a*b;
    let xor3: expr, expr, expr -> expr = |a, b, c| xor(xor(a, b), c);
    // a b c xor3
    // 0 0 0  0
    // 0 0 1  1
    // 0 1 0  1
    // 0 1 1  0
    // 1 0 0  1
    // 1 0 1  0
    // 1 1 0  0
    // 1 1 1  1

    array::new(320, |i| {
        let x = i / 64;
        let z = i % 64;
        c_prime[i] = xor3(
            c[i], 
            c[((x + 4) % 5) * 64 + z], 
            c[((x + 1) % 5) * 64 + ((z + 63) % 64)]
        )
    });

    // // Check that the input limbs are consistent with A' and D.
    // // A[x, y, z] = xor(A'[x, y, z], D[x, y, z])
    // //            = xor(A'[x, y, z], C[x - 1, z], C[x + 1, z - 1])
    // //            = xor(A'[x, y, z], C[x, z], C'[x, z]).
    // // The last step is valid based on the identity we checked above.
    // // It isn't required, but makes this check a bit cleaner.
    // for y in 0..5 {
    //     for x in 0..5 {
    //         let get_bit = |z| {
    //             let a_prime: AB::Var = local.a_prime[y][x][z];
    //             let c: AB::Var = local.c[x][z];
    //             let c_prime: AB::Var = local.c_prime[x][z];
    //             xor3_gen::<AB::Expr>(a_prime.into(), c.into(), c_prime.into())
    //         };

    //         for limb in 0..U64_LIMBS {
    //             let a_limb = local.a[y][x][limb];
    //             let computed_limb = (limb * BITS_PER_LIMB..(limb + 1) * BITS_PER_LIMB) // bigger address correspond to more significant bit
    //                 .rev()
    //                 .fold(AB::Expr::zero(), |acc, z| {
    //                     builder.assert_bool(local.a_prime[y][x][z]);
    //                     acc.double() + get_bit(z)
    //                 });
    //             builder.assert_eq(computed_limb, a_limb);
    //         }
    //     }
    // }

    let bits_to_value_be: expr[] -> expr = |bits_be| array::fold(bits_be, 0, |acc, e| (acc * 2 + e));

    array::new(50, |i| {
        let y = i / 10;
        let x = (i / 2) % 5;
        let limb = i % 2;
        let get_bit: int -> expr = |z| xor3(a_prime[y * 320 + x * 64 + z], c[x * 64 + z], c_prime[x * 64 + z]);

        let limb_bits_be: expr[] = array::reverse(array::new(32, |z| get_bit(limb * 32 + z)));
        a[i] = bits_to_value_be(limb_bits_be)
    });

    // // xor_{i=0}^4 A'[x, i, z] = C'[x, z], so for each x, z,
    // // diff * (diff - 2) * (diff - 4) = 0, where
    // // diff = sum_{i=0}^4 A'[x, i, z] - C'[x, z]
    // for x in 0..5 {
    //     for z in 0..64 {
    //         let sum: AB::Expr = (0..5).map(|y| local.a_prime[y][x][z].into()).sum();
    //         let diff = sum - local.c_prime[x][z];
    //         let four = AB::Expr::from_canonical_u8(4);
    //         builder
    //             .assert_zero(diff.clone() * (diff.clone() - AB::Expr::two()) * (diff - four));
    //     }
    // }

    array::new(320, |i| {
        let x = i / 64;
        let z = i % 64;
        let sum = utils::sum(5, |y| a_prime[y * 320 + i]);
        let diff = sum - c_prime[i];
        diff * (diff - 2) * (diff - 4) = 0
    });

    // // A''[x, y] = xor(B[x, y], andn(B[x + 1, y], B[x + 2, y])).
    // for y in 0..5 {
    //     for x in 0..5 {
    //         let get_bit = |z| {
    //             let andn = andn_gen::<AB::Expr>(
    //                 local.b((x + 1) % 5, y, z).into(),
    //                 local.b((x + 2) % 5, y, z).into(),
    //             );
    //             xor_gen::<AB::Expr>(local.b(x, y, z).into(), andn)
    //         };

    //         for limb in 0..U64_LIMBS {
    //             let computed_limb = (limb * BITS_PER_LIMB..(limb + 1) * BITS_PER_LIMB)
    //                 .rev()
    //                 .fold(AB::Expr::zero(), |acc, z| acc.double() + get_bit(z));
    //             builder.assert_eq(computed_limb, local.a_prime_prime[y][x][limb]);
    //         }
    //     }
    // }

    array::new(50, |i| {
        let y = i / 10;
        let x = (i / 2) % 5;
        let limb = i % 2;

        let get_bit: int -> expr = |z| {
            xor(b(x, y, z), andn(b((x + 1) % 5, y, z), b((x + 2) % 5, y, z)))
        };
        let limb_bits_be: expr[] = array::reverse(array::new(32, |z| get_bit(limb * 32 + z)));
        a_prime_prime[i] = bits_to_value_be(limb_bits_be)
    });

    // pub fn b(&self, x: usize, y: usize, z: usize) -> T {
    //     debug_assert!(x < 5);
    //     debug_assert!(y < 5);
    //     debug_assert!(z < 64);

    //     // B is just a rotation of A', so these are aliases for A' registers.
    //     // From the spec,
    //     //     B[y, (2x + 3y) % 5] = ROT(A'[x, y], r[x, y])
    //     // So,
    //     //     B[x, y] = f((x + 3y) % 5, x)
    //     // where f(a, b) = ROT(A'[a, b], r[a, b])
    //     let a = (x + 3 * y) % 5;
    //     let b = x;
    //     let rot = R[a][b] as usize;
    //     self.a_prime[b][a][(z + 64 - rot) % 64]
    // }

    let b: int, int, int -> expr = |x, y, z| {
        let a: int = (x + 3 * y) % 5;
        let rot: int = R[a * 5 + x]; // b = x
        a_prime[x * 320 + a * 64 + (z + 64 - rot) % 64]
    };

    // // A'''[0, 0] = A''[0, 0] XOR RC
    // for limb in 0..U64_LIMBS {
    //     let computed_a_prime_prime_0_0_limb = (limb * BITS_PER_LIMB
    //         ..(limb + 1) * BITS_PER_LIMB)
    //         .rev()
    //         .fold(AB::Expr::zero(), |acc, z| {
    //             builder.assert_bool(local.a_prime_prime_0_0_bits[z]);
    //             acc.double() + local.a_prime_prime_0_0_bits[z]
    //         });
    //     let a_prime_prime_0_0_limb = local.a_prime_prime[0][0][limb];
    //     builder.assert_eq(computed_a_prime_prime_0_0_limb, a_prime_prime_0_0_limb);
    // }

    array::new(2, |limb| {
        let limb_bits_be: expr[] = array::reverse(array::new(32, |z| a_prime_prime_0_0_bits[limb * 32 + z]));
        a_prime_prime[limb] = bits_to_value_be(limb_bits_be)
    });

    // let get_xored_bit = |i| {
    //     let mut rc_bit_i = AB::Expr::zero();
    //     for r in 0..NUM_ROUNDS {
    //         let this_round = local.step_flags[r];
    //         let this_round_constant = AB::Expr::from_canonical_u8(rc_value_bit(r, i));
    //         rc_bit_i += this_round * this_round_constant;
    //     }

    //     xor_gen::<AB::Expr>(local.a_prime_prime_0_0_bits[i].into(), rc_bit_i)
    // };

    let get_xored_bit: int -> expr = |i| xor(a_prime_prime_0_0_bits[i], utils::sum(NUM_ROUNDS, |r| expr(RC_BITS[r * 64 + i]) * step_flags[r] ));

    // for limb in 0..U64_LIMBS {
    //     let a_prime_prime_prime_0_0_limb = local.a_prime_prime_prime_0_0_limbs[limb];
    //     let computed_a_prime_prime_prime_0_0_limb = (limb * BITS_PER_LIMB
    //         ..(limb + 1) * BITS_PER_LIMB)
    //         .rev()
    //         .fold(AB::Expr::zero(), |acc, z| acc.double() + get_xored_bit(z));
    //     builder.assert_eq(
    //         computed_a_prime_prime_prime_0_0_limb,
    //         a_prime_prime_prime_0_0_limb,
    //     );
    // }

    array::new(2, |limb| {
        let limb_bits_be: expr[] = array::reverse(array::new(32, |z| get_xored_bit(limb * 32 + z)));
        a_prime_prime_prime_0_0_limbs[limb] = bits_to_value_be(limb_bits_be)
    });

    // // Enforce that this round's output equals the next round's input.
    // for x in 0..5 {
    //     for y in 0..5 {
    //         for limb in 0..U64_LIMBS {
    //             let output = local.a_prime_prime_prime(y, x, limb);
    //             let input = next.a[y][x][limb];
    //             builder
    //                 .when_transition()
    //                 .when(not_final_step.clone())
    //                 .assert_eq(output, input);
    //         }
    //     }
    // }

    // final_step and is_last should never be 1 at the same time, because final_step is 1 at multiples of 24 and can never be 1 at power of 2.
    // (1 - final_step - is_last) is used to deactivate constraints that reference the next row, whenever we are at the latch row or the last row of the trace (so that we don't incorrectly cycle to the first row).
    array::new(50, |i| {
        let y = i / 10;
        let x = (i / 2) % 5;
        let limb = i % 2;
        (1 - final_step - is_last) * (a_prime_prime_prime(y, x, limb) - a[i]') = 0
    });

    // pub fn a_prime_prime_prime(&self, y: usize, x: usize, limb: usize) -> T {
    //     debug_assert!(y < 5);
    //     debug_assert!(x < 5);
    //     debug_assert!(limb < U64_LIMBS);

    //     if y == 0 && x == 0 {
    //         self.a_prime_prime_prime_0_0_limbs[limb]
    //     } else {
    //         self.a_prime_prime[y][x][limb]
    //     }
    // }

    let a_prime_prime_prime: int, int, int -> expr = |y, x, limb| if y == 0 && x == 0 { a_prime_prime_prime_0_0_limbs[limb] } else { a_prime_prime[y * 10 + x * 2 + limb] };

    let R: int[] = [
        0, 36, 3, 41, 18, 
        1, 44, 10, 45, 2,
        62, 6, 43, 15, 61,
        28, 55, 25, 21, 56,
        27, 20, 39, 8, 14
    ];

    let RC: int[] = [
        0x0000000000000001,
        0x0000000000008082,
        0x800000000000808A,
        0x8000000080008000,
        0x000000000000808B,
        0x0000000080000001,
        0x8000000080008081,
        0x8000000000008009,
        0x000000000000008A,
        0x0000000000000088,
        0x0000000080008009,
        0x000000008000000A,
        0x000000008000808B,
        0x800000000000008B,
        0x8000000000008089,
        0x8000000000008003,
        0x8000000000008002,
        0x8000000000000080,
        0x000000000000800A,
        0x800000008000000A,
        0x8000000080008081,
        0x8000000000008080,
        0x0000000080000001,
        0x8000000080008008
    ];

    let RC_BITS: int[] = array::new(24 * 64, |i| {
        let rc_idx = i / 64;
        let bit = i % 64;
        RC[rc_idx] >> bit & 0x1
    });

    // Prover function section (for witness generation).
    // Hints are only needed for c and a_prime, the solver is able to figure out the
    // rest of the witness.

    // // Populate C[x] = xor(A[x, 0], A[x, 1], A[x, 2], A[x, 3], A[x, 4]).
    // for x in 0..5 {
    //     for z in 0..64 {
    //         let limb = z / BITS_PER_LIMB;
    //         let bit_in_limb = z % BITS_PER_LIMB;
    //         let a = (0..5).map(|y| {
    //             let a_limb = row.a[y][x][limb].as_canonical_u64() as u16;
    //             ((a_limb >> bit_in_limb) & 1) != 0
    //         });
    //         row.c[x][z] = F::from_bool(a.fold(false, |acc, x| acc ^ x));
    //     }
    // }

    query |row| compute_from_multi(
        c, row, a,
        |a_fe| array::new(array::len(c), |i| {
            let x = i / 64;
            let z = i % 64;
            let limb = z / 32;
            let bit_in_limb = z % 32;

            fe(utils::fold(
                5,
                |y| (int(a_fe[y * 10 + x * 2 + limb]) >> bit_in_limb) & 0x1,
                0,
                |acc, e| acc ^ e
            ))
        }));

    // // Populate A'. To avoid shifting indices, we rewrite
    // //     A'[x, y, z] = xor(A[x, y, z], C[x - 1, z], C[x + 1, z - 1])
    // // as
    // //     A'[x, y, z] = xor(A[x, y, z], C[x, z], C'[x, z]).
    // for x in 0..5 {
    //     for y in 0..5 {
    //         for z in 0..64 {
    //             let limb = z / BITS_PER_LIMB;
    //             let bit_in_limb = z % BITS_PER_LIMB;
    //             let a_limb = row.a[y][x][limb].as_canonical_u64() as u16;
    //             let a_bit = F::from_bool(((a_limb >> bit_in_limb) & 1) != 0);
    //             row.a_prime[y][x][z] = xor([a_bit, row.c[x][z], row.c_prime[x][z]]);
    //         }
    //     }
    // }

    query |row| compute_from_multi(
        a_prime, row, a + c + c_prime,
        |inputs| array::new(array::len(a_prime), |i| {
            let y = i / 320;
            let x = (i / 64) % 5;
            let z = i % 64;
            let limb = z / 32;
            let bit_in_limb = z % 32;

            let a_elem = inputs[y * 10 + x * 2 + limb];
            let c_elem = inputs[x * 64 + z + 5 * 5 * 2];
            let c_prime_elem = inputs[x * 64 + z + 5 * 5 * 2 + 5 * 64];

            fe(((int(a_elem) >> bit_in_limb) & 0x1) ^ int(c_elem) ^ int(c_prime_elem))
        }));

    // TODO: This hint is correct but not needed (the solver can figure this out).
    // We keep it here because it prevents the JIT solver from succeeding (because of the
    // use of `provide_value`), because it currently fails when compiling Rust code.
    // Once these issues are resolved, we can remove this hint.
    query |row| {
        std::prover::provide_value(
            a_prime_prime_0_0_bits[0], 
            row, 
            fe((int(eval(a_prime_prime[0]))) & 0x1)
        );
    };

}
