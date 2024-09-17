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
use std::prover::provide_value;

machine Keccakf16 with
    latch: final_step,
    operation_id: operation_id,
    call_selectors: sel,
{
    // Adapted from Plonky3 implementation of Keccak: https://github.com/Plonky3/Plonky3/tree/main/keccak-air/src

    // Expects input of 25 64-bit numbers decomposed to 25 chunks of 4 16-bit little endian limbs. 
    // The output is a_prime_prime_prime_0_0_limbs for the first 4 and a_prime_prime for the rest.
    operation keccakf16<0> preimage[0], preimage[1], preimage[2], preimage[3], preimage[4], preimage[5], preimage[6], preimage[7], preimage[8], preimage[9], preimage[10], preimage[11], preimage[12], preimage[13], preimage[14], preimage[15], preimage[16], preimage[17], preimage[18], preimage[19], preimage[20], preimage[21], preimage[22], preimage[23], preimage[24], preimage[25], preimage[26], preimage[27], preimage[28], preimage[29], preimage[30], preimage[31], preimage[32], preimage[33], preimage[34], preimage[35], preimage[36], preimage[37], preimage[38], preimage[39], preimage[40], preimage[41], preimage[42], preimage[43], preimage[44], preimage[45], preimage[46], preimage[47], preimage[48], preimage[49], preimage[50], preimage[51], preimage[52], preimage[53], preimage[54], preimage[55], preimage[56], preimage[57], preimage[58], preimage[59], preimage[60], preimage[61], preimage[62], preimage[63], preimage[64], preimage[65], preimage[66], preimage[67], preimage[68], preimage[69], preimage[70], preimage[71], preimage[72], preimage[73], preimage[74], preimage[75], preimage[76], preimage[77], preimage[78], preimage[79], preimage[80], preimage[81], preimage[82], preimage[83], preimage[84], preimage[85], preimage[86], preimage[87], preimage[88], preimage[89], preimage[90], preimage[91], preimage[92], preimage[93], preimage[94], preimage[95], preimage[96], preimage[97], preimage[98], preimage[99] -> a_prime_prime_prime_0_0_limbs[0], a_prime_prime_prime_0_0_limbs[1], a_prime_prime_prime_0_0_limbs[2], a_prime_prime_prime_0_0_limbs[3], a_prime_prime[4], a_prime_prime[5], a_prime_prime[6], a_prime_prime[7], a_prime_prime[8], a_prime_prime[9], a_prime_prime[10], a_prime_prime[11], a_prime_prime[12], a_prime_prime[13], a_prime_prime[14], a_prime_prime[15], a_prime_prime[16], a_prime_prime[17], a_prime_prime[18], a_prime_prime[19], a_prime_prime[20], a_prime_prime[21], a_prime_prime[22], a_prime_prime[23], a_prime_prime[24], a_prime_prime[25], a_prime_prime[26], a_prime_prime[27], a_prime_prime[28], a_prime_prime[29], a_prime_prime[30], a_prime_prime[31], a_prime_prime[32], a_prime_prime[33], a_prime_prime[34], a_prime_prime[35], a_prime_prime[36], a_prime_prime[37], a_prime_prime[38], a_prime_prime[39], a_prime_prime[40], a_prime_prime[41], a_prime_prime[42], a_prime_prime[43], a_prime_prime[44], a_prime_prime[45], a_prime_prime[46], a_prime_prime[47], a_prime_prime[48], a_prime_prime[49], a_prime_prime[50], a_prime_prime[51], a_prime_prime[52], a_prime_prime[53], a_prime_prime[54], a_prime_prime[55], a_prime_prime[56], a_prime_prime[57], a_prime_prime[58], a_prime_prime[59], a_prime_prime[60], a_prime_prime[61], a_prime_prime[62], a_prime_prime[63], a_prime_prime[64], a_prime_prime[65], a_prime_prime[66], a_prime_prime[67], a_prime_prime[68], a_prime_prime[69], a_prime_prime[70], a_prime_prime[71], a_prime_prime[72], a_prime_prime[73], a_prime_prime[74], a_prime_prime[75], a_prime_prime[76], a_prime_prime[77], a_prime_prime[78], a_prime_prime[79], a_prime_prime[80], a_prime_prime[81], a_prime_prime[82], a_prime_prime[83], a_prime_prime[84], a_prime_prime[85], a_prime_prime[86], a_prime_prime[87], a_prime_prime[88], a_prime_prime[89], a_prime_prime[90], a_prime_prime[91], a_prime_prime[92], a_prime_prime[93], a_prime_prime[94], a_prime_prime[95], a_prime_prime[96], a_prime_prime[97], a_prime_prime[98], a_prime_prime[99];

    col witness operation_id;

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

    let c = array::new(5, |y| array::new(64, |z| {let tt; tt})); 

    pol commit preimage[5 * 5 * 4];
    pol commit a[5 * 5 * 4];
    // pol commit c[5 * 64];
    pol commit c_prime[5 * 64];
    pol commit a_prime[5 * 5 * 64];
    pol commit a_prime_prime[5 * 5 * 4];
    pol commit a_prime_prime_0_0_bits[64];
    pol commit a_prime_prime_prime_0_0_limbs[4];

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

    let first_step: expr = step_flags[0]; // aliasing instead of defining a new fixed column
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

    // array::map(c, |i| force_bool(i));
    
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
            c[x][z], 
            c[((x + 4) % 5)][z], 
            c[((x + 1) % 5)][((z + 63) % 64)]
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

    array::map(a_prime, |i| force_bool(i));
    array::new(100, |i| {
        let y = i / 20;
        let x = (i / 4) % 5;
        let limb = i % 4;
        let get_bit: int -> expr = |z| xor3(a_prime[y * 320 + x * 64 + z], c[x][z], c_prime[x * 64 + z]);
        let limb_bits_be: expr[] = array::reverse(array::new(16, |z| get_bit(limb * 16 + z)));
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

    array::new(100, |i| {
        let y = i / 20;
        let x = (i / 4) % 5;
        let limb = i % 4;

        let get_bit: int -> expr = |z| {
            xor(b(x, y, z), andn(b((x + 1) % 5, y, z), b((x + 2) % 5, y, z)))
        };
        let limb_bits_be: expr[] = array::reverse(array::new(16, |z| get_bit(limb * 16 + z)));
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

    array::map(a_prime_prime_0_0_bits, |i| force_bool(i));

    array::new(4, |limb| {
        let limb_bits_be: expr[] = array::reverse(array::new(16, |z| a_prime_prime_0_0_bits[limb * 16 + z]));
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

    array::new(4, |limb| {
        let limb_bits_be: expr[] = array::reverse(array::new(16, |z| get_xored_bit(limb * 16 + z)));
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
    array::new(100, |i| {
        let y = i / 20;
        let x = (i / 4) % 5;
        let limb = i % 4;
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

    let a_prime_prime_prime: int, int, int -> expr = |y, x, limb| if y == 0 && x == 0 { a_prime_prime_prime_0_0_limbs[limb] } else { a_prime_prime[y * 20 + x * 4 + limb] };

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

    let query_c: int, int, int -> int = query |x, limb, bit_in_limb|
        utils::fold(
            5, 
            |y| (int(eval(a[y * 20 + x * 4 + limb])) >> bit_in_limb) & 0x1, 
            0, 
            |acc, e| acc ^ e
        );

    query |row| {
        let _ = array::map_enumerated(c, |i, c_i| {
            let _ = array::map_enumerated(c_i, |j, c_ij| {
                let limb: int = j / 16;
                let bit_in_limb: int = j % 16;

                provide_value(c_ij, row, fe(query_c(i, limb, bit_in_limb)));
            });
        });
    };

    query |row| {
        let _ = array::new(5 * 64, |i| {
            let x = i / 64;
            let z = i % 64;
            let limb = z / 16;
            let bit_in_limb = z % 16;
            
            provide_value(c[x][z], row, fe(query_c(x, limb, bit_in_limb)));
        });
    };

    //query |row| {
    //    let _ = array::map_enumerated(c, |i, c_i| {
    //        let x = i / 64;
    //        let z = i % 64;
    //        let limb = z / 16;
    //        let bit_in_limb = z % 16;
//
    //        provide_value(c_i, row, fe(query_c(x, limb, bit_in_limb)));
    //    });
    //};

    // // Populate C'[x, z] = xor(C[x, z], C[x - 1, z], C[x + 1, z - 1]).
    // for x in 0..5 {
    //     for z in 0..64 {
    //         row.c_prime[x][z] = xor([
    //             row.c[x][z],
    //             row.c[(x + 4) % 5][z],
    //             row.c[(x + 1) % 5][(z + 63) % 64],
    //         ]);
    //     }
    // }

    let query_c_prime: int, int -> int = query |x, z| 
        int(eval(c[x][z])) ^ 
        int(eval(c[((x + 4) % 5)][z])) ^ 
        int(eval(c[((x + 1) % 5)][(z + 63) % 64]));

    query |row| {
        let _ = array::map_enumerated(c_prime, |i, c_i| {
            let x = i / 64;
            let z = i % 64;

            provide_value(c_i, row, fe(query_c_prime(x, z)));
        });
    };

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

    let query_a_prime: int, int, int, int, int -> int = query |x, y, z, limb, bit_in_limb| 
        ((int(eval(a[y * 20 + x * 4 + limb])) >> bit_in_limb) & 0x1) ^ 
        int(eval(c[x][z])) ^ 
        int(eval(c_prime[x * 64 + z]));

    query |row| {
        let _ = array::map_enumerated(a_prime, |i, a_i| {
            let y = i / 320;
            let x = (i / 64) % 5;
            let z = i % 64;
            let limb = z / 16;
            let bit_in_limb = z % 16;

            provide_value(a_i, row, fe(query_a_prime(x, y, z, limb, bit_in_limb)));
        });
    };

    // // Populate A''.P
    // // A''[x, y] = xor(B[x, y], andn(B[x + 1, y], B[x + 2, y])).
    // for y in 0..5 {
    //     for x in 0..5 {
    //         for limb in 0..U64_LIMBS {
    //             row.a_prime_prime[y][x][limb] = (limb * BITS_PER_LIMB..(limb + 1) * BITS_PER_LIMB)
    //                 .rev()
    //                 .fold(F::zero(), |acc, z| {
    //                     let bit = xor([
    //                         row.b(x, y, z),
    //                         andn(row.b((x + 1) % 5, y, z), row.b((x + 2) % 5, y, z)),
    //                     ]);
    //                     acc.double() + bit
    //                 });
    //         }
    //     }
    // }

    let query_a_prime_prime: int, int, int -> int = query |x, y, limb| 
        utils::fold(
            16, 
            |z| 
                int(eval(b(x, y, (limb + 1) * 16 - 1 - z))) ^ 
                int(eval(andn(b((x + 1) % 5, y, (limb + 1) * 16 - 1 - z), 
                b((x + 2) % 5, y, (limb + 1) * 16 - 1 - z)))), 
            0, 
            |acc, e| acc * 2 + e
        );

    query |row| {
        let _ = array::map_enumerated(a_prime_prime, |i, a_i| {
            let y = i / 20;
            let x = (i / 4) % 5;
            let limb = i % 4;

            provide_value(a_i, row, fe(query_a_prime_prime(x, y, limb)));
        });
    };

    // // For the XOR, we split A''[0, 0] to bits.
    // let mut val = 0; // smaller address correspond to less significant limb
    // for limb in 0..U64_LIMBS {
    //     let val_limb = row.a_prime_prime[0][0][limb].as_canonical_u64();
    //     val |= val_limb << (limb * BITS_PER_LIMB);
    // }
    // let val_bits: Vec<bool> = (0..64) // smaller address correspond to less significant bit
    //     .scan(val, |acc, _| {
    //         let bit = (*acc & 1) != 0;
    //         *acc >>= 1;
    //         Some(bit)
    //     })
    //     .collect();
    // for (i, bit) in row.a_prime_prime_0_0_bits.iter_mut().enumerate() {
    //     *bit = F::from_bool(val_bits[i]);
    // }

    query |row| {
        let _ = array::map_enumerated(a_prime_prime_0_0_bits, |i, a_i| {
            let limb = i / 16;
            let bit_in_limb = i % 16;

            provide_value(
                a_i, 
                row, 
                fe((int(eval(a_prime_prime[limb])) >> bit_in_limb) & 0x1)
            );
        });
    };

    // // A''[0, 0] is additionally xor'd with RC.
    // for limb in 0..U64_LIMBS {
    //     let rc_lo = rc_value_limb(round, limb);
    //     row.a_prime_prime_prime_0_0_limbs[limb] =
    //         F::from_canonical_u16(row.a_prime_prime[0][0][limb].as_canonical_u64() as u16 ^ rc_lo);
    // }

    let query_a_prime_prime_prime_0_0_limbs: int -> int = query |limb| 
        int(eval(a_prime_prime[limb])) ^ 
        ((int(eval(utils::sum(NUM_ROUNDS, |r| expr(RC[r]) * step_flags[r]))) >> (limb * 16)) & 0xffff);

    query |row| {
        let _ = array::new(4, |limb| {
            provide_value(a_prime_prime_prime_0_0_limbs[limb], row, fe(query_a_prime_prime_prime_0_0_limbs(limb)));
        });
    };
}
