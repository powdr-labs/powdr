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

machine Keccak with
    degree: 24,
    latch: final_step,
    operation_id: operation_id,
    call_selectors: sel, // is this needed?
{
    // toy inputs
    array::map(preimage, |i| first_step * i = 0);
    
    // operation keccakf<0> preimage[0], preimage[1], ... -> a_prime_prime_prime[0, 0, 0], a_prime_prime_prime[0, 0, 1], ....
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

    pol commit export;
    pol commit preimage[100]; // 5 * 5 * 4
    pol commit a[100]; // 5 * 5 * 4
    // pol commit c[320]; // 5 * 64
    pol commit c_prime[320]; // 5 * 64
    pol commit a_prime[1600]; // 5 * 5 * 64
    pol commit a_prime_prime[100]; // 5 * 5 * 4 
    pol commit a_prime_prime_0_0_bits[64]; // 64
    pol commit a_prime_prime_prime_0_0_limbs[4]; // 4

    // eval_round_flags(builder);

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

    pol constant first_step(i) { if i % NUM_ROUNDS == 0 { 1 } else { 0 } }; // same as step_flags[0]
    pol constant final_step(i) { if i % NUM_ROUNDS == NUM_ROUNDS - 1 { 1 } else { 0 } }; // same as step_flags[NUM_ROUNDS - 1]
    pol constant not_final_step(i) { if i % NUM_ROUNDS == NUM_ROUNDS - 1 { 0 } else { 1 } };

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

    array::new(100, |i| first_step * (preimage[i] - a[i]) = 0);

    // // The export flag must be 0 or 1.
    // builder.assert_bool(local.export);

    force_bool(export);

    // // If this is not the final step, the export flag must be off.
    // builder
    //     .when(not_final_step.clone())
    //     .assert_zero(local.export);

    not_final_step * export = 0;

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

    array::new(100, |i| not_final_step * (preimage[i] - preimage[i]') = 0); // why is not_final_step and when_transition both needed? aren't they the same?

    // // C'[x, z] = xor(C[x, z], C[x - 1, z], C[x + 1, z - 1]).
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

    array::map(c, |i| force_bool(i));
    let xor: expr, expr -> expr = |a, b| a + b - 2*a*b;
    let xor3: expr, expr, expr -> expr = |a, b, c| a + b + c - 2*(a*b + b*c + c*a) + 4*a*b*c;
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
        xor3(c[i], c[((x + 4) % 5) * 64 + z], c[((x + 1) % 5) * 64 + ((z + 63) % 64)]) - c_prime[i] = 0
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
    //             let computed_limb = (limb * BITS_PER_LIMB..(limb + 1) * BITS_PER_LIMB)
    //                 .rev()
    //                 .fold(AB::Expr::zero(), |acc, z| {
    //                     builder.assert_bool(local.a_prime[y][x][z]);
    //                     acc.double() + get_bit(z)
    //                 });
    //             builder.assert_eq(computed_limb, a_limb);
    //         }
    //     }
    // }

    array::map(a_prime, |i| force_bool(i));
    array::new(100, |i| {
        let y = i / 20;
        let x = (i / 4) % 5;
        let limb = i % 4;
        let get_bit: int -> expr = |z| xor3(a_prime[y * 320 + x * 64 + z], c[x * 64 + z], c_prime[x * 64 + z]);
        let computed_limb: expr = utils::fold(16, |z| get_bit((limb + 1) * 16 - 1 - z), 0, |acc, e| (acc * 2 + e)); // 15->0, 31->16, 47->32, 63->48, where z is always 0->15 in utils::fold
        computed_limb - a[i] = 0
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

    let andn: expr, expr -> expr = |a, b| (1 - a) * b;

    // have similar questions for this chunk as the chunk above involving limbs
    array::new(100, |i| {
        let y = i / 20;
        let x = (i / 4) % 5;
        let limb = i % 4;
        
        let get_bit: int -> expr = |z| {
            xor(b(x, y, z), andn(b((x + 1) % 5, y, z), b((x + 2) % 5, y, z)))
        };
        let computed_limb: expr = utils::fold(16, |z| get_bit((limb + 1) * 16 - 1 - z), 0, |acc, e| (acc * 2 + e));
        computed_limb - a_prime_prime[i] = 0
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
        let computed_a_prime_prime_0_0_limb = utils::fold(16, |z| a_prime_prime_0_0_bits[(limb + 1) * 16 - 1 - z], 0, |acc, e| (acc * 2 + e));
        computed_a_prime_prime_0_0_limb - a_prime_prime[limb] = 0
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
        let computed_a_prime_prime_prime_0_0_limb = utils::fold(16, |z| get_xored_bit((limb + 1) * 16 - 1 - z), 0, |acc, e| (acc * 2 + e));
        computed_a_prime_prime_prime_0_0_limb - a_prime_prime_prime_0_0_limbs[limb] = 0
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

    array::new(100, |i| {
        let x = i / 20;
        let y = (i / 4) % 5;
        let limb = i % 4;
        not_final_step * (a_prime_prime_prime(y, x, limb) - a[i]') = 0
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
    
    let RC_BITS: int[] = [ // 24 * 64
        1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0,
    
        0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0,
    
        0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 1,
    
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 1,

        1, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0,

        1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0,

        1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 1,

        1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 1,

        0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0,

        0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0,

        1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0,

        0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0,

        1, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0,

        1, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 1,

        1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 1,

        1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 1,

        0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 1,

        0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 1,

        0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0,

        0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 1,

        1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 1,

        0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 1,

        1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0,

        0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 1
    ];

    // pub fn input_limb(i: usize) -> usize {
    //     debug_assert!(i < RATE_LIMBS);

    //     let i_u64 = i / U64_LIMBS;
    //     let limb_index = i % U64_LIMBS;

    //     // The 5x5 state is treated as y-major, as per the Keccak spec.
    //     let y = i_u64 / 5;
    //     let x = i_u64 % 5;

    //     KECCAK_COL_MAP.preimage[y][x][limb_index]
    // }

    let input_limb: int -> expr = |i| preimage[i]; // return value is usize rather than expr?

    // pub fn output_limb(i: usize) -> usize {
    //     debug_assert!(i < RATE_LIMBS);

    //     let i_u64 = i / U64_LIMBS;
    //     let limb_index = i % U64_LIMBS;

    //     // The 5x5 state is treated as y-major, as per the Keccak spec.
    //     let y = i_u64 / 5;
    //     let x = i_u64 % 5;

    //     KECCAK_COL_MAP.a_prime_prime_prime(y, x, limb_index)
    // }

    let output_limb: int -> expr = |i| { // return value is usize rather than expr?
        let y = i / 20;
        let x = (i / 4) % 5;
        let limb = i % 4;
        a_prime_prime_prime(y, x, limb)
    };

    // hints

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

    let c: inter[320] = array::new(320, |i| {
        let x = i / 64;
        let z = i % 64;
        let limb = z / 16;
        let bit_in_limb = z % 16;

        let c_i;
        set_hint(c_i, |_| Query::Hint(fe(utils::fold(5, |y| (int(eval(a[y * 20 + x * 4 + limb])) >> bit_in_limb) & 0x1, 0, |acc, e| acc ^ e))));
        c_i
    });

}
