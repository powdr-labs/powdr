use std::array;
use std::utils;

// TODO
let<T1, T2> routine: T1, T1, (T1, T1 -> T1), (T1, T1 -> T1), (T1, T1 -> T1) -> T2 =
    |a, b, xor, and, or| a + b;

// function definition
let or: int, int -> int = |a, b| a | b;
let and: int, int -> int = |a, b| a & b;
let xor: int, int -> int = |a, b| a ^ b;

// ln 9 - 12
// TODO: check that these are good for word size 32
let RHO: int[24] = [
    1,  3,  6,  10, 15, 21, 28, 36, 45, 55, 2,  14,
    27, 41, 56, 8,  25, 43, 62, 18, 39, 61, 20, 44
];

// ln 14 - 16
// TODO: check that these are good for word size 32
let PI: int[24] = [
    10, 7,  11, 17, 18, 3, 5,  16, 8,  21, 24, 4,
    15, 23, 19, 13, 12, 2, 20, 14, 22, 9,  6,  1
];

// ln 19 - 28
// TODO: check that these are good for word size 32
let RC: int[24] = [
    0x0000000000000001, 0x0000000000008082, 0x800000000000808a,
    0x8000000080008000, 0x000000000000808b, 0x0000000080000001,
    0x8000000080008081, 0x8000000000008009, 0x000000000000008a,
    0x0000000000000088, 0x0000000080008009, 0x000000008000000a,
    0x000000008000808b, 0x800000000000008b, 0x8000000000008089,
    0x8000000000008003, 0x8000000000008002, 0x8000000000000080,
    0x000000000000800a, 0x800000008000000a, 0x8000000080008081,
    0x8000000000008080, 0x0000000080000001, 0x8000000080008008
];

// ln 30 - 33
// left rotation
let rotl32: u32, u32 -> u32 = |x, n| or((x << n), (x >> (32 - n)));

// ln 35 - 40
// change endianness for a 32 bit number byte by byte
// e.g. 0xaabbccdd -> 0xddccbbaa
let swap_u32: int -> int = |x| 
    or((or(and((x << 8), 0xff00ff00), and((x >> 8), 0x00ff00ff)) >> 16),
    (or(and((x << 8), 0xff00ff00), and((x >> 8), 0x00ff00ff)) << 16)); 

// ln 44
// no bc is needed as it's just a helper array

// ln 45
// no t needed as it's just a helper variable

// ln 47 - 49
let swap_u32_loop: int[25] -> int[25] = |st| array::new(25, |i| swap_u32(st[i]));

// ln 52 - 55
let xor_mult: int[] -> int = |input| std::array::fold(input, 0, |x, y| xor(x, y));
let theta_bc: int[25] -> int = |st, i| |i| xor_mult([st[i], st[i + 5], st[i + 10], st[i + 15], st[i + 20]]);

// ln 57 - 62
let theta_st: int[25] -> int[25] = |st| array::map_enumerated(st, |idx, elem| {
    let i = idx % 5;
    let j = idx / 5;
    let t = xor(theta_bc(st, (i + 4) % 5), rotl32(theta_bc(st, (i + 1) % 5), 1));
    xor(elem, t)
});

// ln 64
// t will be initialized in fold

// ln 66 - 72
// rho pi

// for u32 i in 0..24 {
//    u32 j = PI[i];
//    bc[0] = st[j];
//    st[j] = rotl64(t, RHO[i]);
//    t = bc[0];
// }
// reduced to:
// for u32 i in 0..24 {
//    u32 j = PI[i];
//    t_next = st[j];
//    st[j] = rotl64(t, RHO[i]);
//    t = t_next;
// }
// bc[0] = st[PI[23]];

// helper for the fold loop
let rho_pi: int[25], int -> int = |st, i| {
    let t = if i == 0 { st[1] } else { st[PI[i - 1]] };
    let new_st_j = rotl32(t, RHO[i]);
    (new_st_j)
};
// collect st_j
let rho_pi_loop: int[25] -> int[25] = |st| utils::fold(24, |i| i, [], |new_st, idx| {
    let new_st_j = rho_pi(st, idx);
    new_st + [new_st_j]
});

// rearrange st_j
let rho_pi_rearrange: int[25] -> int[25] = |st| {
    let rearranged_st = [
        st[23], st[17], st[5], st[11], st[6], st[22],
        st[1], st[8], st[21], st[0], st[2], st[16], 
        st[15], st[19], st[12], st[7], st[3], st[4], 
        st[14], st[18], st[9], st[20], st[13], st[10]
    ];
    rearranged_st
};

// ln 74 - 83
// chi
// TODO: make sure that modulus has the same precedence as multiplication
let chi: int[25] -> int[25] = |st| array::map_enumerated(st, |idx, elem| {
    let i = idx / 5;
    let j = idx % 5;
    xor(st[idx], and(not(st[i * 5 + (j + 1) % 5]), st[i * 5 + (j + 2) % 5]))
});

// ln 85 - 86
// iota
let iota: int[25], int -> int[25] = |st, r| array::new(24, |i| if i == 0 { xor(st[0], RC[r]) } else { st[i] } );

// ln 51 - 87
let r_loop: int[25] -> int[25] = |st| utils::fold(24, |i| i, st, |acc, i| iota(chi(rho_pi_rearrange(rho_pi_loop(theta_st(acc)))), r) );

// ln 42 - 94
// compression function
let keccakf: int[25] -> int[25] = |st| swap_u32_loop(r_loop(swap_u32_loop(st)));


// main machine
machine Main { 
    let x; 
}