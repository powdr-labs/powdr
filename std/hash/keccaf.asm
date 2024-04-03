use std::array;

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

// ln 35 - 40
// change endianness for a 32 bit number byte by byte
// e.g. 0xaabbccdd -> 0xddccbbaa
let swap_u32: int -> int = |x| 
    or((or(and((x << 8), 0xff00ff00), and((x >> 8), 0x00ff00ff)) >> 16),
    (or(and((x << 8), 0xff00ff00), and((x >> 8), 0x00ff00ff)) << 16)); 

// compression function

// ln 44
let bc: int[5] = [0, 0, 0, 0, 0];

// ln 45
let t: int = 0;

// ln 47 - 49
let swap_u32_loop: int[25] -> int[25] = |st| array::map(array::new(25, |i| i), |i| swap_u32(st[i]));

// ln 53 - 55
let theta_bc: int[25], i -> int = |st, i| xor(st[i], xor(st[i + 5], xor(st[i + 10], xor(st[i + 15], st[i + 20]))));
let theta_bc_loop: int[25] -> int[5] = |st| array::map(array::new(5, |i| i), |i| theta_bc(st, i));

// ln 57 - 62
let theta_t: int[5] -> int = |bc, i| xor(bc[(i + 4) % 5], rotl32(bc[(i + 1) % 5], 1));
let theta_st: int[25], int, int, int -> int = |st, t, j, i| xor(st[(j * 5) + i], t);
let theta_st_loop: int[25], int, int -> int[5] = |st, t, i| array::map(array::new(5, |i| i), |j| theta_st(st, t, j, i));
let theta_t_loop: int[5], int[25], int -> int[5][5] = |bc, st, t| array::map(array::new(5, |i| i), |i| {
    let t = theta_t(bc, i); // int
    let st_partial = theta_st_loop(st, t, i); // int[5]
    st_partial
});
// rearrange st returned in ln 60 - version 1: calls theta_t_loop
let theta_t_loop_rearrange: int[5], int[25], int -> int[25] = |bc, st, t| {
    let st_partials = theta_t_loop(bc, st, t);
    let st = [
        st_partials[0][0], st_partials[1][0], st_partials[2][0], st_partials[3][0], st_partials[4][0],
        st_partials[0][1], st_partials[1][1], st_partials[2][1], st_partials[3][1], st_partials[4][1],
        st_partials[0][2], st_partials[1][2], st_partials[2][2], st_partials[3][2], st_partials[4][2],
        st_partials[0][3], st_partials[1][3], st_partials[2][3], st_partials[3][3], st_partials[4][3],
        st_partials[0][4], st_partials[1][4], st_partials[2][4], st_partials[3][4], st_partials[4][4]
    ];
    st 
};
// rearrange st returned in ln 60 - version 2: needs to be called with theta_t_loop in another function
let theta_t_loop_rearrange_v2: int[5][5] -> int[25] = |sp_partials| 
    std::array::fold(std::array::new(5, |i| i), [], |initial, i| 
        a + [sp_partials[0][i], sp_partials[1][i], sp_partials[2][i], sp_partials[3][i], sp_partials[4][i]]
    );

// ln 64
let theta_t_reassign: int[25] -> int = |st| st[1];


// main machine
machine Main { 
    let x; 
}