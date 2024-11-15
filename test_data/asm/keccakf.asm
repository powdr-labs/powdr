use std::array;
use std::utils;

// elementary gates
let or: int, int -> int = |a, b| a | b;
let and: int, int -> int = |a, b| a & b;
let xor: int, int -> int = |a, b| a ^ b;
let not: int -> int = |a| a ^ 0xffffffffffffffff; // bitwise not for 64 bits
let rotl64: int, int -> int = |x, n| and(or((x << n), (x >> (64 - n))), 0xffffffffffffffff); // left rotation

// constants
let RHO: int[] = [
    1,  3,  6,  10, 15, 21, 28, 36, 45, 55, 2,  14,
    27, 41, 56, 8,  25, 43, 62, 18, 39, 61, 20, 44
];
let PI: int[] = [
    10, 7,  11, 17, 18, 3, 5,  16, 8,  21, 24, 4,
    15, 23, 19, 13, 12, 2, 20, 14, 22, 9,  6,  1
];
// PI[PI_INVERSE[i] - 1] = i for all i = 1 to 24
// PI_INVERSE has one more element than PI,
// with an extra 0 inserted at the start,
// because st[0] isn't updated in rho pi step
let PI_INVERSE: int[] = [
    0, 
    24, 18, 6, 12, 7, 23, 2, 9, 22, 1, 3, 17, 
    16, 20, 13, 8, 4, 5, 15, 19, 10, 21, 14, 11
];
let RC: int[] = [
    0x0000000000000001, 0x0000000000008082, 0x800000000000808a,
    0x8000000080008000, 0x000000000000808b, 0x0000000080000001,
    0x8000000080008081, 0x8000000000008009, 0x000000000000008a,
    0x0000000000000088, 0x0000000080008009, 0x000000008000000a,
    0x000000008000808b, 0x800000000000008b, 0x8000000000008089,
    0x8000000000008003, 0x8000000000008002, 0x8000000000000080,
    0x000000000000800a, 0x800000008000000a, 0x8000000080008081,
    0x8000000000008080, 0x0000000080000001, 0x8000000080008008
];

// helper functions
let swap_u64: int -> int = |x| { // byte swap
    let val = or(and((x << 8), 0xFF00FF00FF00FF00), and((x >> 8), 0x00FF00FF00FF00FF));
    let val_2 = or(and((val << 16), 0xFFFF0000FFFF0000), and((val >> 16), 0x0000FFFF0000FFFF));
    or(and((val_2 << 32), 0xFFFFFFFFFFFFFFFF), (val_2 >> 32)) // added 0xFFFFFFFFFFFFFFFF to mask val_2 << 32
};

// ln 47 - 49
let swap_u64_loop: int[] -> int[] = |st| array::new(25, |i| swap_u64(st[i])); // int[25] -> int[25]

// ln 52 - 55
// note that neither t nor bc is needed as they are both helper variables
let xor_mult: int[] -> int = |input| array::fold(input, 0, |x, y| xor(x, y));
let theta_bc: int[], int -> int = |st, i| xor_mult([st[i], st[i + 5], st[i + 10], st[i + 15], st[i + 20]]); // int[25], int -> int

// ln 57 - 62
let theta_st: int[] -> int[] = |st| array::map_enumerated(st, |idx, elem| { // int[25] -> int[25]
    let i = idx % 5;
    let t = xor(theta_bc(st, (i + 4) % 5), rotl64(theta_bc(st, (i + 1) % 5), 1));
    xor(elem, t)
});

// ln 66 - 72
// rho pi
let rho_pi: int[], int -> int = |st, i| { // int[25], int -> int
    let p = if i == 0 { 23 } else { i - 1 };
    rotl64(st[PI[p]], RHO[i])
};
// collect st_j
let rho_pi_loop: int[] -> int[] = |st| array::new(25, |i| if i == 0 { st[0] } else { rho_pi(st, i - 1) } ); // int[25] -> int[25]
// rearrange st_j
let rho_pi_rearrange: int[] -> int[] = |st| array::new(25, |i| st[PI_INVERSE[i]]); // int[25] -> int[25]

// ln 74 - 83
// chi
let chi: int[] -> int[] = |st| array::map_enumerated(st, |idx, elem| { // int[25] -> int[25]
    let i = idx / 5;
    let j = idx % 5;
    xor(st[idx], and(not(st[i * 5 + (j + 1) % 5]), st[i * 5 + (j + 2) % 5]))
});

// ln 85 - 86
// iota
let iota: int[], int -> int[] = |st, r| array::map_enumerated(st, |idx, elem| if idx == 0 { xor(elem, RC[r]) } else { elem } ); // int[25], int -> int[25]

// ln 51 - 87
let keccakf_inner: int[] -> int[] = |st| utils::fold(24, |i| i, st, |acc, r| iota(chi(rho_pi_rearrange(rho_pi_loop(theta_st(acc)))), r) ); // int[25] -> int[25]

// ln 42 - 94
// compression function
let keccakf: int[] -> int[] = |st| swap_u64_loop(keccakf_inner(swap_u64_loop(st))); // int[25] -> int[25]

// ln 96 - 141
// u64 to little endian bytes
let to_bytes: int[] -> int[] = |input| // int[25] -> int[200] 
    array::fold(input, [], |acc, elem| {
        let new_bytes = array::new(8, |i| 
            // elem % (1 << 64) / (1 << 56)
            // elem % (1 << 56) / (1 << 48)
            // elem % (1 << 48) / (1 << 40)
            // ...
            elem % (1 << (64 - 8 * i)) / (1 << (56 - 8 * i))
        );
        acc + new_bytes
    });

// Little endian bytes to u64
let from_bytes: int[] -> int[] = |input| // int[200] -> int[25]
    array::new(25, |i| 
        utils::fold(8, |j| j, 0, |acc, idx| 
            acc + input[i * 8 + 7 - idx] * (1 << (8 * idx))
        )
    );

// ln 148 - 158
// int[N], int, int -> int[25], where N is input number of bytes
let update_finalize_b: int[], int, int -> int[] = |input, rate, delim| {
    let num_loop = array::len(input) / rate;
    let num_remaining = array::len(input) % rate;
    let b_delim_idx = (num_remaining + 1) % rate;

    let b_keccak = utils::fold(num_loop, |i| i, array::new(200, |i| 0), |acc, idx| {
        let new_b = array::zip(array::new(rate, |i| acc[i]), array::new(rate, |i| input[idx * rate + i]), xor);
        let new_b_pad = array::new(200, |i| if i < rate { new_b[i] } else { acc[i] });
        to_bytes(keccakf(from_bytes(new_b_pad)))
    });
    let b_finalize = array::new(200, |i| 
        // num_remaining is 0 the minimum and rate - 1 the maximum
        if i < num_remaining {
            // ln 150, one of the remaining to be xor'ed
            xor(b_keccak[i], input[num_loop * rate + i])
        } else {
            if i == num_remaining {
                if i == rate - 1 {
                    xor_mult([b_keccak[i], delim, 0x80])
                } else {
                    xor(b_keccak[i], delim)
                }
            } else {
                if i == rate - 1 {
                    xor(b_keccak[i], 0x80)
                } else {
                    b_keccak[i]
                }
            }
        }
    );
    // ln 158
    to_bytes(keccakf(from_bytes(b_finalize)))    
};

// ln 143 - 161
// W is output number of bytes, input is array of bytes, delim is a single byte
let main: int, int[], int -> int[] = |W, input, delim| { 
    // ln 144 - 145
    let rate = 200 - (2 * W); // int, 100 if u32

    let b_finalized = update_finalize_b(input,rate, delim); // 200 bytes

    // ln 160
    // note that the biggest W is 64, i.e. 512-bit keccak
    // therefore, W should always be lower than byte length of b_finalized (200)
    array::new(W, |i| b_finalized[i])
};

// main machine
machine KeccakF with
    degree: 8,
    latch: LATCH,
    degree: 8
{ 
    let x;
    let y;
    pol commit STEP;
    operation keccakf x, y, STEP ->;
}
