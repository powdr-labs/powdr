use std::array;
use std::utils;

mod circuit;

use circuit::State;
use circuit::Gate;


/// Represent the numbers 0 to 63, used for rotl.
let rotl_constants = array::new(64, |i| Gate::Reference(25 + i));
/// Represent the array RC.
let rc_constants = array::new(24, |i| Gate::Reference(25 + 64 + i));

let xor_c: Gate, Gate -> Gate = |a, b| Gate::Op(1, a, b);
let rotl64_c: Gate, int -> Gate = |x, n| Gate::Op(2, x, rotl_constants[n]);
/// Evaluates as and(not(a), b)
let and_not_c: Gate, Gate -> Gate = |a, b| Gate::Op(3, a, b);

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

let theta_bc_c = |st, i| xor_c(xor_c(xor_c(xor_c(st[i], st[i + 5]), st[i + 10]), st[i + 15]), st[i + 20]);

let theta_st_c = |state, inputs| {
    let bc = array::new(5, |i| theta_bc_c(inputs, i));
    // TODO we could turn the bc into a reference here already.
    let r = array::map_enumerated(inputs, |idx, elem| {
        let i = idx % 5;
        let t = xor_c(bc[(i + 4) % 5], rotl64_c(bc[(i + 1) % 5], 1));
        xor_c(elem, t)
    });
    circuit::add_routines(state, r)
};

let rho_pi_c: Gate[], int -> Gate = |inputs, i| {
    let p = if i == 0 { 23 } else { i - 1 };
    rotl64_c(inputs[PI[p]], RHO[i])
};
// collect st_j
let rho_pi_loop_c = |inputs| array::new(25, |i| if i == 0 { inputs[0] } else { rho_pi_c(inputs, i - 1) } );

// rearrange st_j
let rho_pi_rearrange_c: Gate[] -> Gate[] = |inputs| array::new(25, |i| inputs[PI_INVERSE[i]]);

// chi
let chi_c: Gate[] -> Gate[] = |inputs| array::map_enumerated(inputs, |idx, elem| {
    let i = idx / 5;
    let j = idx % 5;
    xor_c(inputs[idx], and_not_c(inputs[i * 5 + (j + 1) % 5], inputs[i * 5 + (j + 2) % 5]))
});

// iota
let iota_c: Gate[], int -> Gate[] = |inputs, r| array::map_enumerated(inputs, |idx, elem| if idx == 0 { xor_c(elem, rc_constants[r]) } else { elem } );

let add_inputs: State, int -> (State, Gate[]) = |state, n|
    utils::fold(n, |i| i, (state, []), |(s, inp), _| {
        let (s2, g) = circuit::add_input(s);
        (s2, inp + [g])
    });

let keccakf_circuit: -> (circuit::State, Gate[]) = || {
    let (s1, inputs) = add_inputs(circuit::new(), 25);
    let (s2, rotl_const) = add_inputs(s1, 64);
    let (s3, rc_const) = add_inputs(s2, 24);
    // TODO assert that rotl_const equal rotl_constants
    // TODO assert that rc_const equal rc_constants
    utils::fold(24, |i| i, (s3, inputs), |(s, st), r| {
        let (s_1, th) = theta_st_c(s, st);
        circuit::add_routines(s_1, iota_c(chi_c(rho_pi_rearrange_c(rho_pi_loop_c(th))), r))
    })
};

let eval_gate: int, int, int -> int = |gate, in1, in2| match gate {
    1 => in1 ^ in2,
    2 => (((in1 << in2) | (in1 >> (64 - in2))) & 0xffffffffffffffff),
    3 => (in1 ^ 0xffffffffffffffff) & in2,
    _ => std::check::panic("Invalid gate"),
};

let eval_circuit: circuit::State, Gate[], int[] -> int[] = |state, outputs, inputs| match state {
    State::S(gates, _) => {
        let initial = inputs + array::new(64, |i| i) + array::new(24, |i| RC[i]);
        let values = std::utils::fold(std::array::len(gates), |i| i, [], |acc, i| {
            let value = if i < array::len(initial) { initial[i] } else {
                let (id, in1, in2) = gates[i];
                eval_gate(id, acc[in1], acc[in2])
            };
            acc + [value]
        });
        array::map(outputs, |g| match g {
            Gate::Reference(i) => values[i],
            _ => std::check::panic("Invalid output gate"),
        })
    },
};


let test = || {
    let input = [8315180248889782138, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9223372036854775808, 0, 0, 0, 0, 0, 0, 0, 0];
    let expectation = [
        0xb6dc406d97d185ca,
        0x836e59c6c8ec3bca,
        0x6a01cf85414f77c0,
        0x397fa356584f8305,
        0xc8ad140a950d0cba,
        0x3dacc584b36c843f,
        0x428a466fad758146,
        0xa68af9b0cfafaf4c,
        0xffbba567083af2a9,
        0xb880d631598051a4,
        0x683f441da93e7295,
        0xbb5b42d2641b17c6,
        0xf4ec07dc2064443c,
        0x21959efb97953f8b,
        0x31c5e9b638335876,
        0xaa95e01d2bf963ed,
        0x82117b4b8decd828,
        0xe2a255871d47f57f,
        0x659b271c81bf6d3b,
        0x680b15e3d98b97ee,
        0x58118ac68850970d,
        0xada41f9e251307e6,
        0xf9a0529a1f229355,
        0x17cf3d9d8026e97f,
        0xdf84d5da988117d2
    ];

    let (circuit_state, circuit_outputs) = keccakf_circuit();
    let l = match circuit_state {
        State::S(gates, _) => std::array::len(gates),
    };
    let _ = std::debug::print("Gate count: ");
    let _ = std::debug::println(l);
    let result2 = eval_circuit(circuit_state, circuit_outputs, input);
    let _ = std::array::zip(result2, expectation, |a, b| std::check::assert(a == b, || "Keccakf failed"));
    []
};

// main machine
machine Main { 
    let x;
    test();
}
