use std::array;
use std::utils;


// ln 9 - 12
// TODO: check that these are good for word size 32
let RHO: int[] = [
    1,  3,  6,  10, 15, 21, 28, 36, 45, 55, 2,  14,
    27, 41, 56, 8,  25, 43, 62, 18, 39, 61, 20, 44
];

// ln 14 - 16
// TODO: check that these are good for word size 32
let PI: int[] = [
    10, 7,  11, 17, 18, 3, 5,  16, 8,  21, 24, 4,
    15, 23, 19, 13, 12, 2, 20, 14, 22, 9,  6,  1
];
// inverse of PI using 1 index plus zero at the beginning
// because rho pi step doesn't update st[0]
let PI_INVERSE: int[] = [
    0, 
    24, 18, 6, 12, 7, 23, 2, 9, 22, 1, 3, 17, 
    16, 20, 13, 8, 4, 5, 15, 19, 10, 21, 14, 11
];

// ln 19 - 28
// TODO: check that these are good for word size 32
let RC = std::array::map([
    0x0000000000000001, 0x0000000000008082, 0x800000000000808a,
    0x8000000080008000, 0x000000000000808b, 0x0000000080000001,
    0x8000000080008081, 0x8000000000008009, 0x000000000000008a,
    0x0000000000000088, 0x0000000080008009, 0x000000008000000a,
    0x000000008000808b, 0x800000000000008b, 0x8000000000008089,
    0x8000000000008003, 0x8000000000008002, 0x8000000000000080,
    0x000000000000800a, 0x800000008000000a, 0x8000000080008081,
    0x8000000000008080, 0x0000000080000001, 0x8000000080008008
], |x| Gate::Constant(x));

// Note that neither t nor bc is needed as they are both helper variables
// ln 52 - 55
let theta_bc = |s, st, i|
    std::array::fold([st[i + 5], st[i + 10], st[i + 15]], (s, st[i]), xor);

// ln 57 - 62
let theta_st = |s, st| {
    let (s2, bc) = new_array_stateful(5, s, |s1, i| theta_bc(s1, st, i));
    let (s4, bc_rot) = new_array_stateful(5, s2, |s3, i| xor(rotl64((s3, bc[(i + 1) % 5]), 1), bc[(i + 4) % 5]));
    map_enumerated_stateful(st, s4, |s5, idx, elem| xor((s5, bc_rot[idx % 5]), elem))
};

// ln 66 - 72
// rho pi
let rho_pi = |s, st, i| {
    let p = if i == 0 { 23 } else { i - 1 };
    rotl64((s, st[PI[p]]), RHO[i])
};

let new_array_stateful = |l, s, f| std::utils::fold(l, |i| i, (s, []), |(s1, res), i| {
    let (s2, x) = f(s1, i);
    (s2, res + [x])
});

let map_enumerated_stateful = |arr, s, f|
    new_array_stateful(std::array::len(arr), s, |s1, i| f(s1, i, arr[i]));

// collect st_j
let rho_pi_loop = |(s, st)| new_array_stateful(25, s, |s1, i| if i == 0 { (s1, st[0]) } else { rho_pi(s1, st, i - 1) } );
// rearrange st_j
let rho_pi_rearrange = |(s, st)| (s, array::new(25, |i| st[PI_INVERSE[i]]));

// chi
let chi = |(s, st)| map_enumerated_stateful(st, s, |s1, idx, elem| {
    let i = idx / 5;
    let j = idx % 5;
    xor(and_not((s, st[i * 5 + (j + 1) % 5]), st[i * 5 + (j + 2) % 5]), st[idx])
});

// ln 85 - 86
// iota
let iota: (int, Gate[]), int -> (int, Gate[]) = |(s, st), r| map_enumerated_stateful(st, s, |s1, idx, elem| if idx == 0 { xor((s1, elem), RC[r]) } else { (s1, elem) } ); // int[25], int -> int[25]

// ln 51 - 87
let r_loop = |(s, st)| {
    let (s_f, g) = utils::fold(24, |i| i, (s, st), |(s2, st2), r| iota(chi(rho_pi_rearrange(rho_pi_loop(theta_st(s2, st2)))), r));
    let _ = std::debug::println("Gates:");
    let _ = std::debug::println(s_f);
    g
};

enum Gate {
    Input(int),
    Constant(int),
    Xor(Gate, Gate),
    AndNot(Gate, Gate),
    Rotl(Gate, int)
}

enum Option<T> {
    None,
    Some(T),
}

let<T1, T2> option_map: Option<T1>, (T1 -> T2) -> Option<T2> = |x, f| match x {
    Option::Some(v) => Option::Some(f(v)),
    Option::None => Option::None
};

enum BTreeNode<K, V> {
    N(K, V, int, Option<BTreeNode>, Option<BTreeNode>)
}

enum CmpResult {
    Less,
    Equal,
    Greater,
}

// TODO cmp should be stored together with the tree
let<K, V> btree_find: BTreeNode<K, V>, (K, K -> CmpResult), K -> Option<V> = |node, cmp, needle| match node {
    BTreeNode::N(key, value, _, left, right) => match cmp(needle, key) {
        CmpResult::Less => option_map(left, |l| btree_find(l, cmp, needle)),
        CmpResult::Equal => Option::Some(value),
        CmpResult::Greater => option_map(right, |r| btree_find(r, cmp, needle)),
    }
};

// TODO the rotations below are only simple rotations, but we also need
// to implement more complicated rotations.

let btree_insert = |node, cmp, k, v| match node {
    Option::None => Option::Some(BTreeNode::N(k, v, 1, Option::None, Option::None)),
    Option::Some(BTreeNode::N(key, value, depth, left, right)) => Option::Some(match cmp(k, key) {
        CmpResult::Less => {
            let new_left = btree_insert(left, cmp, k, v);

            let left_depth = btree_node_depth(new_left);
            let right_depth = btree_node_depth(right)
            if left_depth >= right_depth + 2 {
                match new_left {
                    Option::Some(BTreeNode::N(l_k, l_v, l_d, l_left, l_right)) =>
                        update_depth(Option::Some(BTreeNode::N(l_k, l_v, l_d, l_left, Option::Some(
                            update_depth(Option::Some(BTreeNode::N(key, value, l_right, right)))
                        )))),
                    Option::None => std::check::panic(),
                }
            } else {
                Option::Some(BTreeNode::N(key, value, max(left_depth, right_depth) + 1, new_left, right)))
            }
        },
        CmpResult::Equal => Option::Some(BTreeNode::N(k, v, depth, left, right)),
        CmpResult::Greater => {
            let new_right = btree_insert(right, cmp, k, v);

            let left_depth = btree_node_depth(left);
            let right_depth = btree_node_depth(new_right)
            if right_depth >= left_depth + 2 {
                match new_right {
                    Option::Some(BTreeNode::N(l_k, l_v, l_d, l_left, l_right)) =>
                        update_depth(Option::Some(BTreeNode::N(l_k, l_v, l_d, l_left, Option::Some(
                            update_depth(Option::Some(BTreeNode::N(key, value, l_right, right)))
                        )))),
                    Option::None => std::check::panic(),
                }
            } else {
                Option::Some(BTreeNode::N(key, value, max(left_depth, right_depth) + 1, new_right, right)))
            }
        },
    })
};

let btree_node_depth = |node| match node {
    Option::None => 0,
    Option::Some(BTreeNode::N(_, _, depth, _, _)) => depth,
};

let update_depth = |node| match node {
    Option::None => Option::None,
    Option::Some(BTreeNode::N(k, v, _, l, r)) => {
        let dl = btree_node_depth(l);
        let dr = btree_node_depth(r);
        Option::Some(BTreeNode::N(k, v, max(dl, dr) + 1, l, r))
    }
};

/*


*/

let size_overhead = 2; // we need two edges to store one word

let input: int, int -> (int, Gate) = |s, i| (s + size_overhead, Gate::Input(i));
let xor = |(s, a), b| (s + size_overhead, Gate::Xor(a, b)); // TODO create ID
let and_not = |(s, a), b| (s + size_overhead, Gate::AndNot(a, b));
let rotl64 = |(s, a), n| (s, Gate::Rotl(a, n)); // TODO not sure about the overhead


// TODO this is wrong because it counts multiplicities, i.e.
// it counts the size of the circuit expanded to a tree.
let gate_count: Gate -> int = |g| match g {
    Gate::Input(_) => 1,
    Gate::Constant(_) => 1,
    Gate::Xor(a, b) => gate_count(a) + gate_count(b) + 1,
    Gate::AndNot(a, b) => gate_count(a) + gate_count(b) + 1,
    Gate::Rotl(a, _) => gate_count(a) + 1,
};

let gate_to_string: Gate -> string = |g| match g {
    Gate::Input(i) => "input_" + std::convert::to_string(i),
    Gate::Constant(k) => std::convert::to_string(k),
    Gate::Xor(a, b) => "(" + gate_to_string(a) + " ^ " + gate_to_string(b) + ")",
    Gate::AndNot(a, b) => "(~" + gate_to_string(a) + " & " + gate_to_string(b) + ")",
    Gate::Rotl(a, _) => "rotl(" + gate_to_string(a) + ")",
};

let eval = |inputs, g| match g {
    Gate::Input(i) => inputs[i],
    Gate::Constant(k) => k,
    Gate::Xor(a, b) => eval(inputs, a) ^ eval(inputs, b),
    Gate::AndNot(a, b) => (eval(inputs, a) ^ 0xffffffffffffffff) & eval(inputs, b),
    Gate::Rotl(a, k) => {
        let x = eval(inputs, a);
        ((x << k) | (x >> k)) & 0xffffffffffffffff
    }
};

machine Main { 
    let x;

    let inputs = new_array_stateful(25, 0, |s, i| input(s, i));
    let circuit = r_loop(inputs);
    std::debug::println("Gate count:");
    std::debug::println(std::array::sum(std::array::map(circuit, |g| gate_count(g))));
    std::debug::println(gate_to_string(circuit[1]));
}
