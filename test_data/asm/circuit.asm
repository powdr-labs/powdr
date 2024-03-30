enum Op {
    Input(int),
    Xor(Op, Op),
    And(Op, Op),
    Rotl(Op, Op), // Rotate left by one bit, the second argument is ignored.
}


let symbolic = || (
    Op::Input(0),
    Op::Input(1),
    |x, y| Op::Xor(x, y),
    |x, y| Op::And(x, y),
    |x, y| Op::Rotl(x, y)
);

let concrete: int, int -> (int, int, (int, int -> int), (int, int -> int), (int, int -> int)) = |a, b| (
    a,
    b,
    |x, y| x ^ y,
    |x, y| x & y,
    |x, y| ((x << 1) | (x >> 31)) & 0xffffffff
);

machine Main {
    let w;
 }

// This is the main input, the description of the circuit:
let routine = |(a, b, xor, and)| and(xor(a, b), and(a, xor(b, a)));

/*
// TODO How to efficiently reference repeated ops?
// Through a "ref" Op that has an ID?
// Or should we deduplicate automatically?


// mk_circuit flattens an Op-structure into an
// array of (gate_type, input_id1, input_id2)
let mk_circuit = |routine| {
    mk_routine_inner(routine, []);
};

let mk_routine_inner = |routine, state| {
    match routine {
        Op::xor(a, b) => {
            let (s2, a_out) = mk_routine_inner(state, a);
            let (s3, b_out) = mk_routine_inner(s2, b);
            add_gate(s3, Gate::xor, a_out, b_out);
        }
        ...
    }
};

// add_gate could actually de-duplicate.
let add_gate = |state, gate, in1, in2| {
    let id = std::array::len(state);
    (state + [(gate, in1, in2)], id)
}


// Now we need to turn this into:
// - row to gate type mapping
// - permutation constraints for inputs and outputs

let row_to_gate_type = |row| { 
}

// The main state is just an array of (op, left, right).
// Now the difficulty is to compute the permutation.

let ops_to_permutation = |ops| {
    // First create an edge list and sort it.
    // The first component of the edge list is the gate index.
    // The second component is the vertex index:
    // 0 mod 3: left input, 1 mod 3: right input, 2 mod 3: output
    // TODO Maybe use an enum?
    let input1_id = |row| 3 * row + 0;
    let input2_id = |row| 3 * row + 1;
    let output_id = |row| 3 * row + 2;
    let edges = sort(flatten(map_enumerate(ops, |i, (_, l, r)| [(l, input1_id(i)), (r, input2_id(i))])));
    // Now we compute a successor list by grouping the edges by the first component.

    let successors = {
        // Helper: Take the current list and current tentative successor list
        // and add it to the final list. Also adds empty lists until the length
        // is equal to "next".
        let finalize = |list, succ, next| {
            list + [succ] + repeat([], next - std::array::len(list) - 1)
        };
        let (list, succ) = fold(([], []), |(list, succ), (from, to)| {
            if from == std::array::len(list) {
                (list, succ + [to])
            } else {
                (finalize(list, succ, from), [to])
            }
        });
        finalize(list, succ, len(ops))
    };


    let perm = |i| match i {
        // This is in row-first order, although we need column-first order,
        // but it is probably easy to transpose.
        let op_index = i / 3;
        match i % 3 {
            0 => {
                let (_, source, _) = ops[op_index];
                // TODO What about inputs?
                let succ_index = index_of(successors[source], i);
                if succ_index + 1 >= std::array::len(successors[source]) {
                    output_id(source)
                } else {
                    successors[source][succ_index + 1]
                }
            },
            1 => {
                // TODO this line is the only difference.
                let (_, _, source) = ops[op_index];
                // TODO What about inputs?
                let succ_index = index_of(successors[source], i);
                if succ_index + 1 >= std::array::len(successors[source]) {
                    output_id(source)
                } else {
                    successors[source][succ_index + 1]
                }

            }
            2 => {
                // The output column, it maps to the first successor or to itself.
                let l = std::array::len(successors[i]);
                if l == 0 {
                    i
                } else {
                    successors[i][0]
                }
            }
        }

        if successors[i] == [] {
            3 * i

    };
}
*/