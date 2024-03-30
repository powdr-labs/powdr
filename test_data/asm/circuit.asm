// ------------- gate types - can be generic later, but we probably need generic enums and traits for that -------------------

/// Recursive gate enum
enum Op {
    Input(int),
    Xor(Op, Op),
    And(Op, Op),
    Rotl(Op, Op), // Rotate left by one bit, the second argument is ignored.
}

/// Flattened gate type
enum Gate {
    /// An input gate, the parameters are ignored. The input ID is the gate ID.
    Input,
    Xor,
    And,
    Rotl
}

// TODO How to efficiently reference repeated ops?
// Through a "ref" Op that has an ID?
// Or should we deduplicate automatically?

// ---------------- circuit flattening operations -----------------------------------

// flattens an Op-structure into an array of (gate_type, input_id1, input_id2)
let flatten_circuit = |routine| {
    let input_count = internal::largest_input(routine) + 1;
    let state = std::array::new(input_count, |i| (Gate::Input, i, i));
    let (flattened, _) = internal::flatten_circuit(state, routine);
    flattened
};

mod internal {
    use std::utils::max;
    use super::Op;
    use super::Gate;

    let largest_input = |routine|
        match routine {
            Op::Input(n) => n,
            Op::Xor(a, b) => max(largest_input(a), largest_input(b)),
            Op::And(a, b) => max(largest_input(a), largest_input(b)),
            Op::Rotl(a, b) => max(largest_input(a), largest_input(b)),
        };

    let flatten_circuit = |state, routine|
        match routine {
            Op::Input(n) => (state, n),
            Op::Xor(a, b) => {
                let (s2, a_out) = flatten_circuit(state, a);
                let (s3, b_out) = flatten_circuit(s2, b);
                append_gate(s3, Gate::Xor, a_out, b_out)
            },
            Op::And(a, b) => {
                let (s2, a_out) = flatten_circuit(state, a);
                let (s3, b_out) = flatten_circuit(s2, b);
                append_gate(s3, Gate::And, a_out, b_out)
            },
            Op::Rotl(a, b) => {
                let (s2, a_out) = flatten_circuit(state, a);
                let (s3, b_out) = flatten_circuit(s2, b);
                append_gate(s3, Gate::Rotl, a_out, b_out)
            },
        };

    let append_gate = |state, gate, in1, in2|
        (state + [(gate, in1, in2)], std::array::len(state));

}

enum Vertex {
    Input1,
    Input2,
    Output
}

let vertex_id = |row, vertex| match vertex {
    Vertex::Input1 => 3 * row + 0,
    Vertex::Input2 => 3 * row + 1,
    Vertex::Output => 3 * row + 2,
};

let vertex_id_to_row = |vertex_id| {
    let kind = match vertex_id % 3 {
        0 => Vertex::Input1,
        1 => Vertex::Input2,
        2 => Vertex::Output,
    };
    (vertex_id / 3, kind)
};

// ------------------------------- permutation routines for the copy constraints ---------------------------

/// Computes the permutation from a flattened circuit.
let ops_to_permutation: (Gate, int, int)[] -> (int -> int) = |ops| {
    // First create an edge list and sort it.
    // The first component of the edge list is the gate index.
    // The second component is the vertex index:

    let edges_unsorted = std::array::flatten(std::array::map_enumerated(
        ops,
        |i, (gate, l, r)| match gate {
            Gate::Input => [], // no incoming edges for inputs
            _ => [(l, vertex_id(i, Vertex::Input1)), (r, vertex_id(i, Vertex::Input2))]
        }
    ));
    let edges = std::utils::sort(edges_unsorted, |(i, _), (j, _)| i < j);

    // Now we compute a successor list by grouping the edges by the first component.
    // This is an array where the index is the row number and the values are vertex ids.
    // The successor list also includes the output vertex, i.e. the "from" vertex.
    let successors = {
        // Helper: Take the current list and current tentative successor list
        // and add it to the final list. Also adds empty lists until the length
        // is equal to "next". Also adds the output vertex to the tentative successor lists.
        let finalize = |list, succ, next| {
            let row_id = std::array::len(list);
            list + [succ] + std::array::new(next - row_id - 1, |i| [vertex_id(i, Vertex::Output)])
        };
        let (list, succ) = std::array::fold(edges, ([], [vertex_id(0, Vertex::Output)]), |(list, succ), (from, to)|
            if from == std::array::len(list) {
                // we are still operating on the same output vertex,
                // add "to" as a new vertex to the current list.
                (list, succ + [to])
            } else {
                // this is a new output vertex, finalize the old one
                // and then create a new group
                (finalize(list, succ, from), [to])
            }
        );
        finalize(list, succ, std::array::len(ops))
    };


    // This is the permutation in row-first order, although we need column-first order,
    // but it is probably easy to transpose.
    |i| {
        let (row, vertex_kind) = vertex_id_to_row(i);
        let source = match vertex_kind {
            Vertex::Output => row,
            Vertex::Input1 => { let (_, s, _) = ops[row]; s },
            Vertex::Input2 => { let (_, _, s) = ops[row]; s },
        };
        let succ = successors[source];
        let self_index = std::array::index_of(succ, i);
        succ[(self_index + 1) % std::array::len(succ)]
    }
};

// --------------------------- circuit description ---------------------------------

// This is the main input, the description of the circuit:
let<T1, T2> routine: (T1, T1, (T1, T1 -> T1), (T1, T1 -> T1), (T1, T1 -> T2)) -> T2 =
    |(a, b, xor, and, rotl)| rotl(and(xor(a, b), and(a, xor(b, a))), a);


// symbolic representation of each primitive, we could even implement them in more complex expressions
// (like rot via two shifts and an or)
let symbolic = || (
    Op::Input(0),
    Op::Input(1),
    |x, y| Op::Xor(x, y),
    |x, y| Op::And(x, y),
    |x, y| Op::Rotl(x, y)
);

// concrete representation of each primivite.
let concrete: int, int -> (int, int, (int, int -> int), (int, int -> int), (int, int -> int)) = |a, b| (
    a,
    b,
    |x, y| x ^ y,
    |x, y| x & y,
    |x, y| ((x << 1) | (x >> 8)) & 0xff
);

// A symbolic representation of the circuit
let symbolic_routine = routine(symbolic());
// A concreet representation of the circuit
let concrete_routine = |a, b| routine(concrete(a, b));

let flattened = flatten_circuit(symbolic_routine);
let circuit_len = std::array::len(flattened);

// TODO would be nice to allow certain user-defined types as values
// for fixed columns - maybe via a trait?
// And then we would allow lookups only between columns of the same user-defined type?
// Maybe have `col<Type>`, where Type could be omitted if it can be inferred?
let gate_to_int = |g| match g {
    Gate::Input => 0,
    Gate::Xor => 1,
    Gate::And => 2,
    Gate::Rotl => 3,
};
let int_to_gate = |i| match i {
    0 => Gate::Input,
    1 => Gate::Xor,
    2 => Gate::And,
    3 => Gate::Rotl,
};

let permutation = ops_to_permutation(flattened);
// TODO I don't think this is correct, it shuold add namespace len.
// TODO Are they really stacked on top of each other?
let transposed = |i| match i % 3 {
    0 => i / 3,
    1 => i / 3 + circuit_len,
    2 => i / 3 + 2 * circuit_len,
};

machine Main {
    // A, B are gate inputs, C is the gate output
    col witness A, B, C;
    // C(0) and C(1) are the public inputs
    let GATE: col = |i| { let (gate, _, _) = flattened[i % circuit_len]; gate_to_int(gate) };

    let Conn_A: col = |i| transposed(permutation(3 * (i % circuit_len))) + (i / circuit_len) * circuit_len;
    let Conn_B: col = |i| transposed(permutation(3 * (i % circuit_len) + 1)) + (i / circuit_len) * circuit_len;
    let Conn_C: col = |i| transposed(permutation(3 * (i % circuit_len) + 2)) + (i / circuit_len) * circuit_len;

    let inputs: (int -> int)[] = std::utils::cross_product([256, 256, 4]);
    let a = inputs[0];
    let b = inputs[1];
    let op = inputs[2];
    let P_A: col = |i| a(i);
    let P_B: col = |i| b(i);
    let P_GATE: col = |i| op(i);
    let P_C: col = |i| match int_to_gate(op(i)) {
        Gate::Input => a(i), // there should be no copy-constraints on an input gate, so this should fully unconstrain it
        Gate::Xor => a(i) ^ b(i),
        Gate::And => a(i) & b(i),
        Gate::Rotl => ((a(i) << 1) | (a(i) >> 8)) & 0xff
    };

    { GATE, A, B, C } in { P_GATE, P_A, P_B, P_C };
    { A, B, C } connect { Conn_A, Conn_B, Conn_C };

    // TODO What is the purpose of these constrainst in the polygon file?
    // Global.L1 * a44 = 0;
    // Global.L1 * (2**44-1-b44) = 0;
}