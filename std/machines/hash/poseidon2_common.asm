use std::array;
use std::utils;
use std::machines::small_field::pointer_arith::increment_ptr;

// X to the power of 7. Used as the S-box some fields.
let pow_7 = constr |x| {
    let x3;
    x3 = x * x * x;
    let x7;
    x7 = x3 * x3 * x;
    x7
};

// A full poseidon2 permutation.
let poseidon2 = constr |
    state_size,
    half_external_rounds,
    internal_rounds,
    mds,
    external_round_constants,
    diffusion_diagonal,
    diffusion_multiplier,
    internal_rounds_constants,
    s_box_function,
    input_state| {

    // The linear layer of the external round: multiplication by the MDS matrix.
    let apply_mds = |input, output_len| {
        let dot_product = |v1, v2| array::sum(array::zip(v1, v2, |v1_i, v2_i| v1_i * v2_i));
        array::map(array::sub_array(mds, 0, output_len), |row| dot_product(row, input))
    };

    // The full external round.
    let external_round = constr |c_idx, input, output| {
        // Add constants
        let step_a = array::zip(input, external_round_constants[c_idx], |v, c| v + c);

        // Apply S-box
        let s_boxed = array::map(step_a, s_box_function);

        // Multiply with MDS Matrix
        array::zip(output, apply_mds(s_boxed, array::len(output)), |out, x| out = x);
    };

    let internal_round = constr |c_idx, input, output| {
        // Add constant just to the first element (weird, I thought the entire state was
        // used here, but this is how Plonky3 does it).
        let step_a = input[0] + internal_rounds_constants[c_idx];

        // Apply S-box just to the first element
        let s_boxed = s_box_function(step_a);

        // Multiply with the diffusion matrix
        //
        // The 8x8 diffusion matrix looks like this:
        //
        //                        [A, 1, 1, 1, 1, 1, 1, 1]
        //                        [1, B, 1, 1, 1, 1, 1, 1]
        //                        [1, 1, C, 1, 1, 1, 1, 1]
        // diffusion_multiplier * [1, 1, 1, D, 1, 1, 1, 1]
        //                        [1, 1, 1, 1, E, 1, 1, 1]
        //                        [1, 1, 1, 1, 1, F, 1, 1]
        //                        [1, 1, 1, 1, 1, 1, G, 1]
        //                        [1, 1, 1, 1, 1, 1, 1, H]
        //
        // Where A, B, C, ..., H are the elements of the "diffusion_diagonal" array plus 1.
        //
        // The idea of using such matrix in Poseidon2 is that, instead of performing
        // a full matrix multiplication, we can optimize it by summing the elements
        // of the input vector, and then adjusting each output[k] element by
        // input[k] * diffusion_diagonal[k].
        let line_sum = s_boxed + array::sum(array::sub_array(input, 1, state_size - 1));
        output[0] = (line_sum + diffusion_diagonal[0] * s_boxed) * diffusion_multiplier;
        array::zip(
            array::zip(
                array::sub_array(input, 1, state_size - 1),
                array::sub_array(output, 1, state_size - 1),
                constr |in_v, out_v| (in_v, out_v)
            ),
            array::sub_array(diffusion_diagonal, 1, state_size - 1),
            constr |(in_v, out_v), diag| out_v = (line_sum + diag * in_v) * diffusion_multiplier
        );
    };

    // We now chain the rounds together:

    // Perform the initial MDS step
    let pre_rounds = apply_mds(input_state, state_size);

    // Perform the first half of the external rounds
    let after_initial_rounds = utils::fold(
        half_external_rounds, |round_idx| round_idx, pre_rounds,
        constr |pre_state, round_idx| {
            let post_state = array::new(state_size, |_| { let x; x});
            external_round(round_idx, pre_state, post_state);
            post_state
        }
    );

    // Perform the internal rounds
    let after_internal_rounds = utils::fold(
        internal_rounds, |round_idx| round_idx, after_initial_rounds,
        constr |pre_state, round_idx| {
            let post_state = array::new(state_size, |_| { let x; x});
            internal_round(round_idx, pre_state, post_state);
            post_state
        }
    );

    // Perform the second half of the external rounds
    utils::fold(
        half_external_rounds,
        |round_idx| round_idx + half_external_rounds,
        after_internal_rounds,
        constr |pre_state, round_idx| {
            let post_state = array::new(state_size, |_| { let x; x});
            external_round(round_idx, pre_state, post_state);
            post_state
        }
    )
};
