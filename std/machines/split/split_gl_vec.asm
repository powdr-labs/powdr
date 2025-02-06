use std::machines::large_field::memory::Memory;
use super::SplitGL;

machine SplitGLVec8(mem: Memory, split_gl: SplitGL) with
    latch: latch,
    call_selectors: sel,
{
    // One full decomposition per row.
    let latch = 1;

    // Is this a used row?
    let is_used = array::sum(sel);

    // Reads 8 memory words from input_addr as field elements at time_step
    // and writes 16 memory words to output_addr with u32s representing
    // the decomposed field elements, in little-endian (i.e., the
    // lower word address is the least significant limb), in time_step + 1.
    //
    // The addresses must be multiple of 4.
    operation split input_addr, output_addr, time_step;

    let input_addr;
    let output_addr;
    let time_step;

    let input: col[8];

    // TODO: when link is available inside functions, we can turn this into array operations.
    link if is_used ~> input[0] = mem.mload(input_addr + 0, time_step);
    link if is_used ~> input[1] = mem.mload(input_addr + 4, time_step);
    link if is_used ~> input[2] = mem.mload(input_addr + 8, time_step);
    link if is_used ~> input[3] = mem.mload(input_addr + 12, time_step);
    link if is_used ~> input[4] = mem.mload(input_addr + 16, time_step);
    link if is_used ~> input[5] = mem.mload(input_addr + 20, time_step);
    link if is_used ~> input[6] = mem.mload(input_addr + 24, time_step);
    link if is_used ~> input[7] = mem.mload(input_addr + 28, time_step);

    // Split the output into high and low limbs
    let output_low: col[8];
    let output_high: col[8];

    // TODO: turn this into array operations
    link if is_used ~> (output_low[0], output_high[0]) = split_GL.split(input[0]);
    link if is_used ~> (output_low[1], output_high[1]) = split_GL.split(input[1]);
    link if is_used ~> (output_low[2], output_high[2]) = split_GL.split(input[2]);
    link if is_used ~> (output_low[3], output_high[3]) = split_GL.split(input[3]);
    link if is_used ~> (output_low[4], output_high[4]) = split_GL.split(input[4]);
    link if is_used ~> (output_low[5], output_high[5]) = split_GL.split(input[5]);
    link if is_used ~> (output_low[6], output_high[6]) = split_GL.split(input[6]);
    link if is_used ~> (output_low[7], output_high[7]) = split_GL.split(input[7]);

    // TODO: turn this into array operations
    link if is_used ~> mem.mstore(output_addr + 0, time_step + 1, output_low[0]);
    link if is_used ~> mem.mstore(output_addr + 4, time_step + 1, output_high[0]);

    link if is_used ~> mem.mstore(output_addr + 8, time_step + 1, output_low[1]);
    link if is_used ~> mem.mstore(output_addr + 12, time_step + 1, output_high[1]);

    link if is_used ~> mem.mstore(output_addr + 16, time_step + 1, output_low[2]);
    link if is_used ~> mem.mstore(output_addr + 20, time_step + 1, output_high[2]);

    link if is_used ~> mem.mstore(output_addr + 24, time_step + 1, output_low[3]);
    link if is_used ~> mem.mstore(output_addr + 28, time_step + 1, output_high[3]);

    link if is_used ~> mem.mstore(output_addr + 32, time_step + 1, output_low[4]);
    link if is_used ~> mem.mstore(output_addr + 36, time_step + 1, output_high[4]);

    link if is_used ~> mem.mstore(output_addr + 40, time_step + 1, output_low[5]);
    link if is_used ~> mem.mstore(output_addr + 44, time_step + 1, output_high[5]);

    link if is_used ~> mem.mstore(output_addr + 48, time_step + 1, output_low[6]);
    link if is_used ~> mem.mstore(output_addr + 52, time_step + 1, output_high[6]);

    link if is_used ~> mem.mstore(output_addr + 56, time_step + 1, output_low[7]);
    link if is_used ~> mem.mstore(output_addr + 60, time_step + 1, output_high[7]);

}
