use std::machines::memory::Memory;

// TODO: placeholder for Rust wrapper
machine KeccakF(mem: Memory) with
    latch: LATCH,
    call_selectors: sel,
{ 
    let x;
    let y;
    pol commit STEP;
    operation keccakf x, y, STEP ->;
}
