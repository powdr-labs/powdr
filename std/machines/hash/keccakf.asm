// TODO: placeholder for Rust wrapper
machine KeccakF with
    latch: LATCH,
{ 
    let x;
    let y;
    pol commit STEP;
    operation keccakf x, y, STEP ->;
}
