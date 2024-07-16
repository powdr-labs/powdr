// This tests whether side effects are counted as "progress" when processing identities.
// The MemoryProxy writes values to memory. Processing the link to memory does not result
// in any updates to the MemoryProxy witness, but it does have a side effect (adding an
// entry to the memory machine). Because of this, processing the link should not be skipped.

use std::machines::memory::Memory;

mod test_util;
use test_util::FakeByte2 as Byte2;

let N: int = 256;

machine MemoryProxy with
    latch: latch,
    operation_id: operation_id,
    call_selectors: sel,
    degree: N
{
    operation mstore<0> addr, step, value ->;

    let operation_id;
    col fixed latch = [1]*;

    Byte2 byte2;
    Memory mem(byte2);

    col witness addr, step, value;
    
    col witness used;
    used = std::array::sum(sel);
    std::utils::force_bool(used);

    link if used ~> mem.mstore(addr, step, value);
}

machine Main with degree: N {
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];

    col fixed STEP(i) { i };
    MemoryProxy mem;
    instr mstore X, Y -> link ~> mem.mstore(X, STEP, Y);

    function main {
        
        mstore 1, 1;
        mstore 2, 2;
        mstore 3, 3;
        mstore 4, 4;

        return;
    }
}
