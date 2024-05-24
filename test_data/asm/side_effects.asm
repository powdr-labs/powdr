// This tests whether side effects are counted as "progress" when processing identities.
// The MemoryProxy writes values to memory. Processing the link to memory does not result
// in any updates to the MemoryProxy witness, but it does have a side effect (adding an
// entry to the memory machine). Because of this, processing the link should not be skipped.

use std::machines::memory::Memory;

machine MemoryProxy with
    latch: latch,
    operation_id: operation_id,
    call_selectors: sel,
{
    operation mstore<0> addr, step, value ->;

    col witness operation_id;
    col fixed latch = [1]*;

    Memory mem;

    col witness addr, step, value;
    
    col witness used;
    used = std::array::sum(sel);
    std::utils::force_bool(used);

    link used ~> mem.mstore addr, step, value ->;
}

machine Main with degree: 1024 {
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];

    col fixed STEP(i) { i };
    MemoryProxy mem;
    instr mstore X, Y -> ~ mem.mstore X, STEP, Y ->;

    function main {
        
        mstore 1, 1;
        mstore 2, 2;
        mstore 3, 3;
        mstore 4, 4;

        return;
    }
}
