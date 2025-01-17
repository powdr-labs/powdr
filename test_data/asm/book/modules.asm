use my_module::Other as LocalOther;

// we can define a module at `./submodule.asm`
mod submodule;

// we can define a module at `./submodule_in_folder/mod.asm`
mod submodule_in_folder;

use submodule::Other as SubmoduleOther;
use submodule_in_folder::Other as FolderSubmoduleOther;

let zero: int = 0;

// we can also define modules inline
mod utils {
    // Each module has a fresh symbol list. Every external symbol needs to be imported,
    // even from the parent module.
    use super::zero;

    let one = zero + 1;
}

machine Main with degree: 8 {
    // use a machine from another module by relative path
    my_module::Other a;

    // use a machine from another module using a local binding
    LocalOther b;

    // use a machine from another module defined in a different file
    SubmoduleOther c;

    // use a machine from another module defined in a different directory
    FolderSubmoduleOther d;

    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg A;

    instr identity X -> Y link => Y = a.identity(X);
    instr also_identity X -> Y link => Y = a.identity(X);
    instr still_identity X -> Y link => Y = a.identity(X);
    instr identity_again X -> Y link => Y = a.identity(X);

    function main {
        A <== identity(A);
        A <== also_identity(A);
        A <== still_identity(A);
        A <== identity_again(A);
        return;
    }
}

mod my_module {
    machine Other with
        degree: 8,
        latch: latch,
        operation_id: operation_id
    {
        operation identity<0> x -> y;

        col witness x;
        col witness y;
        x = y;
        col fixed latch = [1]*;
        col fixed operation_id = [0]*;
    }
}
