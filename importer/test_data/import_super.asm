machine Machine {
}
mod submodule {
    mod subbbb {
        use super::super::Machine as Machine0;
        machine M {
            Machine0 m;
        }
    }
}
