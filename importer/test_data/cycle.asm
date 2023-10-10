use module::Machine;

mod module {
    use super::other_module::submodule::MyMachine as Machine;
}

mod other_module {
    mod submodule {
        use super::super::Machine as MyMachine;
    }
}