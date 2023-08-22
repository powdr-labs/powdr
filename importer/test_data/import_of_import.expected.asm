machine Bar {
    submodule::subbbb::Foo a;
    submodule::subbbb::Foo b;
    submodule::subbbb::Foo c;
}
mod submodule {
    mod subbbb {
        machine Foo {
        }
    }
}
