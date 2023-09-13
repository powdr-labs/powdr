use submodule::subbbb::Foo as Foo;
mod submodule {
    use subbbb::Foo as Foo;
    mod subbbb {
        machine Foo {
        }
    }
}
