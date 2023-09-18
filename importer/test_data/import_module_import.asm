use submodule::alias::Foo as Foo;
mod submodule {
    use subbbb as alias;
    mod subbbb {
        machine Foo {
        }
    }
}
