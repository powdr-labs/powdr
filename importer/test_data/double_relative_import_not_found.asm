use submodule::subbbb::Foo as Foo1;
mod submodule {
    use subbbb::Foo as Foo0;
    mod subbbb {
    }
}
