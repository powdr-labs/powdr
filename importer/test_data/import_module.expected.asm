mod submodule {
    machine Foo {
    }
}
machine Foo {
    ::submodule::Foo foo;
}
