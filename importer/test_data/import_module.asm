use submodule as alias;
mod submodule {
    machine Foo {
    }
}
machine Foo {
    alias::Foo foo;
}
