machine Bar {
    Foo1 a;
    submodule::Foo0 b;
    submodule::subbbb::Foo c;
}
use submodule::Foo0 as Foo1;
mod submodule {
    use subbbb::Foo as Foo0;
    mod subbbb {
        machine Foo {

        }
    }
}
