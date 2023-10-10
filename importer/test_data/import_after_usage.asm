machine Foo {
}

mod module {
    machine Bar {
        Foo foo;
    }
    use super::Foo;
}