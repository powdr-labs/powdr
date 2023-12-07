mod bar {
    machine Bar {
    }
}
machine Foo {
    ::bar::Bar foo;
}
