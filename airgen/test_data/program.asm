machine Bar {

}

machine Foo {
    Bar bar;
}

machine Baz {
    Foo foo;
    Bar bar;
}

// let bar = Bar {};
// "::bar" -> "bar"
// "bar" -> Bar()

// let foo = Foo { bar };
// "::foo" -> "foo"
// "foo" -> Foo("bar")

// let main = Baz { foo, bar };
// "::main" -> "main"
// "main" -> Baz("foo", "bar")

// let main2 = Baz { foo: Foo { bar: Bar {} }, bar }
// "::main2" -> "main2"
// "main2_foo_bar" -> Bar()
// "main2_foo" -> Foo("main2_foo_bar")
// "main2" -> Baz("main2_foo")

// let bar2 = bar
// "::bar2" -> "bar"

// let bar3 = foo.bar;
// "::bar3" -> "bar"

// let foo2 = Foo { bar: foo.bar }
// "::foo2" -> "foo2"
// "foo2" -> Foo("bar")

//struct Instances {
//    abs_to_loc: (path -> loc),
//    map: (loc -> (ty, loc[]))
//}