mod bar {
    machine Bar {
    }
}
use bar::Bar as LocalBar;
machine Foo {
    LocalBar foo;
}
