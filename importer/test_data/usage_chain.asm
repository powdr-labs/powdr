machine Bar {
}
mod b {
    use super::Bar;
}
mod a {
    use super::b as b;
    machine M {
        b::Bar bar;
    }
}
