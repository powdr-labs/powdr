mod std {
    mod prelude {
        let x: int = 1;
    }
}

let y = module::x;

mod module {
}