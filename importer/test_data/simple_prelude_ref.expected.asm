mod std {
    mod prelude {
        let x: int = 1;
    }
}
mod module {
    let y = std::prelude::x;
}
