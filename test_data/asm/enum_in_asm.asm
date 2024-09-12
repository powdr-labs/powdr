mod types {
    enum OptionInt {
        None,
        Some(int, x::Other)
    }
    mod x {
        enum Other {
            A(int)
        }
    }
}

mod utils {
    use super::types::OptionInt;
    use super::types::x;
    let o: OptionInt = OptionInt::Some(42, x::Other::A(2));
}

machine Empty with degree: 4 {
    col witness w;
    w = w * w;
}