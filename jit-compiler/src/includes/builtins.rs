static DEGREE: std::sync::RwLock<Option<ibig::IBig>> = std::sync::RwLock::new(None);

#[no_mangle]
pub extern "C" fn __set_degree(degree: u64) {
    *DEGREE.write().unwrap() = Some(ibig::IBig::from(degree));
}

fn input_from_channel(channel: ibig::IBig, index: ibig::IBig) -> FieldElement {
    // TODO
    18.into()
}

fn output_to_channel(channel: ibig::IBig, value: FieldElement) {
    // TODO
    print!("{}", IntType::from(value) as u8 as char);
}
