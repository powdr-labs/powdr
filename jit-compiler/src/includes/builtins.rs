static DEGREE: std::sync::RwLock<Option<ibig::IBig>> = std::sync::RwLock::new(None);

#[no_mangle]
pub extern "C" fn __set_degree(degree: u64) {
    *DEGREE.write().unwrap() = Some(ibig::IBig::from(degree));
}
