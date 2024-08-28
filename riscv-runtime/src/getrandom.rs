use getrandom::{register_custom_getrandom, Error};

/// The de-facto standard rust interface to low level random number generation.
fn powdr_getrandom(buf: &mut [u8]) -> Result<(), Error> {
    crate::entropy_source::getrandom(buf);
    Ok(())
}
register_custom_getrandom!(powdr_getrandom);
