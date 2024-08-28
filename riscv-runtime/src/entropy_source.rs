/// This is a placeholder to pretend to provide a random number generator, for
/// places like the hash function of HashMap who needs something.
///
/// This could be improved by using the rand_chacha crate, but I think it is
/// worse, as it will just mask the fact that we are not providing a real
/// entropy source.
///
/// TODO: figure how to be truly random
#[cfg(feature = "allow_fake_rand")]
pub(crate) fn getrandom(s: &mut [u8]) {
    const VALUE: u8 = 3;
    s.iter_mut().for_each(|v| *v = VALUE);
}

#[cfg(not(feature = "allow_fake_rand"))]
pub(crate) fn getrandom(_: &mut [u8]) {
    panic!("there is no real entropy source in Powdr");
}
