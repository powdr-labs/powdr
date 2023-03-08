pub trait WitnessColumnNamer {
    fn name(&self, i: usize) -> &str;
}
