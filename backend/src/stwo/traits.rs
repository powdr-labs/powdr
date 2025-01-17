use stwo_prover::core::channel::FELTS_PER_HASH;
pub trait BaseFieldChallenger{
    // pub const fn digest(&self) -> Blake2sHash {
    //     self.digest
    // }
    // pub fn update_digest(&mut self, new_digest: Blake2sHash) {
    //     self.digest = new_digest;
    //     self.channel_time.inc_challenges();
    // }
    /// Generates a uniform random vector of BaseField elements.
    fn draw_base_felts(&mut self) -> [BaseField; FELTS_PER_HASH] {
    }
}