use stwo_prover::core::fields::m31::{BaseField, M31};
use stwo_prover::core::channel::{Blake2sChannel,Poseidon252Channel};
use stwo_prover::core::channel::Channel;
use stwo_prover::core::fields::m31::N_BYTES_FELT;

//blaske2sChannel
pub const BLAKE_BYTES_PER_HASH: usize = 32;
pub const FELTS_PER_HASH: usize = 8;

//Poseidon252Channel

//there are two options to make this work:
//1. make the draw_base_felt a trait method among Blaske2sChannel and Poseidon252Channel,
//   the problem is therie draw_base_felt method is private, so I need to implement them here
//2. use the functions to draw from secure field, then convert to base field somehow.
// trying the first method now.
pub trait BaseFieldChallenger{
    // pub const fn digest(&self) -> Blake2sHash {
    //     self.digest
    // }
    // pub fn update_digest(&mut self, new_digest: Blake2sHash) {
    //     self.digest = new_digest;
    //     self.channel_time.inc_challenges();
    // }
    /// Generates a uniform random vector of BaseField elements.
    fn draw_base_felt(&mut self) -> BaseField;
}

impl BaseFieldChallenger for Blake2sChannel {
    fn draw_base_felt(&mut self) -> [BaseField; FELTS_PER_HASH] {
         // Repeats hashing with an increasing counter until getting a good result.
        // Retry probability for each round is ~ 2^(-28).
        loop {
            let u32s: [u32; FELTS_PER_HASH] = self
                .draw_random_bytes()
                .chunks_exact(N_BYTES_FELT) // 4 bytes per u32.
                .map(|chunk| u32::from_le_bytes(chunk.try_into().unwrap()))
                .collect::<Vec<_>>()
                .try_into()
                .unwrap();

            // Retry if not all the u32 are in the range [0, 2P).
            if u32s.iter().all(|x| *x < 2 * P) {
                return u32s
                    .into_iter()
                    .map(|x| BaseField::reduce(x as u64))
                    .collect::<Vec<_>>()
                    .try_into()
                    .unwrap();
            }
        }
    
    }
    
}

impl BaseFieldChallenger for  Poseidon252Channel {
    fn draw_base_felt(&mut self) -> BaseField {
        unimplemented!()
    }
    
}