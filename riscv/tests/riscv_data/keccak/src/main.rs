#![no_main]
#![no_std]

extern crate powdr_riscv_runtime;
use tiny_keccak::{Hasher, Keccak, keccakf};

#[no_mangle]
pub fn main() {
    let mut inputs: [u64; 25] = [0; 25];
    keccakf(&mut inputs);
    let output_tiny = inputs.clone();

    assert_eq!(
        output_tiny,
        [17376452488221285863, 9571781953733019530, 15391093639620504046, 13624874521033984333, 10027350355371872343, 18417369716475457492, 10448040663659726788, 10113917136857017974, 12479658147685402012, 3500241080921619556, 16959053435453822517, 12224711289652453635, 9342009439668884831, 4879704952849025062, 140226327413610143, 424854978622500449, 7259519967065370866, 7004910057750291985, 13293599522548616907, 10105770293752443592, 10668034807192757780, 1747952066141424100, 1654286879329379778, 8500057116360352059, 16929593379567477321]
    );

    let mut state_u32: [u32; 50] = [0; 50];
    let mut output_u32: [u32; 50] = [0; 50];

    for (i, chunk) in inputs.iter().enumerate() {
        let lower = (*chunk & 0xFFFFFFFF) as u32;
        let upper = ((*chunk >> 32) & 0xFFFFFFFF) as u32;

        state_u32[i * 2] = upper.to_le();
        state_u32[i * 2 + 1] = lower.to_le();
    }

    let input_u32: &[u32; 50] = unsafe { &*(&inputs as *const [u64; 25] as *const [u32; 50]) };
    powdr_riscv_runtime::hash::keccakf(&mut state_u32, &mut output_u32);
    //powdr_riscv_runtime::hash::keccakf(&input_u32, &mut output_u32);

    //let output_u64: &mut [u64; 25] = unsafe {
    //    &mut *(output_u32.as_mut_ptr() as *mut [u64; 25])
    //};
    let mut output_u64: [u64; 25] = [0; 25];

    for (i, chunk) in output_u64.iter_mut().enumerate() {
        let upper = u64::from_le(output_u32[i * 2] as u64);
        let lower = u64::from_le(output_u32[i * 2 + 1] as u64) << 32;

        *chunk = upper | lower;
    }

    assert_eq!(output_u64, output_tiny);

    /*
    let inputs = [b"Solidity", b"Powdrrrr"];
    let mut output = [0u8; 32];
    let mut hasher = Keccak::v256();
    //for input in inputs.into_iter().cycle().take(100) {
    //    hasher.update(input);
    //}
    hasher.update(&[0]);
    hasher.finalize(&mut output);

    assert_eq!(
        output,
        [188, 54, 120, 158, 122, 30, 40, 20, 54, 70, 66, 41, 130, 143, 129, 125, 102, 18, 247, 180, 119, 214, 101, 145, 255, 150, 169, 224, 100, 188, 201, 138]
    );
    */

    /*
    assert_eq!(
        output,
        [
            0xb2, 0x60, 0x1c, 0x72, 0x12, 0xd8, 0x26, 0x0d, 0xa4, 0x6d, 0xde, 0x19, 0x8d, 0x50,
            0xa7, 0xe4, 0x67, 0x1f, 0xc1, 0xbb, 0x8f, 0xf2, 0xd1, 0x72, 0x5a, 0x8d, 0xa1, 0x08,
            0x11, 0xb5, 0x81, 0x69
        ],
    );
    */
}
