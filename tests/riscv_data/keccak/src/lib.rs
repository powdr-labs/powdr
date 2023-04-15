#![no_std]
use core::panic::PanicInfo;

use tiny_keccak::{Hasher, Keccak};

#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    loop {}
}

#[no_mangle]
pub extern "C" fn main() -> ! {
    let input = b"Solidity";
    let mut output = [0u8; 32];
    let mut hasher = Keccak::v256();
    //hasher.update(input);
    hasher.finalize(&mut output);
    //    println!("{output:x?}");
    loop {}
}
