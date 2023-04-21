#![no_std]

use core::arch::asm;

#[no_mangle]
pub extern "C" fn main() -> ! {
    let _s: &str = write_to::show(format_args!("X")).unwrap();
    print_prover(_s);
    loop {}
}

pub mod write_to {
    use crate::print_prover;
    use core::cmp::min;
    use core::fmt;

    pub struct WriteTo {}

    impl fmt::Write for WriteTo {
        fn write_str(&mut self, s: &str) -> fmt::Result {
            Ok(())
        }
    }

    pub fn show(args: fmt::Arguments) -> Result<&'static str, fmt::Error> {
        let mut w = WriteTo {};
        print_prover("Created");
        fmt::write(&mut w, args).unwrap();
        loop {}
    }
}

#[inline]
fn get_prover_input(index: u32) -> u32 {
    let mut value: u32;
    unsafe {
        asm!("ecall", lateout("a0") value, in("a0") index);
    }
    value
}

#[inline]
fn print_prover(s: &str) {
    for b in s.bytes() {
        let mut value = b as u32;
        unsafe {
            asm!("ebreak", lateout("a0") value, in("a0") value);
        }
    }
}
