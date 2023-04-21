#![no_std]

use core::arch::asm;

#[no_mangle]
pub extern "C" fn main() -> ! {
    let sum = 1u32;
    //    let proposed_sum = get_prover_input(0);

    let mut buf = [0u8; 10];
    let _s: &str = write_to::show(
        &mut buf,
        format_args!("X"), //: {sum}, proposed: {proposed_sum}"),
    )
    .unwrap();
    print_prover(_s);
    loop {}
}

pub mod write_to {
    use crate::print_prover;
    use core::cmp::min;
    use core::fmt;

    pub struct WriteTo<'a> {
        buffer: &'a mut [u8],
        // on write error (i.e. not enough space in buffer) this grows beyond
        // `buffer.len()`.
        used: usize,
    }

    impl<'a> WriteTo<'a> {
        pub fn new(buffer: &'a mut [u8]) -> Self {
            WriteTo { buffer, used: 0 }
        }

        pub fn as_str(self) -> Option<&'a str> {
            if self.used <= self.buffer.len() {
                // only successful concats of str - must be a valid str.
                use core::str::from_utf8_unchecked;
                Some(unsafe { from_utf8_unchecked(&self.buffer[..self.used]) })
            } else {
                None
            }
        }
    }

    impl<'a> fmt::Write for WriteTo<'a> {
        fn write_str(&mut self, s: &str) -> fmt::Result {
            Ok(())
        }
    }

    pub fn show<'a>(buffer: &'a mut [u8], args: fmt::Arguments) -> Result<&'a str, fmt::Error> {
        let mut w = WriteTo::new(buffer);
        print_prover("Created");
        fmt::write(&mut w, args)?;
        Ok("")
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
