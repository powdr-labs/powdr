use core::arch::asm;
use core::fmt;

// #[macro_export]
// macro_rules! print {
//     ($($arg:tt)+) => (print_args(format_args!( $($arg)+)))
// }

// TODO turn this into a macro
pub fn print(args: fmt::Arguments) {
    let mut buf = [0u8; 1024];
    let _s: &str = buf_formatter::format(&mut buf, args).unwrap();
    print_prover(_s);
}

#[inline]
fn print_prover(s: &str) {
    for b in s.bytes() {
        print_prover_char(b)
    }
}

#[inline]
fn print_prover_char(c: u8) {
    let mut value = c as u32;
    #[allow(unused_assignments)]
    unsafe {
        asm!("ebreak", lateout("a0") value, in("a0") value);
    }
}

mod buf_formatter {
    use core::cmp::min;
    use core::fmt;

    pub struct BufFormatter<'a> {
        buffer: &'a mut [u8],
        used: usize,
    }

    impl<'a> BufFormatter<'a> {
        pub fn new(buffer: &'a mut [u8]) -> Self {
            Self { buffer, used: 0 }
        }

        pub fn as_str(self) -> Option<&'a str> {
            if self.used <= self.buffer.len() {
                // we only concatenate str, so the result must be valid utf8 as well.
                Some(unsafe { core::str::from_utf8_unchecked(&self.buffer[..self.used]) })
            } else {
                None
            }
        }
    }

    impl<'a> fmt::Write for BufFormatter<'a> {
        fn write_str(&mut self, s: &str) -> fmt::Result {
            if self.used > self.buffer.len() {
                return Err(fmt::Error);
            }
            let remaining_buf = &mut self.buffer[self.used..];
            let raw_s = s.as_bytes();
            let write_num = min(raw_s.len(), remaining_buf.len());
            remaining_buf[..write_num].copy_from_slice(&raw_s[..write_num]);
            self.used += raw_s.len();
            if write_num < raw_s.len() {
                Err(fmt::Error)
            } else {
                Ok(())
            }
        }
    }

    pub fn format<'a>(buffer: &'a mut [u8], args: fmt::Arguments) -> Result<&'a str, fmt::Error> {
        let mut w = BufFormatter::new(buffer);
        fmt::write(&mut w, args)?;
        w.as_str().ok_or(fmt::Error)
    }
}
