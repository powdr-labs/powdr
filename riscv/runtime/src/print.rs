use core::arch::asm;
use core::fmt;
use core::mem::MaybeUninit;

// #[macro_export]
// macro_rules! print {
//     ($($arg:tt)+) => (print_args(format_args!( $($arg)+)))
// }

// TODO turn this into a macro
pub fn print(args: fmt::Arguments) {
    const BUF_SIZE: usize = 1024;
    let mut buf = unsafe { MaybeUninit::<[MaybeUninit<u8>; BUF_SIZE]>::uninit().assume_init() };
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
    use core::fmt;
    use core::mem::MaybeUninit;

    pub struct BufFormatter<'a> {
        buffer: &'a mut [MaybeUninit<u8>],
        used: usize,
    }

    impl<'a> BufFormatter<'a> {
        pub fn new(buffer: &'a mut [MaybeUninit<u8>]) -> Self {
            Self { buffer, used: 0 }
        }

        pub fn as_str(&self) -> &'a str {
            unsafe {
                // This is safe because everything until self.used has been initialized.
                let buf =
                    &*(&self.buffer[..self.used] as *const [MaybeUninit<u8>] as *const [u8]);
                // we only concatenate str, so the result must be valid utf8 as well.
                core::str::from_utf8_unchecked(buf)
            }
        }
    }

    impl<'a> fmt::Write for BufFormatter<'a> {
        fn write_str(&mut self, s: &str) -> fmt::Result {
            let raw_s = s.as_bytes();
            let write_len = raw_s.len();
            if write_len > self.buffer.len() - self.used {
                return Err(fmt::Error);
            }
            let s_uninit: &[MaybeUninit<u8>] = unsafe { core::mem::transmute(&raw_s[..write_len]) };
            self.buffer[self.used..][..write_len].copy_from_slice(s_uninit);
            self.used += write_len;
            Ok(())
        }
    }

    pub fn format<'a>(
        buffer: &'a mut [MaybeUninit<u8>],
        args: fmt::Arguments,
    ) -> Result<&'a str, fmt::Error> {
        let mut w = BufFormatter::new(buffer);
        fmt::write(&mut w, args)?;
        Ok(w.as_str())
    }
}
