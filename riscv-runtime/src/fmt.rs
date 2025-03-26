use core::arch::asm;
use core::fmt;

use powdr_syscalls::Syscall;

#[macro_export]
macro_rules! print {
    ($($arg:tt)+) => {{
        $crate::fmt::print_args(format_args!( $($arg)+));
    }};
}

pub fn print_args(args: fmt::Arguments) {
    fmt::write(&mut ProverWriter {}, args).unwrap();
}

struct ProverWriter {}

impl fmt::Write for ProverWriter {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        print_str(s);
        Ok(())
    }
}

pub fn print_str(s: &str) {
    // DEAR DEV, please don't allow this function to panic.
    //
    // This is called from the panic handler.
    for b in s.bytes() {
        print_prover_char(b)
    }
}

#[inline]
fn print_prover_char(c: u8) {
    let mut value = c as u32;
    #[allow(unused_assignments)]
    unsafe {
        ecall!(Syscall::Output, lateout("a0") value, in("a0") 0, in("a1") value);
    }
}
