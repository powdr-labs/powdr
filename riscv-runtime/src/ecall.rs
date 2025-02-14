/// Generates an ecall instruction with the given system call number and arguments.
///
/// Uses the instruction sequence convention for the system call to be inlined.
#[macro_export]
macro_rules! ecall {
    ($syscall:expr, $($tokens:tt)*) => {
        asm!(
            "addi t0, x0, {}",
            "ecall",
            const $syscall as u8,
            // No system call we have at this point allocates on stack.
            options(nostack),
            // Inform the compiler that the t0 was overwritten.
            out("t0") _,
            $($tokens)*
        );
    };
}
