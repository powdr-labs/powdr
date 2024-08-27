# Powdr linker script.
#
# If you are using powdr-riscv-runtime, it expects the symbols
# "__global_pointer$" and "__powdr_stack_start" to be defined.
#
# This linker script provides usable definitions to these
# symbols, with a 256 MB stack. If you are not building via
# powdr-rs, you must manually specify "-C link-arg=-Tpowdr.x"
# in rustc to use this linker script (e.g. via RUSTFLAGS).

SECTIONS
{
  # Data starts here, before is the stack.
  . = 0x10000100;
  .data : {
    *(.data)
    PROVIDE( __global_pointer$ = . + 0x800 );
  }
  .bss : { *(.bss) }

  # Text addresses are fake in powdr, we use a different address space.
  .text : { *(.text) }

  __powdr_stack_start = 0x10000000;
}

# Specify the entry point function provided by powdr-riscv-runtime:
ASSERT(DEFINED(__runtime_start), "Error: __runtime_start is not defined. Try adding the line `extern crate powdr_riscv_runtime;` in your `main.rs`.")
ENTRY(__runtime_start)
