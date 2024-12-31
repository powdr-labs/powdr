# Powdr linker script.
#
# If you are using powdr-riscv-runtime, it expects the following
# symbols to be defined:
#   __global_pointer$
#   __powdr_stack_start
#   __powdr_prover_data_init
#   __powdr_prover_data_end
#
# Where "__powdr_prover_data_init" and "__powdr_prover_data_end"
# defines a region bigger than 1 page (2 KB), whose size is a
# power of 2, and aligned to its size.
#
# This linker script provides usable definitions to these
# symbols, with a stack of almost 256 MB. If you are not building
# via powdr-rs, you must manually specify "-C link-arg=-Tpowdr.x"
# in rustc to use this linker script (e.g. via RUSTFLAGS).

SECTIONS
{
  # Stack starts backwards from here (one 2 KB page short of 256 MB)
  __powdr_stack_start = 0x10000000 - 0x800;

  # The prover data (the second 256 MB chunk of the address space)
  __powdr_prover_data_start = 0x10000000;
  __powdr_prover_data_end = 0x20000000;

  # Data starts here, one page after the prover data.
  . = 0x20000000 + 0x800;
  .data : {
    *(.data)
    PROVIDE( __global_pointer$ = . + 0x800 );
  }
  .bss : { *(.bss) }

  # Text addresses are fake in powdr, we use a different address space.
  .text : { *(.text) }
}

# Specify the entry point function provided by powdr-riscv-runtime:
ASSERT(DEFINED(__runtime_start), "Error: __runtime_start is not defined. Try adding the line `extern crate powdr_riscv_runtime;` in your `main.rs`.")
ENTRY(__runtime_start)
