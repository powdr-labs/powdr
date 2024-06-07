# Powdr linker script.
#
# If you are using powdr-riscv-runtime, it expects the symbols
# "__global_pointer$" and "__powdr_stack_start" to be defined.
#
# This is an usable version with a 256 MB stack. If you are not building via powdr-rs,
# to use this linker script you must manually specify "-C link-arg=-Tpowdr.x" in rustc
# (e.g. via RUSTFLAGS).

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

ENTRY(__runtime_start)
