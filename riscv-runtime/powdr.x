# Powdr linker script.
#
# A segment of type LOOS+0xda (0x600000da) is understood by powdr, and it
# delimits an unconstrained memory region that can be used to pass data
# to the ZK program. If present, this region must be bigger than
# one page (2 KB), have a power of 2 size, and be aligned to its own size.
#
# This linker script defines the user data segment to be the second 256 MB
# chunk of the address space.
#
# If you are using powdr-riscv-runtime, it also expects the following
# symbols to be defined:
#   __global_pointer$
#   __powdr_stack_start
#   __powdr_prover_data_init
#   __powdr_prover_data_end
#
# Where "__powdr_prover_data_init" and "__powdr_prover_data_end"
# defines the bounds of the prover data segment.
#
# This linker script provides usable definitions to these
# symbols, with a stack of almost 256 MB. If you are not building
# via powdr-rs, you must manually specify "-C link-arg=-Tpowdr.x"
# in rustc to use this linker script (e.g. via RUSTFLAGS).

PHDRS {
  # The powdr specific p_type 0x600000da indicates to
  # powdr ELF parser where the prover data is.
  powdr_prover_data_seg 0x600000da;

  # text is R (4) + X (1)
  text_seg PT_LOAD FLAGS(5);

  # data and bss segments are R (4) + W (2)
  # there is no point in having a separated rodata segment because powdr doesn't enforce permissions
  data_seg PT_LOAD FLAGS(6);
  bss_seg PT_LOAD FLAGS(6);
}

SECTIONS
{
  # Stack starts backwards from here (one 2 KB page short of 256 MB)
  PROVIDE( __powdr_stack_start = 0x10000000 - 0x800 );

  # The prover data section bounds (the second 256 MB chunk of the address space)
  . = 0x10000000;
  .powdr.prover.data (NOLOAD) : {
    PROVIDE( __powdr_prover_data_start = . );
    . += 0x10000000;
    PROVIDE( __powdr_prover_data_end = . );
  } :powdr_prover_data_seg

  # Place the rest of the segments one page after the prover data.
  . += 0x800;

  # Text addresses are fake in powdr, we use a different address space.
  .text : ALIGN(4) { *(.text) } :text_seg

  .bss : { *(.sbss .bss .bss.*) } :bss_seg

  .rodata : ALIGN(4) {
    PROVIDE( __global_pointer$ = . + 0x800 );
    *(.rodata .rodata.* )
  } :data_seg

  .data : {
    *(.data .data.* )
  } :data_seg
}

# Specify the entry point function provided by powdr-riscv-runtime:
ASSERT(DEFINED(__runtime_start), "Error: __runtime_start is not defined. Try adding the line `extern crate powdr_riscv_runtime;` in your `main.rs`.")
ENTRY(__runtime_start)
