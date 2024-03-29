Tests from https://github.com/riscv/riscv-tests/tree/master/isa/

Powdr partially implements riscv32imac userspace ISA. One major difference is
that the code in our zkVM is interpreted at a higher abstraction level than the
binary representation of a RISC-V "text" section, and we use code labels as
absolute references, not relative to the PC like in binary format RISC-V. Thus
labels in code sections are treated as opaque values, and we don't support any
kind of arithmetic over `.text` label and addresses, nor alignment or spacing
directives in `.text` sections. Most unsupported instructions are related to
this limitation.

Following there is a list of tests from the test suite that we do not support:

## From the basic instruction set (rv32ui):

- auipc

This test is not supported because we don't support any kind of arithmetic over
`.text` label and addresses, nor the `lla` pseudoinstruction.

- fence_i

This test is not supported because our zkVM "text" is static: we don't support
dynamic binary code or self modifying programs. Thus, our `fence.i` instruction
is just a nop.

- jalr

Tries to load the address of a `.text` label, which we don't support.

- lui

Instruction `sra` not yet implemented.

- ma_data

We don't yet support misaligned data access.

- sra

Not yet implemented.

## From the "M" (multiplication) extension (rv32um):

- div
- rem

These instructions are not yet implemented.

## From the "A" (atomic) extension (rv32ua):

- amoand_w
- amomax_w
- amomaxu_w
- amomin_w
- amominu_w
- amoor_w
- amoswap_w
- amoxor_w

We do not (yet) support the instructions of these tests, but should be easy to
implement, following amoadd_w suit.

## From the "C" (compressed) extension (rv32uc):

- rvc

This RISC-V "C" extension doesn't make much sense for Powdr, as we execute
directly the assembly file, not the binary format. All the alignment and precise
spacing of the `.text` section in this test doesn't make sense for Powdr.

We just compile to riscv32imac instead of riscv32ima because the later is not
supported by rust.
