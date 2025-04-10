Tests from https://github.com/riscv-software-src/riscv-tests/tree/master/isa

Powdr partially implements riscv32imac userspace ISA. One major difference is
that the code in our zkVM is interpreted at a higher abstraction level than the
binary representation of a RISC-V "text" section, and we use code labels as
absolute references, not relative to the PC like in binary format RISC-V. Thus
labels in code sections are treated as opaque values, and we don't support any
kind of arithmetic over `.text` label and addresses, nor alignment or spacing
directives in `.text` sections. Most unsupported instructions are related to
this limitation.

As a consequence, we have limited support for the instructions `auipc` and
`lui`: they can only be used to refer to a `.text` label if they can be fused
with the next instruction, making use of the loaded address immediately (e.g.
`addi` or `jalr`). Furthermore, the register used to store the high bits of the
text address will not be modified, as required by a conformant implementation!
For instance, the `tail` pseudoinstruction, which expands to `auipc` + `jarl`,
is supposed to leave the high bits of the return address in `x6`. This does not
happen in Powdr!

Following there is a list of tests from the test suite that we do not support:

## From the basic instruction set (rv32ui):

- `auipc`: this test is not supported because we don't support any kind of arithmetic over
`.text` label and addresses, nor the `lla` pseudoinstruction;

- `fence_i`: this test is not supported because our zkVM "text" is static: we don't support
dynamic binary code or self modifying programs, so we don't support instruction `fence.i`;

- `jalr`: our support for `jalr` instruction is partial: either offset must be zero,
or the target register `rs` must be `x0`. This is because we don't support arithmetic
over text labels, as they are opaque;

- `lui`: instruction `sra` not yet implemented;

- `ma_data`: we don't yet support misaligned data access;

- `sra`: not yet implemented.

## From the "M" (multiplication) extension (rv32um):

- `div`
- `rem`

These instructions are not yet implemented.

## From the "A" (atomic) extension (rv32ua):

- `amoand_w`
- `amomax_w`
- `amomaxu_w`
- `amomin_w`
- `amominu_w`
- `amoor_w`
- `amoswap_w`
- `amoxor_w`

We do not (yet) support the instructions of these tests, but should be easy to
implement, following `amoadd_w` suit.

## From the "C" (compressed) extension (rv32uc):

- `rvc`

The RISC-V "C" test file is only partially supported when converting from the
ELF executable. Some tests that we don't support were disabled.
