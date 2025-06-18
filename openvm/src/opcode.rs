/// Defines each opcode as a `pub const u32` and also generates
/// a `pub const ALL_OPCODES: &[u32]` containing all of them.
macro_rules! define_opcodes {
  ( $(
      $name:ident = $val:expr,
  )* ) => {
      $(
          pub const $name: u32 = $val;
      )*

      /// A slice containing *all* opcode constants.
      pub const ALL_OPCODES: &[u32] = &[
          $( $name ),*
      ];
  }
}

define_opcodes!(
    // Rv32BaseAluChip
    OPCODE_ADD = 512,
    OPCODE_SUB = 513,
    OPCODE_XOR = 514,
    OPCODE_OR = 515,
    OPCODE_AND = 516,
    // Rv32ShiftChip opcodes
    OPCODE_SLL = 517,
    OPCODE_SRL = 518,
    OPCODE_SRA = 519,
    // Rv32LessThanChip opcodes
    OPCODE_SLT = 520,
    OPCODE_SLTU = 521,
    // Load/Store opcodes
    OPCODE_LOADW = 528,
    OPCODE_LOADBU = 529,
    OPCODE_LOADHU = 530,
    OPCODE_STOREW = 531,
    OPCODE_STOREH = 532,
    OPCODE_STOREB = 533,
    OPCODE_LOADB = 534,
    OPCODE_LOADH = 535,
    // Other opcodes
    OPCODE_BEQ = 544,
    OPCODE_BNE = 545,
    OPCODE_BLT = 549,
    OPCODE_BLTU = 550,
    OPCODE_BGE = 551,
    OPCODE_BGEU = 552,
    OPCODE_JAL = 560,
    OPCODE_LUI = 561,
    OPCODE_JALR = 565,
    OPCODE_AUIPC = 576,
    OPCODE_MUL = 592,
    OPCODE_MULH = 593,
    OPCODE_MULHSU = 594,
    OPCODE_MULHU = 595,
    OPCODE_DIV = 596,
    OPCODE_DIVU = 597,
    OPCODE_REM = 598,
    OPCODE_REMU = 599,
    OPCODE_HINT_STOREW = 608,
    OPCODE_HINT_BUFFER = 609,
);
