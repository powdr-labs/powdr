// Rv32BaseAluChip
pub const OPCODE_ADD: u32 = 512;
pub const OPCODE_SUB: u32 = 513;
pub const OPCODE_XOR: u32 = 514;
pub const OPCODE_OR: u32 = 515;
pub const OPCODE_AND: u32 = 516;

// Rv32ShiftChip opcodes
pub const OPCODE_SLL: u32 = 517;
pub const OPCODE_SRL: u32 = 518;
pub const OPCODE_SRA: u32 = 519;

// Rv32LessThanChip opcodes
pub const OPCODE_SLT: u32 = 520;
pub const OPCODE_SLTU: u32 = 521;

// Load/Store opcodes
pub const OPCODE_LOADW: u32 = 528;
pub const OPCODE_LOADBU: u32 = 529;
pub const OPCODE_LOADHU: u32 = 530;
pub const OPCODE_STOREW: u32 = 531;
pub const OPCODE_STOREH: u32 = 532;
pub const OPCODE_STOREB: u32 = 533;
pub const OPCODE_LOADB: u32 = 534;
pub const OPCODE_LOADH: u32 = 535;

// Other opcodes
pub const OPCODE_BEQ: u32 = 544;
pub const OPCODE_BNE: u32 = 545;

pub const OPCODE_BLT: u32 = 549;
pub const OPCODE_BLTU: u32 = 550;
pub const OPCODE_BGE: u32 = 551;
pub const OPCODE_BGEU: u32 = 552;

pub const OPCODE_JAL: u32 = 560;
pub const OPCODE_LUI: u32 = 561;

pub const OPCODE_JALR: u32 = 565;

pub const OPCODE_AUIPC: u32 = 576;

pub const OPCODE_MUL: u32 = 592;
pub const OPCODE_MULH: u32 = 593;
pub const OPCODE_MULHSU: u32 = 594;
pub const OPCODE_MULHU: u32 = 595;

pub const OPCODE_DIV: u32 = 596;
pub const OPCODE_DIVU: u32 = 597;
pub const OPCODE_REM: u32 = 598;
pub const OPCODE_REMU: u32 = 599;

pub const OPCODE_HINT_STOREW: u32 = 608;
pub const OPCODE_HINT_BUFFER: u32 = 609;
