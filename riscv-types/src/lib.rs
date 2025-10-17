use powdr_isa_utils::SingleDataValue;
use std::fmt;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Register {
    value: u8,
}

impl Register {
    pub fn new(value: u8) -> Self {
        Self { value }
    }

    pub fn is_zero(&self) -> bool {
        self.value == 0
    }

    pub fn addr(&self) -> u8 {
        self.value
    }
}

/// List of machine registers, declared in the asm machine.
/// NOTE: the bootloader expects the PC to be the last register in this list.
pub const REGISTER_NAMES: [&str; 3] = ["main::query_arg_1", "main::query_arg_2", "main::pc"];

/// These are the names of the RISCV registers that are stored in memory.
pub const REGISTER_MEMORY_NAMES: [&str; 37] = [
    "x0",
    "x1",
    "x2",
    "x3",
    "x4",
    "x5",
    "x6",
    "x7",
    "x8",
    "x9",
    "x10",
    "x11",
    "x12",
    "x13",
    "x14",
    "x15",
    "x16",
    "x17",
    "x18",
    "x19",
    "x20",
    "x21",
    "x22",
    "x23",
    "x24",
    "x25",
    "x26",
    "x27",
    "x28",
    "x29",
    "x30",
    "x31",
    "tmp1",
    "tmp2",
    "tmp3",
    "tmp4",
    "lr_sc_reservation",
];

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", REGISTER_MEMORY_NAMES[self.value as usize])
    }
}

impl From<&str> for Register {
    fn from(s: &str) -> Self {
        REGISTER_MEMORY_NAMES
            .iter()
            .position(|&name| name == s)
            .map(|value| Self::new(value as u8))
            .unwrap_or_else(|| panic!("Invalid register"))
    }
}

pub enum Statement<'a, L: AsRef<str>, A: InstructionArgs> {
    DebugLoc { file: u64, line: u64, col: u64 },
    Label(L),
    Instruction { op: &'a str, args: A },
}

pub struct MemEntry {
    pub label: Option<String>,
    pub addr: u32,
    pub value: SingleDataValue,
}

pub struct SourceFileInfo<'a> {
    pub id: u32,
    pub dir: &'a str,
    pub file: &'a str,
}

/// A RISC-V program that can be translated to POWDR ASM.
pub trait RiscVProgram {
    /// Takes the listing of source files, to be used in the debug statements.
    fn take_source_files_info(&mut self) -> impl Iterator<Item = SourceFileInfo<'_>>;

    /// Takes the initial memory snapshot.
    fn take_initial_mem(&mut self) -> impl Iterator<Item = MemEntry>;

    /// Takes the executable statements and labels.
    fn take_executable_statements(
        &mut self,
    ) -> impl Iterator<Item = Statement<'_, impl AsRef<str>, impl InstructionArgs>>;

    /// Returns the addresses of the start and end of prover data.
    fn prover_data_bounds(&self) -> (u32, u32);

    /// The name of the function that should be called to start the program.
    fn start_function(&self) -> impl AsRef<str>;
}

pub trait InstructionArgs {
    type Error: fmt::Display;

    fn l(&self) -> Result<impl AsRef<str>, Self::Error>;
    fn r(&self) -> Result<Register, Self::Error>;
    fn rri(&self) -> Result<(Register, Register, u32), Self::Error>;
    /// Returns the usual rd, rs1, rs2
    fn rrr(&self) -> Result<(Register, Register, Register), Self::Error>;
    /// Special case used in amo* instructions, returning rd, rs2, rs1
    fn rrr2(&self) -> Result<(Register, Register, Register), Self::Error>;
    fn ri(&self) -> Result<(Register, u32), Self::Error>;
    fn rr(&self) -> Result<(Register, Register), Self::Error>;
    fn rrl(&self) -> Result<(Register, Register, impl AsRef<str>), Self::Error>;
    fn rl(&self) -> Result<(Register, impl AsRef<str>), Self::Error>;
    fn rro(&self) -> Result<(Register, Register, u32), Self::Error>;
    fn empty(&self) -> Result<(), Self::Error>;
}
