use std::fmt;

use powdr_isa_utils::SingleDataValue;
use powdr_number::FieldSize;

use crate::CompilerOptions;

use crate::large_field;
use crate::small_field;

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

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.value < 32 {
            // 0 indexed
            write!(f, "x{}", self.value)
        } else if self.value < 36 {
            // 1 indexed
            write!(f, "tmp{}", self.value - 31 + 1)
        } else if self.value == 36 {
            write!(f, "lr_sc_reservation")
        } else {
            // 0 indexed
            write!(f, "xtra{}", self.value - 37)
        }
    }
}

impl From<&str> for Register {
    fn from(s: &str) -> Self {
        if let Some(prefix) = s.strip_prefix("xtra") {
            // 0 indexed
            let value: u8 = prefix.parse().expect("Invalid register");
            Self::new(value + 37)
        } else if let Some(prefix) = s.strip_prefix('x') {
            // 0 indexed
            let value = prefix.parse().expect("Invalid register");
            assert!(value < 32, "Invalid register");
            Self::new(value)
        } else if let Some(prefix) = s.strip_prefix("tmp") {
            // 1 indexed
            let value: u8 = prefix.parse().expect("Invalid register");
            assert!(value >= 1);
            assert!(value <= 4);
            Self::new(value - 1 + 32)
        } else if s == "lr_sc_reservation" {
            Self::new(36)
        } else {
            panic!("Invalid register")
        }
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
    fn take_source_files_info(&mut self) -> impl Iterator<Item = SourceFileInfo>;

    /// Takes the initial memory snapshot.
    fn take_initial_mem(&mut self) -> impl Iterator<Item = MemEntry>;

    /// Takes the executable statements and labels.
    fn take_executable_statements(
        &mut self,
    ) -> impl Iterator<Item = Statement<impl AsRef<str>, impl InstructionArgs>>;

    /// Returns the addresses of the start and end of prover data.
    fn prover_data_bounds(&self) -> (u32, u32);

    /// The name of the function that should be called to start the program.
    fn start_function(&self) -> impl AsRef<str>;
}

/// Translates a RISC-V program to POWDR ASM.
///
/// Will call each of the methods in the `RiscVProgram` just once.
pub fn translate_program(program: impl RiscVProgram, options: CompilerOptions) -> String {
    match options.field.field_size() {
        FieldSize::Small => small_field::code_gen::translate_program(program, options),
        FieldSize::Large => large_field::code_gen::translate_program(program, options),
    }
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
