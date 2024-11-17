#[derive(Debug)]
pub enum BinopType {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Xor,
    Shl,
    Shr,
}

#[repr(u8)]
#[derive(Clone, Debug)]
pub enum OpCode {
    Nop = 0x00,
    Load = 0x01,
    Store = 0x02,
    Set = 0x03,
    Push = 0x04,
    Pop = 0x05,
    Add = 0x06,
    Sub = 0x07,
    Mul = 0x08,
    Div = 0x09,
    Mod = 0x0A,
    And = 0x0B,
    Or = 0x0C,
    Xor = 0x0D,
    Not = 0x0E,
    Shl = 0x0F,
    Shr = 0x10,
    Cmp = 0x11,
    Jmp = 0x12,
    Jz = 0x13,
    Jnz = 0x14,
    Jg = 0x15,
    Jge = 0x16,
    Jl = 0x17,
    Jle = 0x18,
    Call = 0x19,
    Ret = 0x1A,
    Inc = 0x1B,
    Dec = 0x1C,
    ReadStdin = 0x1D,
    WriteStdout = 0x1E,
    Move = 0x1F,
    Neg = 0x20,
    SetZ = 0x21,
    SetNz = 0x22,
    SetG = 0x23,
    SetGe = 0x24,
    SetL = 0x25,
    SetLe = 0x26,
    Exit = 0xFF,
}

#[repr(u8)]
#[derive(Clone, Debug)]
pub enum RegisterCode {
    R0 = 0x00,
    R1 = 0x01,
    R2 = 0x02,
    R3 = 0x03,
    R4 = 0x04,
    R5 = 0x05,
    R6 = 0x06,
    R7 = 0x07,
    R8 = 0x08,
    R9 = 0x09,
    R10 = 0x0A,
    R11 = 0x0B,
    R12 = 0x0C,
    BP = 0x0D,
    SP = 0x0E,
    PC = 0x0F,
}

pub const DATA_SIZE_8: u8 = 0x01;
pub const DATA_SIZE_16: u8 = 0x02;
pub const DATA_SIZE_32: u8 = 0x04;
pub const DATA_SIZE_64: u8 = 0x08;