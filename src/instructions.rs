enum OpCode {
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
    Exit = 0xFF,
}

#[derive(Debug)]
pub(crate) enum BinopType {
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

#[derive(Debug)]
pub(crate) enum JumpType {
    Jmp,
    Jz,
    Jnz,
    Jg,
    Jge,
    Jl,
    Jle,
}

enum DataSize {
    I8 = 0b00,
    I16 = 0b01,
    I32 = 0b10,
    I64 = 0b11,
}
