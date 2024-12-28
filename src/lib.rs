#[derive(Clone, Debug)]
pub enum OpCode {
    Exit,
    Load,
    Store,
    Push,
    Pop,
    Binop(BinopType),
    BinopImmediate(BinopType),
    Jump(FlagConditionType),
    Set(FlagConditionType),
    Unop(UnopType),
    Call,
    Ret,
    ReadStdin,
    WriteStdout,
    Rand,
    SignExtend,
    Lea,
    PushMem,
    PopMem,
    PeekMem,
    Alloc,
    Free,
    FileOpen,
    FileClose,
    FileRead,
    FileWrite,
    MemCopy,
    MemSet,
}

impl OpCode {
    pub fn byte_code(&self) -> u8 {
        match self {
            OpCode::Exit => 0x00,
            OpCode::Load => 0x01,
            OpCode::Store => 0x02,
            OpCode::Push => 0x03,
            OpCode::Pop => 0x04,
            OpCode::Binop(op_type) => 0x05 + op_type.clone() as u8,
            OpCode::BinopImmediate(op_type) => 0x11 + op_type.clone() as u8,
            OpCode::Jump(flag_cond) => 0x1D + flag_cond.clone() as u8,
            OpCode::Set(flag_cond) => 0x24 + flag_cond.clone() as u8,
            OpCode::Unop(op_type) => 0x2B + op_type.clone() as u8,
            OpCode::Call => 0x2F,
            OpCode::Ret => 0x30,
            OpCode::ReadStdin => 0x31,
            OpCode::WriteStdout => 0x32,
            OpCode::Rand => 0x33,
            OpCode::SignExtend => 0x34,
            OpCode::Lea => 0x35,
            OpCode::PushMem => 0x36,
            OpCode::PopMem => 0x37,
            OpCode::PeekMem => 0x38,
            OpCode::Alloc => 0x39,
            OpCode::Free => 0x3A,
            OpCode::FileOpen => 0x3B,
            OpCode::FileClose => 0x3C,
            OpCode::FileRead => 0x3D,
            OpCode::FileWrite => 0x3E,
            OpCode::MemCopy => 0x3F,
            OpCode::MemSet => 0x40,
        }
    }
}

#[repr(u8)]
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BinopType {
    Mov = 0x00,
    Add = 0x01,
    Sub = 0x02,
    Mul = 0x03,
    Div = 0x04,
    Mod = 0x05,
    And = 0x06,
    Or = 0x07,
    Xor = 0x08,
    Shl = 0x09,
    Shr = 0x0A,
    Cmp = 0x0B,
}

#[repr(u8)]
#[derive(Clone, Debug)]
pub enum UnopType {
    Not = 0x00,
    Neg = 0x01,
    Inc = 0x02,
    Dec = 0x03,
}

#[repr(u8)]
#[derive(Clone, Debug)]
pub enum FlagConditionType {
    Always = 0x00,
    Zero = 0x01,
    NotZero = 0x02,
    Greater = 0x03,
    GreaterEquals = 0x04,
    Less = 0x05,
    LessEquals = 0x06,
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

pub const DATA_SIZE_64: u8 = 0x08;
pub const DATA_SIZE_32: u8 = 0x04;
pub const DATA_SIZE_8: u8 = 0x01;
