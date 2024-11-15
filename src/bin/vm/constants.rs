use lychee_vm::RegisterCode;

pub const BP: usize = RegisterCode::BP as usize;
pub const SP: usize = RegisterCode::SP as usize;
pub const PC: usize = RegisterCode::PC as usize;

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
