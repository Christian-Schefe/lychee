use crate::instruction_type::InstructionType;
use lychee_vm::OpCode;

pub enum Instruction {
    Label(String),
    Instruction(InstructionType),
}
