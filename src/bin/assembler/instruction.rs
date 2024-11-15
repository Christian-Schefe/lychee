use crate::instruction_type::InstructionType;
use lychee_vm::OpCode;

#[derive(Debug)]
pub enum Instruction {
    Label(String),
    Instr(InstructionType),
}
