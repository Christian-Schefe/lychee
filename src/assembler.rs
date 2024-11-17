use std::path::PathBuf;
use crate::assembler::core::{convert_line, instructions_to_bytes, Instruction};

mod core;
mod instruction_type;

pub fn assemble(input: &PathBuf, output: &PathBuf) {
    let str = std::fs::read_to_string(input).unwrap();
    let instructions = str.lines().filter(|line| !line.is_empty()).map(|line| convert_line(line)).collect::<Vec<Instruction>>();
    instructions.iter().for_each(|instr| println!("{:?}", instr));
    let bytes = instructions_to_bytes(instructions);

    std::fs::write(output, bytes).unwrap();
}