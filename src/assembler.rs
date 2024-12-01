use crate::assembler::core::{convert_line, instructions_to_bytes, AssemblyInstruction};
use std::path::PathBuf;

mod core;
mod instruction_type;

pub fn assemble(input: &PathBuf, output: &PathBuf) {
    let str = std::fs::read_to_string(input).unwrap();
    let mut instructions = Vec::new();
    for line in str.lines().filter(|line| !line.is_empty()) {
        println!("{:?}", line);
        let instr = convert_line(line);
        println!("{:?}", instr);
        instructions.push(instr);
    }
    let bytes = instructions_to_bytes(instructions);

    std::fs::write(output, bytes).unwrap();
}
