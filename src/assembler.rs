use crate::assembler::core::{convert_line, instructions_to_bytes};
use std::path::PathBuf;

mod core;
mod instruction_type;

pub fn assemble(input_file: &PathBuf) -> Result<PathBuf, anyhow::Error> {
    let str = std::fs::read_to_string(input_file)?;
    let mut instructions = Vec::new();
    for line in str.lines().filter(|line| !line.is_empty()) {
        let instr = convert_line(line);
        instructions.push(instr);
    }
    let bytes = instructions_to_bytes(instructions);

    let output = input_file.with_extension("o");
    std::fs::write(&output, bytes)?;
    Ok(output)
}
