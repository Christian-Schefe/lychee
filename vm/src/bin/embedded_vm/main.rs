use std::fs;
use vm::execute;

fn main() {
    let program = get_bundled();
    execute(program, false);
}

pub fn get_bundled() -> Vec<u8> {
    let vm_path = std::env::args().nth(0).expect("VM executable path missing");
    let vm_binary = fs::read(vm_path).unwrap();

    let program_size_offset = vm_binary.len() - 8;
    let program_size = u64::from_le_bytes(vm_binary[program_size_offset..].try_into().unwrap());

    let program_start = program_size_offset - program_size as usize;
    let program = vm_binary[program_start..program_size_offset].to_vec();

    program
}
