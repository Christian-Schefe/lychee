use std::path::PathBuf;

pub fn read_obj_file(input: &PathBuf) -> Vec<u8> {
    println!("Reading file: {}", input.to_str().unwrap());
    let bytes = std::fs::read(input).unwrap();
    bytes
}
