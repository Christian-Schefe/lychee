use std::path::PathBuf;

pub fn read_obj_file(input: &PathBuf, debug_print: bool) -> Vec<u8> {
    if debug_print {
        println!("Reading file: {}", input.to_str().unwrap());
    }
    let bytes = std::fs::read(input).unwrap();
    bytes
}
