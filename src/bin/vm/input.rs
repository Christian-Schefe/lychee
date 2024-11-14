use std::env::args;

pub fn read_obj_file() -> Vec<u8> {
    let args = args().collect::<Vec<String>>();
    let path = &args[1];
    println!("Reading file: {}", path);
    let bytes = std::fs::read(path).unwrap();
    bytes
}