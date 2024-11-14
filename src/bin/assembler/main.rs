use std::collections::HashMap;

extern crate lychee_vm;

mod instruction_type;
mod instruction;

fn main() {
    let args = std::env::args().collect::<Vec<String>>();
    let path = &args[1];
    let out_path = &args[2];
    let str = std::fs::read_to_string(path).unwrap();
    let lines = str.lines();
    let mut bytes = Vec::new();
    let mut labels = HashMap::new();
    for line in lines {
        convert_line(line, &mut bytes, &mut labels);
    }
    std::fs::write(out_path, bytes).unwrap();
}

fn convert_line(line: &str, bytes: &mut Vec<u8>, labels: &mut HashMap<String, usize>) {
    let parts = line.split_whitespace().collect::<Vec<&str>>();

    if parts[0].ends_with(":") {
        let label_str = parts[0].split(":").collect::<Vec<&str>>()[0];
        labels.insert(label_str.to_string(), bytes.len());
        return;
    }

    let op = parts[0].split("_").collect::<Vec<&str>>()[0];
    let op_bytes = match op {
        "nop" => nop(),
        "load" => load(parts),
        "store" => store(parts),
        "set" => set(parts),
        "push" => push(parts),
        "pop" => pop(parts),
        "add" => binop(parts, 0),
        "sub" => binop(parts, 1),
        "mul" => binop(parts, 2),
        "div" => binop(parts, 3),
        "mod" => binop(parts, 4),
        "and" => binop(parts, 5),
        "or" => binop(parts, 6),
        "xor" => binop(parts, 7),
        "shl" => binop(parts, 8),
        "shr" => binop(parts, 9),
        "cmp" => cmp(parts),
        "jmp" => jump(parts, &labels, 0),
        "je" => jump(parts, &labels, 1),
        "jne" => jump(parts, &labels, 2),
        "jg" => jump(parts, &labels, 3),
        "jge" => jump(parts, &labels, 4),
        "jl" => jump(parts, &labels, 5),
        "jle" => jump(parts, &labels, 6),
        "exit" => exit(),
        _ => panic!("Invalid operation"),
    };
    bytes.extend(op_bytes);
}

fn nop() -> Vec<u8> {
    vec![0]
}

fn load(parts: Vec<&str>) -> Vec<u8> {
    let size_str = parts[0].split("_").collect::<Vec<&str>>()[1];
    let size = match size_str {
        "8" => 0,
        "16" => 1,
        "32" => 2,
        "64" => 3,
        _ => panic!("Invalid size"),
    };
    let register = parts[1].parse::<u8>().unwrap();
    let address = parse_u64(parts[2]);
    let mut bytes = vec![1];
    let byte1 = (size & 0b00000011) | (register << 4);
    bytes.push(byte1);
    bytes.extend(&address.to_le_bytes());
    bytes
}

fn store(parts: Vec<&str>) -> Vec<u8> {
    let size_str = parts[0].split("_").collect::<Vec<&str>>()[1];
    let size = match size_str {
        "8" => 0,
        "16" => 1,
        "32" => 2,
        "64" => 3,
        _ => panic!("Invalid size"),
    };
    let register = parts[1].parse::<u8>().unwrap();
    let address = parse_u64(parts[2]);
    let mut bytes = vec![2];
    let byte1 = (size & 0b00000011) | (register << 4);
    bytes.push(byte1);
    bytes.extend(&address.to_le_bytes());
    bytes
}

fn set(parts: Vec<&str>) -> Vec<u8> {
    let byte_count = 8;
    let register = parts[1].parse::<u8>().unwrap();
    let value = parse_i64(parts[2]);
    let mut bytes = vec![3];
    let byte1 = (byte_count - 1) | (register << 4);
    bytes.push(byte1);
    bytes.extend(&value.to_le_bytes());
    bytes
}

fn push(parts: Vec<&str>) -> Vec<u8> {
    let size_str = parts[0].split("_").collect::<Vec<&str>>()[1];
    let size = match size_str {
        "8" => 0,
        "16" => 1,
        "32" => 2,
        "64" => 3,
        _ => panic!("Invalid size"),
    };
    let register = parts[1].parse::<u8>().unwrap();
    let mut bytes = vec![4];
    let byte1 = (size & 0b00000011) | (register << 4);
    bytes.push(byte1);
    bytes
}

fn pop(parts: Vec<&str>) -> Vec<u8> {
    let size_str = parts[0].split("_").collect::<Vec<&str>>()[1];
    let size = match size_str {
        "8" => 0,
        "16" => 1,
        "32" => 2,
        "64" => 3,
        _ => panic!("Invalid size"),
    };
    let register = parts[1].parse::<u8>().unwrap();
    let mut bytes = vec![5];
    let byte1 = (size & 0b00000011) | (register << 4);
    bytes.push(byte1);
    bytes
}

fn binop(parts: Vec<&str>, op: u8) -> Vec<u8> {
    let register1 = parts[1].parse::<u8>().unwrap();
    let register2 = parts[2].parse::<u8>().unwrap();
    let mut bytes = vec![6 + op];
    let byte1 = (register1 & 0x0F) | ((register2 & 0x0F) << 4);
    bytes.push(byte1);
    bytes
}

fn cmp(parts: Vec<&str>) -> Vec<u8> {
    let register1 = parts[1].parse::<u8>().unwrap();
    let register2 = parts[2].parse::<u8>().unwrap();
    let mut bytes = vec![0x11];
    let byte1 = (register1 & 0x0F) | ((register2 & 0x0F) << 4);
    bytes.push(byte1);
    bytes
}

fn jump(parts: Vec<&str>, labels: &HashMap<String, usize>, op: u8) -> Vec<u8> {
    let address = parse_address(parts[1], &labels);
    let mut bytes = vec![0x12 + op];
    bytes.extend(&address.to_le_bytes());
    bytes
}

fn exit() -> Vec<u8> {
    vec![0xFF]
}

fn parse_address(value: &str, labels: &HashMap<String, usize>) -> u64 {
    if value.starts_with("0x") {
        return u64::from_str_radix(&value[2..], 16).unwrap();
    }
    if value.starts_with("_") {
        return labels.get(value).unwrap().clone() as u64;
    }
    value.parse::<u64>().unwrap()
}

fn parse_u64(value: &str) -> u64 {
    if value.starts_with("0x") {
        u64::from_str_radix(&value[2..], 16).unwrap()
    } else {
        value.parse::<u64>().unwrap()
    }
}

fn parse_i64(value: &str) -> i64 {
    if value.starts_with("-") {
        return parse_i64(&value[1..]) * -1;
    }
    if value.starts_with("0x") {
        i64::from_str_radix(&value[2..], 16).unwrap()
    } else {
        value.parse::<i64>().unwrap()
    }
}
