use crate::assembler::core::{REGISTER_MAP, SIZE_MAP};
use std::collections::HashMap;

#[derive(Debug)]
pub enum MemoryAddress {
    Immediate(u64),
    Register(u8),
    RegisterOffset(u8, i64),
    RegisterScaledIndex(u8, u8, i64),
    Label(String),
}

fn parse_i64(str: &str) -> i64 {
    if str.starts_with("0x") {
        i64::from_str_radix(&str[2..], 16).unwrap()
    } else {
        i64::from_str_radix(str, 10).unwrap()
    }
}

fn parse_u64(str: &str) -> u64 {
    if str.starts_with("0x") {
        u64::from_str_radix(&str[2..], 16).unwrap()
    } else {
        u64::from_str_radix(str, 10).unwrap()
    }
}

impl MemoryAddress {
    pub fn from_str(str: &str) -> Self {
        if str.starts_with("[") && str.ends_with("]") {
            let substr = &str[1..str.len() - 1];
            let parts = substr.split(";").collect::<Vec<&str>>();
            if parts.len() == 1 {
                let reg = REGISTER_MAP.get(parts[0]).cloned().unwrap();
                MemoryAddress::Register(reg as u8)
            } else if parts.len() == 2 {
                let reg = REGISTER_MAP.get(parts[0]).cloned().unwrap();
                let offset = parse_i64(parts[1]);
                MemoryAddress::RegisterOffset(reg as u8, offset)
            } else if parts.len() == 3 {
                let reg = REGISTER_MAP.get(parts[0]).cloned().unwrap();
                let index = REGISTER_MAP.get(parts[1]).cloned().unwrap();
                let offset = parse_i64(parts[2]);
                MemoryAddress::RegisterScaledIndex(reg as u8, index as u8, offset)
            } else {
                panic!("Invalid memory address format: {}", str);
            }
        } else if str.starts_with("_") {
            MemoryAddress::Label(str.to_string())
        } else {
            let address = parse_u64(str);
            MemoryAddress::Immediate(address)
        }
    }

    pub fn add_bytes(
        &self,
        bytes: &mut Vec<u8>,
        labels: &HashMap<String, u64>,
        label_placeholders: &mut HashMap<String, Vec<u64>>,
    ) {
        match self {
            MemoryAddress::Immediate(address) => {
                bytes.push(0);
                bytes.extend(&address.to_le_bytes());
            }
            MemoryAddress::Register(register) => {
                bytes.push(1u8 | (register << 4));
            }
            MemoryAddress::RegisterOffset(register, offset) => {
                bytes.push(2u8 | (register << 4));
                bytes.extend(&offset.to_le_bytes());
            }
            MemoryAddress::RegisterScaledIndex(register, index_register, scale) => {
                bytes.push(3);
                bytes.push(*register | (*index_register << 4));
                bytes.extend(&scale.to_le_bytes());
            }
            MemoryAddress::Label(label) => {
                let address = labels.get(label).cloned().unwrap_or_else(|| {
                    label_placeholders
                        .entry(label.clone())
                        .or_insert_with(Vec::new)
                        .push((bytes.len() + 1) as u64);
                    0
                });
                bytes.push(0);
                bytes.extend(&address.to_le_bytes());
            }
        }
    }
}

#[derive(Debug)]
pub struct Instruction {
    pub kind: InstructionKind,
    pub opcode: u8,
}

#[derive(Debug)]
pub enum InstructionKind {
    Simple,
    Register {
        register: u8,
    },
    SizeRegister {
        size: u8,
        register: u8,
    },
    Address {
        address: MemoryAddress,
    },
    RegisterAddress {
        register: u8,
        address: MemoryAddress,
    },
    SizeRegisterAddress {
        register: u8,
        size: u8,
        address: MemoryAddress,
    },
    RegisterImmediate {
        register: u8,
        immediate: i64,
    },
    TwoRegisters {
        left_register: u8,
        right_register: u8,
    },
}

impl InstructionKind {
    pub fn parse_simple() -> Self {
        InstructionKind::Simple
    }

    pub fn parse_register(parts: Vec<&str>) -> Self {
        let register = parse_register_code(parts[1]);
        InstructionKind::Register { register }
    }

    pub fn parse_size_register(parts: Vec<&str>) -> Self {
        let size = parse_size_code(parts[1]);
        let register = parse_register_code(parts[2]);
        InstructionKind::SizeRegister { size, register }
    }

    pub fn parse_address(parts: Vec<&str>) -> Self {
        let address = MemoryAddress::from_str(parts[1]);
        InstructionKind::Address { address }
    }

    pub fn parse_register_address(parts: Vec<&str>) -> Self {
        let register = parse_register_code(parts[1]);
        let address = MemoryAddress::from_str(parts[2]);
        InstructionKind::RegisterAddress { register, address }
    }

    pub fn parse_register_size_address(parts: Vec<&str>) -> Self {
        let size = parse_size_code(parts[1]);
        let register = parse_register_code(parts[2]);
        let address = MemoryAddress::from_str(parts[3]);
        InstructionKind::SizeRegisterAddress {
            register,
            size,
            address,
        }
    }

    pub fn parse_register_immediate(parts: Vec<&str>) -> Self {
        let register = parse_register_code(parts[1]);
        let immediate = parse_i64(parts[2]);
        InstructionKind::RegisterImmediate {
            register,
            immediate,
        }
    }

    pub fn parse_two_registers(parts: Vec<&str>) -> Self {
        let left_register = parse_register_code(parts[1]);
        let right_register = parse_register_code(parts[2]);
        InstructionKind::TwoRegisters {
            left_register,
            right_register,
        }
    }
}

fn parse_register_code(part: &str) -> u8 {
    let register = REGISTER_MAP.get(part).cloned().unwrap_or_else(|| {
        panic!("Invalid register: {}", part);
    });
    register as u8
}

fn parse_size_code(part: &str) -> u8 {
    SIZE_MAP.get(part).cloned().unwrap_or_else(|| {
        panic!("Invalid size: {}", part);
    })
}

impl Instruction {
    pub fn add_bytes(
        &self,
        bytes: &mut Vec<u8>,
        labels: &HashMap<String, u64>,
        label_placeholders: &mut HashMap<String, Vec<u64>>,
    ) {
        bytes.push(self.opcode);
        match &self.kind {
            InstructionKind::Simple => {}
            InstructionKind::Register { register } => {
                bytes.push(*register);
            }
            InstructionKind::SizeRegister { size, register } => {
                bytes.push((size << 4) | register);
            }
            InstructionKind::Address { address } => {
                address.add_bytes(bytes, labels, label_placeholders);
            }
            InstructionKind::RegisterAddress { register, address } => {
                bytes.push(*register);
                address.add_bytes(bytes, labels, label_placeholders);
            }
            InstructionKind::SizeRegisterAddress {
                register,
                size,
                address,
            } => {
                bytes.push((size << 4) | register);
                address.add_bytes(bytes, labels, label_placeholders);
            }
            InstructionKind::RegisterImmediate {
                register,
                immediate,
            } => {
                bytes.push(*register);
                bytes.extend(&immediate.to_le_bytes());
            }
            InstructionKind::TwoRegisters {
                left_register,
                right_register,
            } => {
                bytes.push(left_register | (right_register << 4));
            }
        };
    }
}
