use std::collections::HashMap;
use lychee_vm::OpCode;
use crate::{REGISTER_MAP, SIZE_MAP};

#[derive(Debug)]
pub enum MemoryAddress {
    Immediate(u64),
    Register(u8),
    RegisterOffset(u8, i64),
    RegisterScaledIndex(u8, u8, i64),
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
        } else {
            let address = parse_u64(str);
            MemoryAddress::Immediate(address)
        }
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        match self {
            MemoryAddress::Immediate(address) => {
                let mut bytes = vec![0];
                bytes.extend(&address.to_le_bytes());
                bytes
            }
            MemoryAddress::Register(register) => vec![1u8 | (register << 4)],
            MemoryAddress::RegisterOffset(register, offset) => {
                let mut bytes = vec![2u8 | (register << 4)];
                bytes.extend(&offset.to_le_bytes());
                bytes
            }
            MemoryAddress::RegisterScaledIndex(register, index_register, scale) => {
                let mut bytes = vec![3, register | (index_register << 4)];
                bytes.extend(&scale.to_le_bytes());
                bytes
            }
        }
    }
}

#[derive(Debug)]
pub enum InstructionType {
    Simple {
        opcode: u8,
    },
    Register {
        opcode: u8,
        register: u8,
    },
    SizeRegister {
        opcode: u8,
        size: u8,
        register: u8,
    },
    Label {
        opcode: u8,
        label: String,
    },
    RegisterAddress {
        opcode: u8,
        register: u8,
        address: MemoryAddress,
    },
    SizeRegisterAddress {
        opcode: u8,
        register: u8,
        size: u8,
        address: MemoryAddress,
    },
    RegisterImmediate {
        opcode: u8,
        register: u8,
        immediate: i64,
    },
    TwoRegisters {
        opcode: u8,
        source_register: u8,
        dest_register: u8,
    },
}

impl InstructionType {
    pub fn parse_simple(opcode: OpCode) -> Self {
        InstructionType::Simple { opcode: opcode as u8 }
    }

    pub fn parse_register(opcode: OpCode, parts: Vec<&str>) -> Self {
        let register = REGISTER_MAP.get(parts[1]).cloned().unwrap();
        InstructionType::Register { opcode: opcode as u8, register: register as u8 }
    }

    pub fn parse_size_register(opcode: OpCode, parts: Vec<&str>) -> Self {
        let size = SIZE_MAP.get(parts[1]).cloned().unwrap();
        let register = REGISTER_MAP.get(parts[2]).cloned().unwrap();
        InstructionType::SizeRegister { opcode: opcode as u8, size, register: register as u8 }
    }

    pub fn parse_label(opcode: OpCode, parts: Vec<&str>) -> Self {
        let address_str = parts[1];
        InstructionType::Label { opcode: opcode as u8, label: address_str.to_string() }
    }

    pub fn parse_register_address(opcode: OpCode, parts: Vec<&str>) -> Self {
        let register = REGISTER_MAP.get(parts[1]).cloned().unwrap();
        let address = MemoryAddress::from_str(parts[2]);
        InstructionType::RegisterAddress { opcode: opcode as u8, register: register as u8, address }
    }

    pub fn parse_register_size_address(opcode: OpCode, parts: Vec<&str>) -> Self {
        let size = SIZE_MAP.get(parts[1]).cloned().unwrap();
        let register = REGISTER_MAP.get(parts[2]).cloned().unwrap();
        let address = MemoryAddress::from_str(parts[3]);
        InstructionType::SizeRegisterAddress { opcode: opcode as u8, register: register as u8, size, address }
    }

    pub fn parse_register_immediate(opcode: OpCode, parts: Vec<&str>) -> Self {
        let register = REGISTER_MAP.get(parts[1]).cloned().unwrap();
        let immediate = parse_i64(parts[2]);
        InstructionType::RegisterImmediate { opcode: opcode as u8, register: register as u8, immediate }
    }

    pub fn parse_two_registers(opcode: OpCode, parts: Vec<&str>) -> Self {
        let dest_register = REGISTER_MAP.get(parts[1]).cloned().unwrap();
        let src_register = REGISTER_MAP.get(parts[2]).cloned().unwrap();
        InstructionType::TwoRegisters { opcode: opcode as u8, source_register: src_register as u8, dest_register: dest_register as u8 }
    }

    pub fn add_bytes(&self, bytes: &mut Vec<u8>, labels: &HashMap<String, u64>, label_placeholders: &mut HashMap<String, Vec<u64>>) {
        let bytes_to_add = match self {
            InstructionType::Simple { opcode } => vec![*opcode],
            InstructionType::Register { opcode, register } => vec![*opcode, *register],
            InstructionType::SizeRegister {
                opcode,
                size,
                register,
            } => {
                vec![*opcode, size | (register << 4)]
            }
            InstructionType::Label { opcode, label } => {
                let address = labels.get(label).cloned().unwrap_or_else(|| {
                    label_placeholders.entry(label.clone()).or_insert_with(Vec::new).push((bytes.len() + 1) as u64);
                    0
                });
                let mut new_bytes = vec![*opcode];
                new_bytes.extend(&address.to_le_bytes());
                new_bytes
            }
            InstructionType::RegisterAddress {
                opcode,
                register,
                address,
            } => {
                let mut new_bytes = vec![*opcode, *register];
                new_bytes.extend(&address.to_bytes());
                new_bytes
            }
            InstructionType::SizeRegisterAddress {
                opcode,
                register,
                size,
                address,
            } => {
                let mut new_bytes = vec![*opcode, size | (register << 4)];
                new_bytes.extend(&address.to_bytes());
                new_bytes
            }
            InstructionType::RegisterImmediate {
                opcode,
                register,
                immediate,
            } => {
                let mut new_bytes = vec![*opcode, *register];
                new_bytes.extend(&immediate.to_le_bytes());
                new_bytes
            }
            InstructionType::TwoRegisters {
                opcode,
                source_register,
                dest_register,
            } => {
                vec![*opcode, source_register | (dest_register << 4)]
            }
        };
        bytes.extend(bytes_to_add);
    }
}
