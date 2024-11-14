use std::collections::HashMap;

pub enum InstructionType {
    Simple {
        opcode: u8,
    },
    SingleRegister {
        opcode: u8,
        register: u8,
    },
    SingleRegisterSized {
        opcode: u8,
        size: u8,
        register: u8,
    },
    RegisterAddress {
        opcode: u8,
        register: u8,
        size: u8,
        address: u64,
    },
    RegisterAddressLabel {
        opcode: u8,
        register: u8,
        size: u8,
        label: String,
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

fn num_byte_size(num: i64) -> usize {
    if num == 0 {
        return 1;
    }
    let mut num = num;
    let mut byte_size = 0;
    while num != 0 {
        num = num >> 8;
        byte_size += 1;
    }
    byte_size
}

impl InstructionType {
    pub fn byte_size(&self) -> usize {
        match self {
            InstructionType::Simple { .. } => 1,
            InstructionType::SingleRegister { .. } => 2,
            InstructionType::SingleRegisterSized { .. } => 2,
            InstructionType::RegisterAddress { .. } => 10,
            InstructionType::RegisterAddressLabel { .. } => 10,
            InstructionType::RegisterImmediate { immediate, .. } => 10 + num_byte_size(*immediate),
            InstructionType::TwoRegisters { .. } => 2,
        }
    }

    pub fn to_bytes(&self, labels: &HashMap<String, u64>) -> Vec<u8> {
        match self {
            InstructionType::Simple { opcode } => vec![*opcode],
            InstructionType::SingleRegister { opcode, register } => vec![*opcode, *register],
            InstructionType::SingleRegisterSized {
                opcode,
                size,
                register,
            } => {
                vec![*opcode, size | (register << 4)]
            }
            InstructionType::RegisterAddress {
                opcode,
                register,
                size,
                address,
            } => {
                let mut bytes = vec![*opcode, size | (register << 4)];
                bytes.extend(&address.to_le_bytes());
                bytes
            }
            InstructionType::RegisterAddressLabel {
                opcode,
                register,
                size,
                label,
            } => {
                let address = labels.get(label).unwrap();
                let mut bytes = vec![*opcode, size | (register << 4)];
                bytes.extend(&address.to_le_bytes());
                bytes
            }
            InstructionType::RegisterImmediate {
                opcode,
                register,
                immediate,
            } => {
                let byte_size = num_byte_size(*immediate) as u8;
                let mut bytes = vec![*opcode, byte_size | (register << 4)];
                bytes.extend(&immediate.to_le_bytes());
                bytes
            }
            InstructionType::TwoRegisters {
                opcode,
                source_register,
                dest_register,
            } => {
                vec![*opcode, source_register | (dest_register << 4)]
            }
        }
    }
}
