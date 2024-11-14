use crate::registers;

pub struct Memory {
    size: usize,
    pub(crate) data: Vec<u8>,
    pub(crate) registers: [i64; 16],
    pub(crate) flags: u64,
}

impl Memory {
    pub fn new(size: usize, program: Vec<u8>) -> Memory {
        let mut memory = Memory {
            size,
            data: vec![0; size],
            registers: [0; 16],
            flags: 0,
        };
        memory.data[..program.len()].copy_from_slice(&program);

        memory.registers[registers::SP] = size as i64;

        memory
    }

    pub fn read_u64_le(&self, address: usize, bytes: usize) -> u64 {
        let mut result = 0;
        for i in 0..bytes {
            result |= (self.data[address + i] as u64) << (i * 8);
        }
        result
    }

    pub fn read_i64_le(&self, address: usize, bytes: usize) -> i64 {
        let mut result = 0;
        for i in 0..bytes {
            result |= (self.data[address + i] as i64) << (i * 8);
        }
        result
    }

    pub fn write_u64_le(&mut self, address: usize, value: u64, bytes: usize) {
        for i in 0..bytes {
            self.data[address + i] = ((value >> (i * 8)) & 0xFF) as u8;
        }
    }
    
    pub fn write_i64_le(&mut self, address: usize, value: i64, bytes: usize) {
        for i in 0..bytes {
            self.data[address + i] = ((value >> (i * 8)) & 0xFF) as u8;
        }
    }
}