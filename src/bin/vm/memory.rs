use std::io::Read;
use std::iter::repeat;
use crate::constants;

#[derive(Debug)]
pub struct Flags {
    pub zero: bool,
    pub positive: bool,
}

pub struct Memory {
    size: usize,
    pub(crate) data: Vec<u8>,
    pub(crate) registers: [u64; 16],
    pub(crate) flags: Flags,
}

impl Memory {
    pub fn new(size: usize, program: Vec<u8>) -> Memory {
        let mut memory = Memory {
            size,
            data: vec![0; size],
            registers: [0; 16],
            flags: Flags {
                zero: false,
                positive: false,
            },
        };
        memory.data[..program.len()].copy_from_slice(&program);

        memory.registers[constants::SP] = size as u64;
        memory.registers[constants::BP] = size as u64;

        memory
    }

    pub fn read_u64_le(&self, address: usize, bytes: usize) -> u64 {
        let mut buffer = [0; 8];
        let byte_arr = &self.data[address..address + bytes];
        buffer[..bytes].copy_from_slice(&byte_arr);
        u64::from_le_bytes(buffer)
    }

    pub fn read_bytes(&self, address: usize, bytes: usize) -> Vec<u8> {
        self.data[address..address + bytes].to_vec()
    }

    pub fn write_u64_le(&mut self, address: usize, value: u64, bytes: usize) {
        let byte_slice = &value.to_le_bytes()[..bytes];
        self.data[address..address + bytes].copy_from_slice(&byte_slice);
    }

    pub fn write_bytes(&mut self, address: usize, bytes: &[u8]) {
        self.data[address..address + bytes.len()].copy_from_slice(bytes);
    }

    pub fn print_registers(&self) {
        println!("Registers: {:?}", self.registers.map(|r| r as i64));
    }
}