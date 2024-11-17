use std::io::{Read, Write};
use std::process::ExitCode;
use lychee_compiler::DATA_SIZE_64;
use crate::constants::BinopType;
use crate::memory::Memory;

mod input;
mod memory;
mod constants;

fn main() {
    let program = input::read_obj_file();

    let size = 0x100000;
    let mut memory = Memory::new(size, program);
    let exit_code = run(&mut memory);
    println!("Main return value: {}", exit_code);
}

pub fn run(memory: &mut Memory) -> i64 {
    let mut exit_code = 0;
    loop {
        let pc = memory.registers[constants::PC] as usize;
        let opcode = memory.data[pc];
        println!("PC: {}, Opcode: {:X}", pc, opcode);
        match opcode {
            0x00 => memory.registers[constants::PC] += 1,
            0x01 => load(pc, memory),
            0x02 => store(pc, memory),
            0x03 => set(pc, memory),
            0x04 => push(pc, memory),
            0x05 => pop(pc, memory),
            0x06 => binop(pc, memory, BinopType::Add),
            0x07 => binop(pc, memory, BinopType::Sub),
            0x08 => binop(pc, memory, BinopType::Mul),
            0x09 => binop(pc, memory, BinopType::Div),
            0x0A => binop(pc, memory, BinopType::Mod),
            0x0B => binop(pc, memory, BinopType::And),
            0x0C => binop(pc, memory, BinopType::Or),
            0x0D => binop(pc, memory, BinopType::Xor),
            0x0F => binop(pc, memory, BinopType::Shl),
            0x10 => binop(pc, memory, BinopType::Shr),
            0x11 => compare(pc, memory),
            0x12 => jump(pc, memory, true),
            0x13 => jump(pc, memory, memory.flags.zero),
            0x14 => jump(pc, memory, !memory.flags.zero),
            0x15 => jump(pc, memory, memory.flags.positive),
            0x16 => jump(pc, memory, memory.flags.positive || memory.flags.zero),
            0x17 => jump(pc, memory, !memory.flags.positive && !memory.flags.zero),
            0x18 => jump(pc, memory, !memory.flags.positive),
            0x19 => call(pc, memory),
            0x1A => ret(memory),
            0x1B => inc_or_dec(pc, memory, 1),
            0x1C => inc_or_dec(pc, memory, -1),
            0x1D => read_stdin(pc, memory),
            0x1E => write_stdout(pc, memory),
            0x1F => move_registers(pc, memory),
            0x20 => negate(pc, memory),
            0x21 => set_from_flags(pc, memory, memory.flags.zero),
            0x22 => set_from_flags(pc, memory, !memory.flags.zero),
            0x23 => set_from_flags(pc, memory, memory.flags.positive),
            0x24 => set_from_flags(pc, memory, memory.flags.positive || memory.flags.zero),
            0x25 => set_from_flags(pc, memory, !memory.flags.positive && !memory.flags.zero),
            0x26 => set_from_flags(pc, memory, !memory.flags.positive),
            0xFF => {
                exit(&mut exit_code, memory);
                break;
            }
            _ => panic!("Unknown opcode: {}", opcode),
        };
        memory.print_registers();
        memory.print_stack();
        println!();
    }
    exit_code
}

fn read_address(pc: usize, memory: &mut Memory) -> u64 {
    let first_byte = memory.data[pc];
    let address_type = first_byte & 0b00000011;
    let address = match address_type {
        0 => {
            memory.registers[constants::PC] += 9;
            memory.read_u64_le(pc + 1, DATA_SIZE_64)
        }
        1 => {
            memory.registers[constants::PC] += 1;
            let register = (first_byte & 0b11110000) >> 4;
            memory.registers[register as usize]
        }
        2 => {
            memory.registers[constants::PC] += 9;
            let register = (first_byte & 0b11110000) >> 4;
            let offset = memory.read_i64_le(pc + 1, DATA_SIZE_64);
            if offset >= 0 {
                memory.registers[register as usize] + offset as u64
            } else {
                memory.registers[register as usize] - (-offset) as u64
            }
        }
        3 => {
            memory.registers[constants::PC] += 10;
            let byte3 = memory.data[pc + 1];
            let index_register = byte3 & 0b00001111;
            let register = (byte3 & 0b11110000) >> 4;
            let scale = memory.read_i64_le(pc + 2, DATA_SIZE_64);
            let signed_offset = scale * memory.registers[index_register as usize] as i64;
            if signed_offset >= 0 {
                memory.registers[register as usize] + signed_offset as u64
            } else {
                memory.registers[register as usize] - (-signed_offset) as u64
            }
        }
        _ => panic!("Invalid address type: {}", address_type),
    };
    address
}

fn load(pc: usize, memory: &mut Memory) {
    let byte1 = memory.data[pc + 1];

    let data_size = 1 << (byte1 & 0b00000011);
    let register = (byte1 & 0b11110000) >> 4;

    let address = read_address(pc + 2, memory);
    let value = memory.read_i64_le(address as usize, data_size);

    memory.registers[register as usize] = value as u64;
    memory.registers[constants::PC] += 2;

    println!(
        "Loaded {} ({} bytes) from {} into register {}",
        value, data_size, address, register
    );
}

fn store(pc: usize, memory: &mut Memory) {
    let byte1 = memory.data[pc + 1];

    let byte_count = 1 << (byte1 & 0b00000011);
    let register = (byte1 & 0b11110000) >> 4;

    let address = read_address(pc + 2, memory);
    let value = memory.registers[register as usize] as i64;

    memory.write_i64_le(address as usize, value, byte_count);
    memory.registers[constants::PC] += 2;

    println!(
        "Stored {} ({} bytes) from register {} into {}",
        value, byte_count, register, address
    );
}

fn set(pc: usize, memory: &mut Memory) {
    let byte1 = memory.data[pc + 1];
    let register = byte1;
    let value = memory.read_i64_le(pc + 2, DATA_SIZE_64);

    memory.registers[register as usize] = value as u64;
    memory.registers[constants::PC] += 10;

    println!("Set register {} to {}", register, value);
}

fn push(pc: usize, memory: &mut Memory) {
    let byte1 = memory.data[pc + 1];
    let data_size = 1 << (byte1 & 0b00000011);
    let register = (byte1 & 0b11110000) >> 4;

    memory.registers[constants::SP] -= data_size;
    let address = memory.registers[constants::SP];

    let value = memory.registers[register as usize] as i64;
    memory.write_i64_le(address as usize, value, data_size as u8);
    memory.registers[constants::PC] += 2;

    println!("Pushed register {} with value {} ({} bytes) onto the stack", register, value, data_size, );
}

fn pop(pc: usize, memory: &mut Memory) {
    let byte1 = memory.data[pc + 1];
    let data_size = 1 << (byte1 & 0b00000011);
    let register = (byte1 & 0b11110000) >> 4;

    let address = memory.registers[constants::SP];
    let value = memory.read_i64_le(address as usize, data_size);

    memory.registers[register as usize] = value as u64;
    memory.registers[constants::SP] += data_size as u64;
    memory.registers[constants::PC] += 2;

    println!("Popped {} bytes from the stack into register {} with value {}", data_size, register, value);
}

fn binop(pc: usize, memory: &mut Memory, op_type: BinopType) {
    let byte1 = memory.data[pc + 1];
    let source_register = (byte1 & 0x0F) as usize;
    let dest_register = ((byte1 & 0xF0) >> 4) as usize;

    let left_value = memory.registers[dest_register] as i64;
    let right_value = memory.registers[source_register] as i64;

    let result = match op_type {
        BinopType::Add => left_value + right_value,
        BinopType::Sub => left_value - right_value,
        BinopType::Mul => left_value * right_value,
        BinopType::Div => left_value / right_value,
        BinopType::Mod => left_value % right_value,
        BinopType::And => left_value & right_value,
        BinopType::Or => left_value | right_value,
        BinopType::Xor => left_value ^ right_value,
        BinopType::Shl => left_value << right_value,
        BinopType::Shr => left_value >> right_value,
    };

    set_flags(memory, result);

    memory.registers[dest_register] = result as u64;
    memory.registers[constants::PC] += 2;

    println!(
        "Performed {:?} on registers {} and {} ({} and {}), Result: {}, Flags: {:?}",
        op_type, dest_register, source_register, left_value, right_value, result, memory.flags
    );
}

fn compare(pc: usize, memory: &mut Memory) {
    let byte1 = memory.data[pc + 1];
    let source_register = (byte1 & 0x0F) as usize;
    let dest_register = ((byte1 & 0xF0) >> 4) as usize;

    let left_value = memory.registers[dest_register];
    let right_value = memory.registers[source_register];

    memory.flags.zero = left_value == right_value;
    memory.flags.positive = left_value > right_value;

    memory.registers[constants::PC] += 2;

    println!("Compared {} and {} ({} and {}), Flags: {:?}", left_value, right_value, dest_register, source_register, memory.flags);
}

fn jump(pc: usize, memory: &mut Memory, should_jump: bool) {
    let address = memory.read_u64_le(pc + 1, DATA_SIZE_64);

    if should_jump {
        memory.registers[constants::PC] = address;
        println!("Jumped to address {}", address);
    } else {
        memory.registers[constants::PC] += 9;
        println!("Did not jump to address {}", address);
    }
}

fn call(pc: usize, memory: &mut Memory) {
    let address = memory.read_u64_le(pc + 1, DATA_SIZE_64);
    memory.registers[constants::SP] -= 8;
    let return_pc = (pc + 9) as u64;
    memory.write_u64_le(memory.registers[constants::SP] as usize, return_pc, DATA_SIZE_64);
    memory.registers[constants::PC] = address;

    println!("Called Address {}, Return PC: {}", address, return_pc);
}

fn ret(memory: &mut Memory) {
    let address = memory.read_u64_le(memory.registers[constants::SP] as usize, DATA_SIZE_64);
    memory.registers[constants::SP] += 8;
    memory.registers[constants::PC] = address;

    println!("Returned to Address {}", address);
}

fn inc_or_dec(pc: usize, memory: &mut Memory, amount: i64) {
    let byte1 = memory.data[pc + 1];
    let register = (byte1 & 0x0F) as usize;
    let value = memory.registers[register] as i64 + amount;
    memory.registers[register] = value as u64;
    set_flags(memory, value);
    memory.registers[constants::PC] += 2;

    println!("Incremented register {} by {}, Result: {}, Flags: {:?}", register, amount, value, memory.flags);
}

fn read_stdin(pc: usize, memory: &mut Memory) {
    let register = memory.data[pc + 1];
    let read_bytes = memory.registers[register as usize] as usize;
    let address = read_address(pc + 2, memory) as usize;

    let mut buffer = vec![0; read_bytes];
    std::io::stdin().read_exact(&mut buffer).unwrap();
    memory.write_bytes(address, &buffer);
    memory.registers[constants::PC] += 2;

    println!("Read {} bytes from stdin into address {}", read_bytes, address);
}

fn write_stdout(pc: usize, memory: &mut Memory) {
    let register = memory.data[pc + 1];
    let write_bytes = memory.registers[register as usize] as usize;
    let address = read_address(pc + 2, memory) as usize;

    let buffer = memory.read_bytes(address, write_bytes);
    std::io::stdout().write_all(&buffer).unwrap();
    memory.registers[constants::PC] += 2;

    println!("Wrote {} bytes to stdout from address {}", write_bytes, address);
}

fn move_registers(pc: usize, memory: &mut Memory) {
    let byte1 = memory.data[pc + 1];
    let source_register = (byte1 & 0x0F) as usize;
    let dest_register = (byte1 & 0xF0) >> 4;
    memory.registers[dest_register as usize] = memory.registers[source_register];
    memory.registers[constants::PC] += 2;

    println!("Moved register {} to register {}", source_register, dest_register);
}

fn negate(pc: usize, memory: &mut Memory) {
    let byte1 = memory.data[pc + 1];
    let register = (byte1 & 0x0F) as usize;
    let value = memory.registers[register] as i64;
    let result = -value;
    set_flags(memory, result);
    memory.registers[register] = result as u64;
    memory.registers[constants::PC] += 2;

    println!("Negated register {}, Result: {}, Flags: {:?}", register, result, memory.flags);
}

fn set_from_flags(pc: usize, memory: &mut Memory, flag: bool) {
    let byte1 = memory.data[pc + 1];
    let register = (byte1 & 0x0F) as usize;
    memory.registers[register] = flag as u64;
    memory.registers[constants::PC] += 2;

    println!("Set register {} to {}", register, flag as u64);
}

fn exit(exit_code: &mut i64, memory: &mut Memory) {
    *exit_code = memory.registers[0] as i64;
    memory.registers[constants::PC] += 1
}

fn set_flags(memory: &mut Memory, value: i64) {
    memory.flags.zero = value == 0;
    memory.flags.positive = value > 0;
}