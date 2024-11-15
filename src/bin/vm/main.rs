use std::io::{Read, Write};
use std::process::ExitCode;
use crate::constants::{BinopType, JumpType};
use crate::memory::Memory;

mod input;
mod memory;
mod constants;

fn main() -> ExitCode {
    let program = input::read_obj_file();

    let size = 0x100000;
    let mut memory = Memory::new(size, program);
    let exit_code = run(&mut memory);
    println!("Exit code: {}", exit_code);
    ExitCode::from(exit_code)
}

pub fn run(memory: &mut Memory) -> u8 {
    let mut exit_code = 0;
    loop {
        let pc = memory.registers[constants::PC] as usize;
        let opcode = memory.data[pc];
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
            0x12 => jump(pc, memory, JumpType::Jmp),
            0x13 => jump(pc, memory, JumpType::Jz),
            0x14 => jump(pc, memory, JumpType::Jnz),
            0x15 => jump(pc, memory, JumpType::Jg),
            0x16 => jump(pc, memory, JumpType::Jge),
            0x17 => jump(pc, memory, JumpType::Jl),
            0x18 => jump(pc, memory, JumpType::Jle),
            0x19 => call(pc, memory),
            0x1A => ret(memory),
            0x1B => inc_or_dec(pc, memory, 1),
            0x1C => inc_or_dec(pc, memory, -1),
            0x1D => read_stdin(pc, memory),
            0x1E => write_stdout(pc, memory),
            0xFF => {
                exit(&mut exit_code, pc, memory);
                break;
            }
            _ => panic!("Unknown opcode: {}", opcode),
        };
        memory.print_registers();
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
            memory.read_u64_le(pc + 1, 8)
        }
        1 => {
            memory.registers[constants::PC] += 1;
            let register = (first_byte & 0b11110000) >> 4;
            memory.registers[register as usize]
        }
        2 => {
            memory.registers[constants::PC] += 9;
            let register = (first_byte & 0b11110000) >> 4;
            let offset = memory.read_u64_le(pc + 1, 8);
            if offset as i64 >= 0 {
                memory.registers[register as usize] + offset
            } else {
                memory.registers[register as usize] - offset
            }
        }
        3 => {
            memory.registers[constants::PC] += 10;
            let byte3 = memory.data[pc + 1];
            let scale_register = byte3 & 0b00001111;
            let register = (byte3 & 0b11110000) >> 4;
            let offset = memory.read_u64_le(pc + 2, 8);
            let signed_offset = offset as i64 * memory.registers[scale_register as usize] as i64;
            if signed_offset >= 0 {
                memory.registers[register as usize] + signed_offset as u64
            } else {
                memory.registers[register as usize] - signed_offset as u64
            }
        }
        _ => panic!("Invalid address type: {}", address_type),
    };
    address
}

fn load(pc: usize, memory: &mut Memory) {
    let byte1 = memory.data[pc + 1];

    let byte_count = 1 << (byte1 & 0b00000011);
    let register = (byte1 & 0b11110000) >> 4;

    let address = read_address(pc + 2, memory);
    let value = memory.read_u64_le(address as usize, byte_count);

    memory.registers[register as usize] = value;
    memory.registers[constants::PC] += 2;

    println!(
        "Loaded {} ({} bytes) from {} into register {}",
        value as i64, byte_count, address, register
    );
}

fn store(pc: usize, memory: &mut Memory) {
    let byte1 = memory.data[pc + 1];

    let byte_count = 1 << (byte1 & 0b00000011);
    let register = (byte1 & 0b11110000) >> 4;

    let address = read_address(pc + 2, memory);
    let value = memory.registers[register as usize];

    memory.write_u64_le(address as usize, value, byte_count);
    memory.registers[constants::PC] += 2;

    println!(
        "Stored {} ({} bytes) from register {} into {}",
        value as i64, byte_count, register, address
    );
}

fn set(pc: usize, memory: &mut Memory) {
    let byte1 = memory.data[pc + 1];
    let register = byte1;
    let value = memory.read_u64_le(pc + 2, 8);

    memory.registers[register as usize] = value;
    memory.registers[constants::PC] += 10;

    println!("Set register {} to {}", register, value as i64);
}

fn push(pc: usize, memory: &mut Memory) {
    let byte1 = memory.data[pc + 1];
    let byte_count = 1 << (byte1 & 0b00000011);
    let register = (byte1 & 0b11110000) >> 4;

    memory.registers[constants::SP] -= byte_count;
    let address = memory.registers[constants::SP];

    let value = memory.registers[register as usize];
    memory.write_u64_le(address as usize, value, byte_count as usize);
    memory.registers[constants::PC] += 2;

    println!("Pushed register {} with value {} ({} bytes) onto the stack", register, byte_count, value);
}

fn pop(pc: usize, memory: &mut Memory) {
    let byte1 = memory.data[pc + 1];
    let byte_count = 1 << (byte1 & 0b00000011);
    let register = (byte1 & 0b11110000) >> 4;

    let address = memory.registers[constants::SP];
    let value = memory.read_u64_le(address as usize, byte_count);

    memory.registers[register as usize] = value;
    memory.registers[constants::SP] += byte_count as u64;
    memory.registers[constants::PC] += 2;

    println!("Popped {} bytes from the stack into register {} with value {}", byte_count, register, value);
}

fn binop(pc: usize, memory: &mut Memory, op_type: BinopType) {
    let byte1 = memory.data[pc + 1];
    let reg1 = (byte1 & 0x0F) as usize;
    let reg2 = (byte1 & 0xF0) >> 4;

    let value1 = memory.registers[reg1] as i64;
    let value2 = memory.registers[reg2 as usize] as i64;

    let result = match op_type {
        BinopType::Add => value1 + value2,
        BinopType::Sub => value1 - value2,
        BinopType::Mul => value1 * value2,
        BinopType::Div => value1 / value2,
        BinopType::Mod => value1 % value2,
        BinopType::And => value1 & value2,
        BinopType::Or => value1 | value2,
        BinopType::Xor => value1 ^ value2,
        BinopType::Shl => value1 << value2,
        BinopType::Shr => value1 >> value2,
    };

    set_flags(memory, result);

    memory.registers[reg1] = result as u64;
    memory.registers[constants::PC] += 2;

    println!(
        "Performed {:?} on {} and {} ({} and {}), Result: {}, Flags: {:?}",
        op_type, value1, value2, reg1, reg2, result, memory.flags
    );
}

fn compare(pc: usize, memory: &mut Memory) {
    let byte1 = memory.data[pc + 1];
    let reg1 = (byte1 & 0x0F) as usize;
    let reg2 = (byte1 & 0xF0) >> 4;

    let value1 = memory.registers[reg1];
    let value2 = memory.registers[reg2 as usize];

    memory.flags.zero = value1 == value2;
    memory.flags.positive = value1 > value2;

    memory.registers[constants::PC] += 2;

    println!("Compared {} and {} ({} and {}), Flags: {:?}", value1, value2, reg1, reg2, memory.flags);
}

fn jump(pc: usize, memory: &mut Memory, jump_type: JumpType) {
    let should_jump = match jump_type {
        JumpType::Jmp => true,
        JumpType::Jz => memory.flags.zero,
        JumpType::Jnz => !memory.flags.zero,
        JumpType::Jg => memory.flags.positive,
        JumpType::Jge => memory.flags.positive || memory.flags.zero,
        JumpType::Jl => !memory.flags.positive,
        JumpType::Jle => !memory.flags.positive || memory.flags.zero,
    };

    let address = memory.read_u64_le(pc + 1, 8);

    if should_jump {
        memory.registers[constants::PC] = address;
        println!("Jumped to address {} because {:?}", address, jump_type);
    } else {
        memory.registers[constants::PC] += 9;
        println!("Did not jump to address {} because {:?}", address, jump_type);
    }
}

fn call(pc: usize, memory: &mut Memory) {
    let address = memory.read_u64_le(pc + 1, 8);
    memory.registers[constants::SP] -= 8;
    let return_pc = (pc + 9) as u64;
    memory.write_u64_le(memory.registers[constants::SP] as usize, return_pc, 8);
    memory.registers[constants::PC] = address;

    println!("Called Address {}, Return PC: {}", address, return_pc);
}

fn ret(memory: &mut Memory) {
    let address = memory.read_u64_le(memory.registers[constants::SP] as usize, 8);
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
    let read_bytes = memory.read_u64_le(pc + 1, 8) as usize;
    let address = read_address(pc + 9, memory) as usize;

    let mut buffer = vec![0; read_bytes];
    std::io::stdin().read_exact(&mut buffer).unwrap();
    memory.write_bytes(address, &buffer);
    memory.registers[constants::PC] += 9;

    println!("Read {} bytes from stdin into address {}", read_bytes, address);
}

fn write_stdout(pc: usize, memory: &mut Memory) {
    let write_bytes = memory.read_u64_le(pc + 1, 8) as usize;
    let address = read_address(pc + 9, memory) as usize;

    let buffer = memory.read_bytes(address, write_bytes);
    std::io::stdout().write_all(&buffer).unwrap();
    memory.registers[constants::PC] += 9;

    println!("Wrote {} bytes to stdout from address {}", write_bytes, address);
}

fn exit(exit_code: &mut u8, pc: usize, memory: &mut Memory) {
    let byte1 = memory.data[pc + 1];
    let register = (byte1 & 0x0F) as usize;
    *exit_code = memory.registers[register] as u8;
    memory.registers[constants::PC] += 2
}

fn set_flags(memory: &mut Memory, value: i64) {
    memory.flags.zero = value == 0;
    memory.flags.positive = value > 0;
}