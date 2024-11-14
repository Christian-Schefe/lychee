use crate::instructions::{BinopType, JumpType};
use std::process::ExitCode;

mod input;
mod instructions;
mod memory;
mod registers;

fn main() -> ExitCode {
    let program = input::read_obj_file();

    let size = 0x100000;
    let mut memory = memory::Memory::new(size, program);
    let exit_code = run(&mut memory);
    println!("Exit code: {}", exit_code);
    ExitCode::from(exit_code)
}

pub fn run(memory: &mut memory::Memory) -> u8 {
    let mut exit_code = 0;
    loop {
        let pc = memory.registers[registers::PC] as usize;
        let opcode = memory.data[pc];
        println!("PC: {}, Opcode: {}", pc, opcode);
        match opcode {
            0x00 => memory.registers[registers::PC] += 1,
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
            0xFF => {
                exit(&mut exit_code, pc, memory);
                break;
            }
            _ => panic!("Unknown opcode: {}", opcode),
        };
        println!("Registers: {:?}\n", memory.registers);
    }
    exit_code
}

fn load(pc: usize, memory: &mut memory::Memory) {
    let byte1 = memory.data[pc + 1];
    let address = memory.read_u64_le(pc + 2, 8);

    let size = byte1 & 0b00000011;
    let register = (byte1 & 0b11110000) >> 4;

    println!(
        "Load: Size: {}, Register: {}, Address: {}",
        size, register, address
    );

    let value = match size {
        0 => memory.data[address as usize] as i64,
        1 => memory.read_i64_le(address as usize, 2),
        2 => memory.read_i64_le(address as usize, 4),
        3 => memory.read_i64_le(address as usize, 8),
        _ => panic!("Invalid size"),
    };

    println!("Value: {}", value);

    memory.registers[register as usize] = value;
    memory.registers[registers::PC] += 10
}

fn store(pc: usize, memory: &mut memory::Memory) {
    let byte1 = memory.data[pc + 1];
    let address = memory.read_u64_le(pc + 2, 8);

    let size = (byte1 & 0b00000011) as usize;
    let register = (byte1 & 0b11110000) >> 4;

    println!(
        "Store: Size: {}, Register: {}, Address: {}",
        size, register, address
    );

    let bytes = memory.registers[register as usize].to_le_bytes();
    let byte_count = match size {
        0 => 1,
        1 => 2,
        2 => 4,
        3 => 8,
        _ => panic!("Invalid size"),
    };

    println!("Bytes: {:?}", &bytes[..byte_count]);

    memory.data[address as usize..address as usize + byte_count]
        .copy_from_slice(&bytes[..byte_count]);
    memory.registers[registers::PC] += 10
}

fn set(pc: usize, memory: &mut memory::Memory) {
    let byte1 = memory.data[pc + 1];
    let byte_count = (byte1 & 0b00000111) as usize + 1;
    let register = (byte1 & 0b11110000) >> 4;
    let value = memory.read_i64_le(pc + 2, byte_count);

    println!("Set: Register: {}, Value: {}", register, value);

    memory.registers[register as usize] = value;
    memory.registers[registers::PC] += (2 + byte_count) as i64;
}

fn push(pc: usize, memory: &mut memory::Memory) {
    let byte1 = memory.data[pc + 1];
    let size = (byte1 & 0b00000011) as usize;
    let register = (byte1 & 0b11110000) >> 4;
    let value = memory.registers[register as usize];
    let byte_count = match size {
        0 => 1,
        1 => 2,
        2 => 4,
        3 => 8,
        _ => panic!("Invalid size"),
    };

    println!("Push: Register: {}, Value: {}", register, value);

    memory.registers[registers::SP] -= byte_count;
    let address = memory.registers[registers::SP];
    memory.write_i64_le(address as usize, value, byte_count as usize);
    memory.registers[registers::PC] += 2
}

fn pop(pc: usize, memory: &mut memory::Memory) {
    let byte1 = memory.data[pc + 1];
    let size = (byte1 & 0b00000011) as usize;
    let register = (byte1 & 0b11110000) >> 4;
    let byte_count = match size {
        0 => 1,
        1 => 2,
        2 => 4,
        3 => 8,
        _ => panic!("Invalid size"),
    };

    let address = memory.registers[registers::SP];
    let value = memory.read_i64_le(address as usize, byte_count);

    println!("Pop: Register: {}, Value: {}", register, value);

    memory.registers[register as usize] = value;
    memory.registers[registers::SP] += byte_count as i64;
    memory.registers[registers::PC] += 2
}

fn binop(pc: usize, memory: &mut memory::Memory, op_type: BinopType) {
    let byte1 = memory.data[pc + 1];
    let reg1 = (byte1 & 0x0F) as usize;
    let reg2 = (byte1 & 0xF0) >> 4;

    let value1 = memory.registers[reg1];
    let value2 = memory.registers[reg2 as usize];

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

    println!(
        "Binop {:?}: Register 1: {}, Register 2: {}, Result: {}",
        op_type, reg1, reg2, result
    );

    if result == 0 {
        memory.flags |= 1;
    } else {
        memory.flags &= !1;
    }

    if result > 0 {
        memory.flags |= 2;
    } else {
        memory.flags &= !2;
    }

    memory.registers[reg1] = result;
    memory.registers[registers::PC] += 2
}

fn compare(pc: usize, memory: &mut memory::Memory) {
    let byte1 = memory.data[pc + 1];
    let reg1 = (byte1 & 0x0F) as usize;
    let reg2 = (byte1 & 0xF0) >> 4;

    let value1 = memory.registers[reg1];
    let value2 = memory.registers[reg2 as usize];

    if value1 == value2 {
        memory.flags |= 1;
    } else {
        memory.flags &= !1;
    }

    if value1 > value2 {
        memory.flags |= 2;
    } else {
        memory.flags &= !2;
    }

    memory.registers[registers::PC] += 2
}

fn jump(pc: usize, memory: &mut memory::Memory, jump_type: JumpType) {
    let should_jump = match jump_type {
        JumpType::Jmp => true,
        JumpType::Jz => memory.flags & 1 != 0,
        JumpType::Jnz => memory.flags & 1 == 0,
        JumpType::Jg => memory.flags & 2 != 0,
        JumpType::Jge => memory.flags & 2 != 0 || memory.flags & 1 != 0,
        JumpType::Jl => memory.flags & 2 == 0,
        JumpType::Jle => memory.flags & 2 == 0 || memory.flags & 1 != 0,
    };

    let address = memory.read_i64_le(pc + 1, 8);
    if should_jump {
        memory.registers[registers::PC] = address;
    } else {
        memory.registers[registers::PC] += 9;
    }
}

fn exit(exit_code: &mut u8, pc: usize, memory: &mut memory::Memory) {
    let byte1 = memory.data[pc + 1];
    let register = (byte1 & 0x0F) as usize;
    *exit_code = memory.registers[register] as u8;
    memory.registers[registers::PC] += 2
}
