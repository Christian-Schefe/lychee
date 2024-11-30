use crate::memory::Memory;
use clap::Parser;
use lychee_compiler::{BinopType, UnopType, DATA_SIZE_64};
use std::io::{Read, Write};
use std::path::PathBuf;

mod constants;
mod input;
mod memory;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    input: PathBuf,
    #[arg(short, long, default_value("false"))]
    debug_print: bool,
}

fn main() {
    let args = Args::parse();
    let program = input::read_obj_file(&args.input);

    let size = 0x100000;
    let mut memory = Memory::new(size, program);
    let start_instant = std::time::Instant::now();
    let exit_code = run(&mut memory, args.debug_print);
    let elapsed = start_instant.elapsed();
    println!("Elapsed: {:?}", elapsed);
    println!("Main return value: {}", exit_code);
}

pub fn run(memory: &mut Memory, debug_print: bool) -> i64 {
    let mut exit_code = 0;
    loop {
        let pc = memory.registers[constants::PC] as usize;
        let opcode = memory.data[pc];
        if debug_print {
            println!("PC: {}, Opcode: {:X}", pc, opcode);
        }
        match opcode {
            0x00 => {
                exit(&mut exit_code, memory, debug_print);
                break;
            }
            0x01 => load(pc, memory, debug_print),
            0x02 => store(pc, memory, debug_print),
            0x03 => push(pc, memory, debug_print),
            0x04 => pop(pc, memory, debug_print),

            0x05 => binop(pc, memory, BinopType::Mov, false, debug_print),
            0x06 => binop(pc, memory, BinopType::Add, false, debug_print),
            0x07 => binop(pc, memory, BinopType::Sub, false, debug_print),
            0x08 => binop(pc, memory, BinopType::Mul, false, debug_print),
            0x09 => binop(pc, memory, BinopType::Div, false, debug_print),
            0x0A => binop(pc, memory, BinopType::Mod, false, debug_print),
            0x0B => binop(pc, memory, BinopType::And, false, debug_print),
            0x0C => binop(pc, memory, BinopType::Or, false, debug_print),
            0x0D => binop(pc, memory, BinopType::Xor, false, debug_print),
            0x0E => binop(pc, memory, BinopType::Shl, false, debug_print),
            0x0F => binop(pc, memory, BinopType::Shr, false, debug_print),
            0x10 => binop(pc, memory, BinopType::Cmp, false, debug_print),

            0x11 => binop(pc, memory, BinopType::Mov, true, debug_print),
            0x12 => binop(pc, memory, BinopType::Add, true, debug_print),
            0x13 => binop(pc, memory, BinopType::Sub, true, debug_print),
            0x14 => binop(pc, memory, BinopType::Mul, true, debug_print),
            0x15 => binop(pc, memory, BinopType::Div, true, debug_print),
            0x16 => binop(pc, memory, BinopType::Mod, true, debug_print),
            0x17 => binop(pc, memory, BinopType::And, true, debug_print),
            0x18 => binop(pc, memory, BinopType::Or, true, debug_print),
            0x19 => binop(pc, memory, BinopType::Xor, true, debug_print),
            0x1A => binop(pc, memory, BinopType::Shl, true, debug_print),
            0x1B => binop(pc, memory, BinopType::Shr, true, debug_print),
            0x1C => binop(pc, memory, BinopType::Cmp, true, debug_print),

            0x1D => jump(pc, memory, true, debug_print),
            0x1E => jump(pc, memory, memory.flags.zero, debug_print),
            0x1F => jump(pc, memory, !memory.flags.zero, debug_print),
            0x20 => jump(pc, memory, memory.flags.positive, debug_print),
            0x21 => jump(
                pc,
                memory,
                memory.flags.positive || memory.flags.zero,
                debug_print,
            ),
            0x22 => jump(
                pc,
                memory,
                !memory.flags.positive && !memory.flags.zero,
                debug_print,
            ),
            0x23 => jump(pc, memory, !memory.flags.positive, debug_print),

            0x24 => set(pc, memory, true, debug_print),
            0x25 => set(pc, memory, memory.flags.zero, debug_print),
            0x26 => set(pc, memory, !memory.flags.zero, debug_print),
            0x27 => set(pc, memory, memory.flags.positive, debug_print),
            0x28 => set(
                pc,
                memory,
                memory.flags.positive || memory.flags.zero,
                debug_print,
            ),
            0x29 => set(
                pc,
                memory,
                !memory.flags.positive && !memory.flags.zero,
                debug_print,
            ),
            0x2A => set(pc, memory, !memory.flags.positive, debug_print),

            0x2B => unop(pc, memory, UnopType::Not, debug_print),
            0x2C => unop(pc, memory, UnopType::Neg, debug_print),
            0x2D => unop(pc, memory, UnopType::Inc, debug_print),
            0x2E => unop(pc, memory, UnopType::Dec, debug_print),

            0x2F => call(pc, memory, debug_print),
            0x30 => ret(memory, debug_print),
            0x31 => read_stdin(pc, memory, debug_print),
            0x32 => write_stdout(pc, memory, debug_print),
            0x33 => memory.registers[constants::PC] += 1,
            0x34 => sign_extend(pc, memory, debug_print),
            0x35 => lea(pc, memory, debug_print),
            0x36 => pushmem(pc, memory, debug_print),
            0x37 => popmem(pc, memory, false, debug_print),
            0x38 => popmem(pc, memory, true, debug_print),
            _ => panic!("Unknown opcode: {}", opcode),
        };
        if debug_print {
            memory.print_registers();
            memory.print_stack();
            println!();
        }
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

fn load(pc: usize, memory: &mut Memory, debug_print: bool) {
    let byte1 = memory.data[pc + 1];

    let data_size = 1 << (byte1 & 0b00000011);
    let register = (byte1 & 0b11110000) >> 4;

    let address = read_address(pc + 2, memory);
    let value = memory.read_i64_le(address as usize, data_size);

    memory.registers[register as usize] = value as u64;
    memory.registers[constants::PC] += 2;

    if debug_print {
        println!(
            "Loaded {} ({} bytes) from {} into register {}",
            value, data_size, address, register
        );
    }
}

fn store(pc: usize, memory: &mut Memory, debug_print: bool) {
    let byte1 = memory.data[pc + 1];

    let byte_count = 1 << (byte1 & 0b00000011);
    let register = (byte1 & 0b11110000) >> 4;

    let address = read_address(pc + 2, memory);
    let value = memory.registers[register as usize] as i64;

    memory.write_i64_le(address as usize, value, byte_count);
    memory.registers[constants::PC] += 2;

    if debug_print {
        println!(
            "Stored {} ({} bytes) from register {} into {}",
            value, byte_count, register, address
        );
    }
}

fn push(pc: usize, memory: &mut Memory, debug_print: bool) {
    let byte1 = memory.data[pc + 1];
    let data_size = 1 << (byte1 & 0b00000011);
    let register = (byte1 & 0b11110000) >> 4;

    memory.registers[constants::SP] -= data_size;
    let address = memory.registers[constants::SP];

    let value = memory.registers[register as usize] as i64;
    memory.write_i64_le(address as usize, value, data_size as u8);
    memory.registers[constants::PC] += 2;

    if debug_print {
        println!(
            "Pushed register {} with value {} ({} bytes) onto the stack",
            register, value, data_size,
        );
    }
}

fn pop(pc: usize, memory: &mut Memory, debug_print: bool) {
    let byte1 = memory.data[pc + 1];
    let data_size = 1 << (byte1 & 0b00000011);
    let register = (byte1 & 0b11110000) >> 4;

    let address = memory.registers[constants::SP];
    let value = memory.read_i64_le(address as usize, data_size);

    memory.registers[register as usize] = value as u64;
    memory.registers[constants::SP] += data_size as u64;
    memory.registers[constants::PC] += 2;

    if debug_print {
        println!(
            "Popped {} bytes from the stack into register {} with value {}",
            data_size, register, value
        );
    }
}

fn binop(pc: usize, memory: &mut Memory, op_type: BinopType, immediate: bool, debug_print: bool) {
    let byte1 = memory.data[pc + 1];
    let dest_register = ((byte1 & 0xF0) >> 4) as usize;
    let left_value = memory.registers[dest_register] as i64;

    let right_value = if immediate {
        memory.read_i64_le(pc + 2, DATA_SIZE_64)
    } else {
        let source_register = (byte1 & 0x0F) as usize;
        memory.registers[source_register] as i64
    };

    let result = match op_type {
        BinopType::Mov => right_value,
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
        BinopType::Cmp => left_value - right_value,
    };

    set_flags(memory, result);

    if op_type != BinopType::Cmp {
        memory.registers[dest_register] = result as u64;
    }

    memory.registers[constants::PC] += if immediate { 10 } else { 2 };

    if debug_print {
        if immediate {
            println!(
                "Performed {:?} operation with register {} and immediate value {}, Result: {}, Flags: {:?}",
                op_type, dest_register, right_value, result, memory.flags
            );
        } else {
            println!(
                "Performed {:?} operation with register {} and register {}, Result: {}, Flags: {:?}",
                op_type,
                dest_register,
                byte1 & 0x0F,
                result,
                memory.flags
            );
        }
    }
}

fn jump(pc: usize, memory: &mut Memory, should_jump: bool, debug_print: bool) {
    let address = memory.read_u64_le(pc + 1, DATA_SIZE_64);

    if should_jump {
        memory.registers[constants::PC] = address;
        if debug_print {
            println!("Jumped to address {}", address);
        }
    } else {
        memory.registers[constants::PC] += 9;
        if debug_print {
            println!("Did not jump to address {}", address);
        }
    }
}

fn set(pc: usize, memory: &mut Memory, flag: bool, debug_print: bool) {
    let byte1 = memory.data[pc + 1];
    let register = (byte1 & 0x0F) as usize;
    memory.registers[register] = flag as u64;
    memory.registers[constants::PC] += 2;

    if debug_print {
        println!("Set register {} to {}", register, flag as u64);
    }
}

fn unop(pc: usize, memory: &mut Memory, op_type: UnopType, debug_print: bool) {
    let byte1 = memory.data[pc + 1];
    let register = (byte1 & 0x0F) as usize;
    let value = memory.registers[register] as i64;
    let result = match op_type {
        UnopType::Not => !value,
        UnopType::Neg => -value,
        UnopType::Inc => value + 1,
        UnopType::Dec => value - 1,
    };
    set_flags(memory, result);

    memory.registers[register] = result as u64;
    memory.registers[constants::PC] += 2;

    if debug_print {
        println!(
            "Performed {:?} operation on register {}, Result: {}, Flags: {:?}",
            op_type, register, result, memory.flags
        );
    }
}

fn call(pc: usize, memory: &mut Memory, debug_print: bool) {
    let address = memory.read_u64_le(pc + 1, DATA_SIZE_64);
    memory.registers[constants::SP] -= 8;
    let return_pc = (pc + 9) as u64;
    memory.write_u64_le(
        memory.registers[constants::SP] as usize,
        return_pc,
        DATA_SIZE_64,
    );
    memory.registers[constants::PC] = address;

    if debug_print {
        println!("Called Address {}, Return PC: {}", address, return_pc);
    }
}

fn ret(memory: &mut Memory, debug_print: bool) {
    let address = memory.read_u64_le(memory.registers[constants::SP] as usize, DATA_SIZE_64);
    memory.registers[constants::SP] += 8;
    memory.registers[constants::PC] = address;

    if debug_print {
        println!("Returned to Address {}", address);
    }
}

fn read_stdin(pc: usize, memory: &mut Memory, debug_print: bool) {
    let register = memory.data[pc + 1];
    let read_bytes = memory.registers[register as usize] as usize;
    let address = read_address(pc + 2, memory) as usize;

    let mut buffer = vec![0; read_bytes];
    std::io::stdin().read_exact(&mut buffer).unwrap();
    memory.write_bytes(address, &buffer);
    memory.registers[constants::PC] += 2;

    if debug_print {
        println!(
            "Read {} bytes from stdin into address {}",
            read_bytes, address
        );
    }
}

fn write_stdout(pc: usize, memory: &mut Memory, debug_print: bool) {
    let register = memory.data[pc + 1];
    let write_bytes = memory.registers[register as usize] as usize;
    let address = read_address(pc + 2, memory) as usize;

    let buffer = memory.read_bytes(address, write_bytes);
    std::io::stdout().write_all(&buffer).unwrap();
    memory.registers[constants::PC] += 2;

    let str = std::str::from_utf8(&buffer).unwrap();

    if debug_print {
        println!(
            "Wrote '{}' ({} bytes) to stdout from address {}",
            str, write_bytes, address
        );
    }
}

fn exit(exit_code: &mut i64, memory: &mut Memory, debug_print: bool) {
    *exit_code = memory.registers[0] as i64;
    memory.registers[constants::PC] += 1;

    if debug_print {
        println!("Exiting with code {}", exit_code);
    }
}

fn set_flags(memory: &mut Memory, value: i64) {
    memory.flags.zero = value == 0;
    memory.flags.positive = value > 0;
}

fn sign_extend(pc: usize, memory: &mut Memory, debug_print: bool) {
    let byte1 = memory.data[pc + 1];
    let data_size = 1 << (byte1 & 0b00000011);
    let register = (byte1 & 0b11110000) >> 4;

    let value = memory.registers[register as usize] as i64;

    let extended_value = match data_size {
        1 => value as i8 as i64,
        2 => value as i16 as i64,
        4 => value as i32 as i64,
        8 => value,
        _ => panic!("Invalid data size: {}", data_size),
    };

    memory.registers[register as usize] = extended_value as u64;
    memory.registers[constants::PC] += 2;

    if debug_print {
        println!(
            "Sign extended register {} with value {} ({} bytes)",
            data_size, value, register
        );
    }
}

fn lea(pc: usize, memory: &mut Memory, debug_print: bool) {
    let register = memory.data[pc + 1];
    let address = read_address(pc + 2, memory);

    memory.registers[register as usize] = address;
    memory.registers[constants::PC] += 2;

    if debug_print {
        println!("Loaded address {} into register {}", address, register);
    }
}

fn pushmem(pc: usize, memory: &mut Memory, debug_print: bool) {
    let register = memory.data[pc + 1];
    let data_size = memory.registers[register as usize];

    memory.registers[constants::SP] -= data_size;
    let dest_address = memory.registers[constants::SP] as usize;
    let src_address = read_address(pc + 2, memory) as usize;

    memory.memory_copy(src_address, dest_address, data_size as usize);

    memory.registers[constants::PC] += 2;

    if debug_print {
        println!(
            "Pushed {} bytes from address {} onto the stack at address {}",
            data_size, src_address, dest_address
        );
    }
}

fn popmem(pc: usize, memory: &mut Memory, is_peek: bool, debug_print: bool) {
    let register = memory.data[pc + 1];
    let data_size = memory.registers[register as usize];

    let src_address = memory.registers[constants::SP] as usize;
    let dest_address = read_address(pc + 2, memory) as usize;

    memory.memory_copy(src_address, dest_address, data_size as usize);

    if !is_peek {
        memory.registers[constants::SP] += data_size;
    }
    memory.registers[constants::PC] += 2;

    if debug_print {
        if is_peek {
            println!(
                "Peeked {} bytes from the stack at address {} into address {}",
                data_size, src_address, dest_address
            );
        } else {
            println!(
                "Popped {} bytes from the stack at address {} into address {}",
                data_size, src_address, dest_address
            );
        }
    }
}
