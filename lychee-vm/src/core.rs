use crate::core::heap::Heap;
use crate::core::memory::Memory;
use lychee_compiler::{BinopType, UnopType, DATA_SIZE_64};
use std::io::{Read, Write};

pub mod constants;
pub mod heap;
pub mod memory;

pub fn execute(program: Vec<u8>, debug_print: bool) {
    let size = 0x200000;
    let heap_size = 0x100000;
    let heap_offset = program.len();
    let mut memory = Memory::new(size, program);
    let mut heap = Heap::new(&mut memory, heap_offset, heap_size);
    let start_instant = std::time::Instant::now();
    let exit_code = run(&mut memory, &mut heap, debug_print);
    if debug_print {
        let elapsed = start_instant.elapsed();
        println!("Elapsed: {:?}", elapsed);
        println!("Exit Code: {}", exit_code);
    }
}

pub fn run(memory: &mut Memory, heap: &mut Heap, debug_print: bool) -> i64 {
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
            0x01 => load_or_store(pc, memory, true, debug_print),
            0x02 => load_or_store(pc, memory, false, debug_print),
            0x03 => push_or_pop(pc, memory, true, debug_print),
            0x04 => push_or_pop(pc, memory, false, debug_print),

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
            0x33 => rand(pc, memory, debug_print),
            0x34 => sign_extend(pc, memory, debug_print),
            0x35 => lea(pc, memory, debug_print),
            0x36 => pushmem(pc, memory, debug_print),
            0x37 => popmem(pc, memory, false, debug_print),
            0x38 => popmem(pc, memory, true, debug_print),
            0x39 => alloc(pc, memory, heap, debug_print),
            0x3A => free(pc, memory, heap, debug_print),
            0x3B => file_open(pc, memory, debug_print),
            0x3C => file_close(pc, memory, debug_print),
            0x3D => file_read(pc, memory, debug_print),
            0x3E => file_write(pc, memory, debug_print),
            0x3F => mem_copy(pc, memory, debug_print),
            _ => panic!("Unknown opcode: {}", opcode),
        };
        if debug_print {
            memory.print_registers();
            memory.print_stack();
            println!();
        }
    }
    if debug_print {
        heap.print_blocks(&memory);
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

fn load_or_store(pc: usize, memory: &mut Memory, is_load: bool, debug_print: bool) {
    let byte1 = memory.data[pc + 1];
    let data_size = 1 << ((byte1 & 0x30) >> 4);
    let register = (byte1 & 0x0F) as usize;
    let address = read_address(pc + 2, memory);

    if is_load {
        let value = memory.read_i64_le(address as usize, data_size);
        memory.registers[register] = value as u64;
    } else {
        let value = memory.registers[register] as i64;
        memory.write_i64_le(address as usize, value, data_size);
    }

    memory.registers[constants::PC] += 2;

    if debug_print {
        let bytes = memory.read_bytes(address as usize, data_size as usize);
        if is_load {
            println!(
                "Loaded {} bytes ({:?}) from address {} into register {}",
                data_size, bytes, address, register
            );
        } else {
            println!(
                "Stored {} bytes from register {} into address {}",
                data_size, register, address
            );
        }
    }
}

fn push_or_pop(pc: usize, memory: &mut Memory, is_push: bool, debug_print: bool) {
    let byte1 = memory.data[pc + 1];
    let data_size = 1 << ((byte1 & 0x30) >> 4);
    let register = (byte1 & 0x0F) as usize;

    if is_push {
        memory.registers[constants::SP] -= data_size as u64;
        let address = memory.registers[constants::SP];
        let value = memory.registers[register] as i64;
        memory.write_i64_le(address as usize, value, data_size);
    } else {
        let address = memory.registers[constants::SP];
        let value = memory.read_i64_le(address as usize, data_size);
        memory.registers[register] = value as u64;
        memory.registers[constants::SP] += data_size as u64;
    }
    memory.registers[constants::PC] += 2;

    if debug_print {
        if is_push {
            println!(
                "Pushed {} bytes from register {} onto the stack at address {}",
                data_size,
                register,
                memory.registers[constants::SP]
            );
        } else {
            println!(
                "Popped {} bytes from the stack at address {} into register {}",
                data_size,
                memory.registers[constants::SP],
                register
            );
        }
    }
}

fn binop(pc: usize, memory: &mut Memory, op_type: BinopType, immediate: bool, debug_print: bool) {
    let byte1 = memory.data[pc + 1];
    let dest_register = (byte1 & 0x0F) as usize;
    let left_value = memory.registers[dest_register] as i64;

    let right_value = if immediate {
        memory.read_i64_le(pc + 2, DATA_SIZE_64)
    } else {
        let source_register = ((byte1 & 0xF0) >> 4) as usize;
        memory.registers[source_register] as i64
    };

    let result = match op_type {
        BinopType::Mov => right_value,
        BinopType::Add => left_value + right_value,
        BinopType::Sub | BinopType::Cmp => left_value - right_value,
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
                ((byte1 & 0xF0) >> 4) as usize,
                result,
                memory.flags
            );
        }
    }
}

fn jump(pc: usize, memory: &mut Memory, should_jump: bool, debug_print: bool) {
    let address = read_address(pc + 1, memory);

    if should_jump {
        memory.registers[constants::PC] = address;
        if debug_print {
            println!("Jumped to address {}", address);
        }
    } else {
        memory.registers[constants::PC] += 1;
        if debug_print {
            println!("Did not jump to address {}", address);
        }
    }
}

fn set(pc: usize, memory: &mut Memory, flag: bool, debug_print: bool) {
    let register = memory.data[pc + 1] as usize;
    memory.registers[register] = flag as u64;
    memory.registers[constants::PC] += 2;

    if debug_print {
        println!("Set register {} to {}", register, flag as u64);
    }
}

fn unop(pc: usize, memory: &mut Memory, op_type: UnopType, debug_print: bool) {
    let register = memory.data[pc + 1] as usize;
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
    let address = read_address(pc + 1, memory);
    memory.registers[constants::SP] -= 8;
    let return_pc = memory.registers[constants::PC] + 1;
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

fn rand(pc: usize, memory: &mut Memory, debug_print: bool) {
    let register = memory.data[pc + 1];
    let value = rand::random::<u64>();
    memory.registers[register as usize] = value;
    memory.registers[constants::PC] += 2;

    if debug_print {
        println!(
            "Generated random number {} and stored it in register {}",
            value, register
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
    let data_size = 1 << ((byte1 & 0x30) >> 4);
    let register = byte1 & 0x0F;

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
            register, value, data_size
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

fn alloc(pc: usize, memory: &mut Memory, heap: &mut Heap, debug_print: bool) {
    let byte1 = memory.data[pc + 1];
    let size_register = (byte1 & 0x0F) as usize;
    let size = memory.registers[size_register] as i64;

    let address_register = ((byte1 & 0xF0) >> 4) as usize;

    let address = heap.malloc(memory, size as u64).unwrap_or_else(|| {
        heap.print_blocks(memory);
        panic!("Failed to allocate {} bytes", size);
    });
    memory.registers[address_register] = address as u64;
    memory.registers[constants::PC] += 2;

    if debug_print {
        println!("Allocated {} bytes at address {}", size, address);
    }
}

fn free(pc: usize, memory: &mut Memory, heap: &mut Heap, debug_print: bool) {
    let address_register = memory.data[pc + 1] as usize;
    let address = memory.registers[address_register] as usize;

    heap.free(memory, address);
    memory.registers[constants::PC] += 2;

    if debug_print {
        println!("Freed memory at address {}", address);
    }
}

fn file_open(pc: usize, memory: &mut Memory, debug_print: bool) {
    let register = memory.data[pc + 1];
    let address = read_address(pc + 2, memory) as usize;

    let filename = memory.read_string(address);

    let file = std::fs::OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .open(&filename)
        .unwrap();

    let file_id = memory.files.len();
    memory.files.push(Some(file));
    memory.registers[register as usize] = file_id as u64;
    memory.registers[constants::PC] += 2;

    if debug_print {
        println!("Opened file '{}' with ID {}", filename, file_id);
    }
}

fn file_close(pc: usize, memory: &mut Memory, debug_print: bool) {
    let register = memory.data[pc + 1] as usize;
    let file_id = memory.registers[register] as usize;
    let file = std::mem::replace(&mut memory.files[file_id], None).unwrap();
    file.sync_all().unwrap();
    file.metadata().unwrap();
    file.sync_data().unwrap();
    drop(file);
    memory.registers[constants::PC] += 2;
    let mut new_len = memory.files.len();
    while new_len > 0 && memory.files[new_len - 1].is_none() {
        new_len -= 1;
    }
    memory.files.truncate(new_len);

    if debug_print {
        println!("Closed file with ID {}", file_id);
    }
}

fn file_read(pc: usize, memory: &mut Memory, debug_print: bool) {
    let byte1 = memory.data[pc + 1];
    let file_id_register = (byte1 & 0x0F) as usize;
    let size_register = ((byte1 & 0xF0) >> 4) as usize;
    let file_id = memory.registers[file_id_register] as usize;
    let size = memory.registers[size_register] as usize;
    let address = read_address(pc + 2, memory) as usize;

    let file = memory.files[file_id].as_mut().unwrap();
    let mut buffer = vec![0; size];
    file.read_exact(&mut buffer).unwrap();
    memory.write_bytes(address, &buffer);
    memory.registers[constants::PC] += 2;

    if debug_print {
        println!(
            "Read {} bytes from file with ID {} into address {}",
            size, file_id, address
        );
    }
}

fn file_write(pc: usize, memory: &mut Memory, debug_print: bool) {
    let byte1 = memory.data[pc + 1];
    let file_id_register = (byte1 & 0x0F) as usize;
    let size_register = ((byte1 & 0xF0) >> 4) as usize;
    let file_id = memory.registers[file_id_register] as usize;
    let size = memory.registers[size_register] as usize;
    let address = read_address(pc + 2, memory) as usize;

    let buffer = memory.read_bytes(address, size);
    let file = memory.files[file_id].as_mut().unwrap();
    file.write_all(&buffer).unwrap();
    memory.registers[constants::PC] += 2;

    if debug_print {
        println!(
            "Wrote {} bytes to file with ID {} from address {}",
            size, file_id, address
        );
    }
}

fn mem_copy(pc: usize, memory: &mut Memory, debug_print: bool) {
    let size_register = memory.data[pc + 1];
    let size = memory.registers[size_register as usize] as usize;
    let dest_address = read_address(pc + 2, memory) as usize;
    let new_pc = memory.registers[constants::PC] as usize;
    let src_address = read_address(new_pc + 2, memory) as usize;

    memory.memory_copy(src_address, dest_address, size);
    memory.registers[constants::PC] += 2;

    if debug_print {
        println!(
            "Copied {} bytes from address {} to address {}",
            size, src_address, dest_address
        );
    }
}
