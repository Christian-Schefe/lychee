use lychee_compiler::{DATA_SIZE_32, DATA_SIZE_64};
use crate::core::memory::Memory;

#[derive(Debug)]
pub struct Heap {
    pub offset: usize,
    pub size: usize,
    pub header_size: usize,
}

/*pub struct Block {
    pub size: u64,
    pub next: u32,
    pub prev: u32,
}*/

impl Heap {
    pub fn new(memory: &mut Memory, offset: usize, size: usize) -> Heap {
        memory.write_i64_le(offset, size as i64, DATA_SIZE_64);
        memory.write_i64_le(offset + 8, -1, DATA_SIZE_32);
        memory.write_i64_le(offset + 12, -1, DATA_SIZE_32);
        Heap {
            offset,
            size,
            header_size: 16,
        }
    }
    pub fn malloc(&mut self, memory: &mut Memory, size: u64) -> Option<usize> {
        let block_size = size + self.header_size as u64;
        let block_address = self.find_free_block(memory, block_size)?;
        self.split_block(memory, block_address, block_size);
        self.toggle_free(memory, block_address);
        Some(block_address + self.header_size)
    }

    pub fn free(&mut self, memory: &mut Memory, address: usize) {
        let block_address = address - self.header_size;
        self.toggle_free(memory, block_address);
        self.merge_blocks(memory, block_address);
    }

    fn find_free_block(&self, memory: &Memory, size: u64) -> Option<usize> {
        let mut current = self.offset;
        loop {
            let block_size = memory.read_i64_le(current, DATA_SIZE_64);
            if block_size >= size as i64 {
                return Some(current);
            }
            let next = memory.read_i64_le(current + 8, DATA_SIZE_32);
            if next == -1 {
                return None;
            }
            current = next as usize;
        }
    }

    fn split_block(&self, memory: &mut Memory, block_address: usize, size: u64) {
        let block_size = memory.read_i64_le(block_address, DATA_SIZE_64);
        if block_size == size as i64 {
            return;
        }
        let next = memory.read_i64_le(block_address + 8, DATA_SIZE_32);
        let prev = memory.read_i64_le(block_address + 12, DATA_SIZE_32);
        let new_block_address = block_address + size as usize;
        let new_block_size = block_size - size as i64;
        memory.write_i64_le(block_address, size as i64, DATA_SIZE_64);
        memory.write_i64_le(block_address + 8, new_block_address as i64, DATA_SIZE_32);
        memory.write_i64_le(block_address + 12, prev, DATA_SIZE_32);
        memory.write_i64_le(new_block_address, new_block_size, DATA_SIZE_64);
        memory.write_i64_le(new_block_address + 8, next, DATA_SIZE_32);
        memory.write_i64_le(new_block_address + 12, block_address as i64, DATA_SIZE_32);
    }

    fn merge_blocks(&self, memory: &mut Memory, block_address: usize) {
        let block_size = memory.read_i64_le(block_address, DATA_SIZE_64);
        let next = memory.read_i64_le(block_address + 8, DATA_SIZE_32);
        if next != -1 {
            let next_block_address = next as usize;
            let next_block_size = memory.read_i64_le(next_block_address, DATA_SIZE_64);
            if next_block_size > 0 {
                memory.write_i64_le(block_address, block_size + next_block_size, DATA_SIZE_64);
                let new_next = memory.read_i64_le(next_block_address + 8, DATA_SIZE_32);
                memory.write_i64_le(block_address + 8, new_next, DATA_SIZE_32);
                if new_next != -1 {
                    memory.write_i64_le(new_next as usize + 12, block_address as i64, DATA_SIZE_32);
                }
            }
        }

        let block_size = memory.read_i64_le(block_address, DATA_SIZE_64);
        let prev = memory.read_i64_le(block_address + 12, DATA_SIZE_32);
        if prev != -1 {
            let prev_block_address = prev as usize;
            let prev_block_size = memory.read_i64_le(prev_block_address, DATA_SIZE_64);
            if prev_block_size > 0 {
                memory.write_i64_le(
                    prev_block_address,
                    prev_block_size + block_size,
                    DATA_SIZE_64,
                );
                let new_next = memory.read_i64_le(block_address + 8, DATA_SIZE_32);
                memory.write_i64_le(prev_block_address + 8, new_next, DATA_SIZE_32);
                if new_next != -1 {
                    memory.write_i64_le(
                        new_next as usize + 12,
                        prev_block_address as i64,
                        DATA_SIZE_32,
                    );
                }
            }
        }
    }

    fn toggle_free(&mut self, memory: &mut Memory, block_address: usize) {
        let block_size = memory.read_i64_le(block_address, DATA_SIZE_64);
        memory.write_i64_le(block_address, -block_size, DATA_SIZE_64);
    }

    pub fn print_blocks(&self, memory: &Memory) {
        let mut current = self.offset;
        loop {
            let block_size = memory.read_i64_le(current, DATA_SIZE_64);
            let next = memory.read_i64_le(current + 8, DATA_SIZE_32);
            let prev = memory.read_i64_le(current + 12, DATA_SIZE_32);
            println!(
                "Block: {:?}, Size: {:?}, Next: {:?}, Prev: {:?}",
                current, block_size, next, prev
            );
            if block_size.abs() < 1000 {
                let content = memory.data[current..current + block_size.abs() as usize].to_vec();
                println!("Content: {:?}", content);
            }
            if next == -1 {
                break;
            }
            current = next as usize;
        }
    }
}
