use std::collections::HashMap;
use std::iter::Iterator;
use lazy_static::lazy_static;
use lychee_vm::{OpCode, RegisterCode};
use crate::instruction::Instruction;
use crate::instruction_type::InstructionType;

extern crate lychee_vm;

mod instruction_type;
mod instruction;

fn main() {
    let args = std::env::args().collect::<Vec<String>>();
    let path = &args[1];
    let out_path = &args[2];
    let str = std::fs::read_to_string(path).unwrap();
    let instructions = str.lines().filter(|line| !line.is_empty()).map(|line| convert_line(line)).collect::<Vec<Instruction>>();
    instructions.iter().for_each(|instr| println!("{:?}", instr));
    let bytes = instructions_to_bytes(instructions);

    std::fs::write(out_path, bytes).unwrap();
}

fn convert_line(line: &str) -> Instruction {
    let parts = line.split_whitespace().collect::<Vec<&str>>();

    if parts[0].ends_with(":") {
        let label_str = parts[0].split(":").collect::<Vec<&str>>()[0];
        return Instruction::Label(label_str.to_string());
    }

    let opcode = OPCODE_MAP.get(parts[0]).cloned().unwrap();

    let instruction = match opcode {
        OpCode::Nop | OpCode::Ret | OpCode::Exit => InstructionType::parse_simple(opcode),
        OpCode::Store | OpCode::Load => InstructionType::parse_register_size_address(opcode, parts),
        OpCode::Push | OpCode::Pop => InstructionType::parse_size_register(opcode, parts),
        OpCode::Add | OpCode::Sub | OpCode::Mul | OpCode::Div | OpCode::Mod | OpCode::And | OpCode::Or | OpCode::Xor | OpCode::Shl | OpCode::Shr | OpCode::Cmp | OpCode::Move => InstructionType::parse_two_registers(opcode, parts),
        OpCode::Call | OpCode::Jmp | OpCode::Jz | OpCode::Jnz | OpCode::Jg | OpCode::Jge | OpCode::Jl | OpCode::Jle => InstructionType::parse_label(opcode, parts),
        OpCode::Set => InstructionType::parse_register_immediate(opcode, parts),
        OpCode::Not | OpCode::Inc | OpCode::Dec | OpCode::Neg | OpCode::SetZ | OpCode::SetNz | OpCode::SetG | OpCode::SetGe | OpCode::SetL | OpCode::SetLe => InstructionType::parse_register(opcode, parts),
        OpCode::ReadStdin | OpCode::WriteStdout => InstructionType::parse_register_address(opcode, parts),
    };

    Instruction::Instr(instruction)
}

fn instructions_to_bytes(instructions: Vec<Instruction>) -> Vec<u8> {
    let mut bytes = Vec::new();
    let mut labels: HashMap<String, u64> = HashMap::new();
    let mut label_placeholders: HashMap<String, Vec<u64>> = HashMap::new();

    for instruction in instructions {
        match instruction {
            Instruction::Label(label) => {
                let label_address = bytes.len() as u64;
                if let Some(spots) = label_placeholders.get(&label) {
                    for &spot in spots {
                        let address_bytes: [u8; 8] = label_address.to_le_bytes();
                        for i in 0..8 {
                            bytes[spot as usize + i] = address_bytes[i];
                        }
                    }
                }
                label_placeholders.remove(&label);
                if let Some(_) = labels.insert(label, label_address) {
                    panic!("Label already exists");
                }
            }
            Instruction::Instr(instr) => {
                instr.add_bytes(&mut bytes, &labels, &mut label_placeholders);
            }
        }
    }

    bytes
}

lazy_static! {
    static ref OPCODE_MAP: HashMap<String, OpCode> = {
        HashMap::from([
            ("nop".to_string(), OpCode::Nop),
            ("load".to_string(), OpCode::Load),
            ("store".to_string(), OpCode::Store),
            ("set".to_string(), OpCode::Set),
            ("push".to_string(), OpCode::Push),
            ("pop".to_string(), OpCode::Pop),
            ("add".to_string(), OpCode::Add),
            ("sub".to_string(), OpCode::Sub),
            ("mul".to_string(), OpCode::Mul),
            ("div".to_string(), OpCode::Div),
            ("mod".to_string(), OpCode::Mod),
            ("and".to_string(), OpCode::And),
            ("or".to_string(), OpCode::Or),
            ("xor".to_string(), OpCode::Xor),
            ("not".to_string(), OpCode::Not),
            ("shl".to_string(), OpCode::Shl),
            ("shr".to_string(), OpCode::Shr),
            ("cmp".to_string(), OpCode::Cmp),
            ("jmp".to_string(), OpCode::Jmp),
            ("jz".to_string(), OpCode::Jz),
            ("jnz".to_string(), OpCode::Jnz),
            ("jg".to_string(), OpCode::Jg),
            ("jge".to_string(), OpCode::Jge),
            ("jl".to_string(), OpCode::Jl),
            ("jle".to_string(), OpCode::Jle),
            ("call".to_string(), OpCode::Call),
            ("ret".to_string(), OpCode::Ret),
            ("inc".to_string(), OpCode::Inc),
            ("dec".to_string(), OpCode::Dec),
            ("read".to_string(), OpCode::ReadStdin),
            ("write".to_string(), OpCode::WriteStdout),
            ("mov".to_string(), OpCode::Move),
            ("neg".to_string(), OpCode::Neg),
            ("setz".to_string(), OpCode::SetZ),
            ("setnz".to_string(), OpCode::SetNz),
            ("setg".to_string(), OpCode::SetG),
            ("setge".to_string(), OpCode::SetGe),
            ("setl".to_string(), OpCode::SetL),
            ("setle".to_string(), OpCode::SetLe),
            ("exit".to_string(), OpCode::Exit),
        ])
    };

    static ref REGISTER_MAP: HashMap<String, RegisterCode> = {
        HashMap::from([
            ("r0".to_string(), RegisterCode::R0),
            ("r1".to_string(), RegisterCode::R1),
            ("r2".to_string(), RegisterCode::R2),
            ("r3".to_string(), RegisterCode::R3),
            ("r4".to_string(), RegisterCode::R4),
            ("r5".to_string(), RegisterCode::R5),
            ("r6".to_string(), RegisterCode::R6),
            ("r7".to_string(), RegisterCode::R7),
            ("r8".to_string(), RegisterCode::R8),
            ("r9".to_string(), RegisterCode::R9),
            ("r10".to_string(), RegisterCode::R10),
            ("r11".to_string(), RegisterCode::R11),
            ("r12".to_string(), RegisterCode::R12),
            ("bp".to_string(), RegisterCode::BP),
            ("sp".to_string(), RegisterCode::SP),
            ("pc".to_string(), RegisterCode::PC),
        ])
    };

    static ref SIZE_MAP: HashMap<String, u8> = {
        HashMap::from([
            ("#8".to_string(), 0b00),
            ("#16".to_string(), 0b01),
            ("#32".to_string(), 0b10),
            ("#64".to_string(), 0b11),
        ])
    };
}
