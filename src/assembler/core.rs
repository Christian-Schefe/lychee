use crate::assembler::instruction_type::{Instruction, InstructionKind};
use lazy_static::lazy_static;
use lychee_compiler::{BinopType, FlagConditionType, OpCode, RegisterCode, UnopType};
use std::collections::HashMap;
use std::iter::Iterator;

#[derive(Debug)]
pub enum AssemblyInstruction {
    Label(String),
    Instr(Instruction),
    Bytes(Vec<u8>),
}

pub(crate) fn convert_line(line: &str) -> AssemblyInstruction {
    let parts = line.split_whitespace().collect::<Vec<&str>>();

    if parts[0].ends_with(":") {
        let label_str = &parts[0][..parts[0].len() - 1];
        return AssemblyInstruction::Label(label_str.to_string());
    }

    if parts[0] == "bytes" {
        let mut bytes = Vec::new();
        for part in parts[1..].iter() {
            bytes.push(u8::from_str_radix(part, 10).unwrap_or_else(|_| {
                panic!("Invalid byte value: {}", part);
            }));
        }
        return AssemblyInstruction::Bytes(bytes);
    }

    let opcode = match OPCODE_MAP.get(parts[0]).cloned() {
        Some(opcode) => opcode,
        None => panic!("Invalid opcode: {}", parts[0]),
    };

    let instruction_kind = match &opcode {
        OpCode::Ret | OpCode::Exit => InstructionKind::parse_simple(),
        OpCode::Store | OpCode::Load => InstructionKind::parse_register_size_address(parts),
        OpCode::Push | OpCode::Pop | OpCode::SignExtend => {
            InstructionKind::parse_size_register(parts)
        }
        OpCode::Binop(_) | OpCode::Alloc => InstructionKind::parse_two_registers(parts),
        OpCode::BinopImmediate(_) => InstructionKind::parse_register_immediate(parts),
        OpCode::Call | OpCode::Jump(_) => InstructionKind::parse_address(parts),
        OpCode::Unop(_) | OpCode::Set(_) | OpCode::Free | OpCode::Rand | OpCode::FileClose => {
            InstructionKind::parse_register(parts)
        }
        OpCode::ReadStdin
        | OpCode::WriteStdout
        | OpCode::Lea
        | OpCode::PushMem
        | OpCode::PopMem
        | OpCode::PeekMem
        | OpCode::FileOpen => InstructionKind::parse_register_address(parts),
        OpCode::FileRead | OpCode::FileWrite => InstructionKind::parse_two_registers_address(parts),
        OpCode::MemCopy => InstructionKind::parse_register_two_addresses(parts),
    };

    let instruction = Instruction {
        opcode: opcode.byte_code(),
        kind: instruction_kind,
    };

    AssemblyInstruction::Instr(instruction)
}

pub(crate) fn instructions_to_bytes(instructions: Vec<AssemblyInstruction>) -> Vec<u8> {
    let mut bytes = Vec::new();
    let mut labels: HashMap<String, u64> = HashMap::new();
    let mut label_placeholders: HashMap<String, Vec<u64>> = HashMap::new();

    for instruction in instructions {
        match instruction {
            AssemblyInstruction::Label(label) => {
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
            AssemblyInstruction::Instr(instr) => {
                instr.add_bytes(&mut bytes, &labels, &mut label_placeholders);
            }
            AssemblyInstruction::Bytes(mut b) => {
                bytes.append(&mut b);
            }
        }
    }

    bytes
}

lazy_static! {
    pub static ref OPCODE_MAP: HashMap<String, OpCode> = {
        HashMap::from([
            ("rand".to_string(), OpCode::Rand),
            ("load".to_string(), OpCode::Load),
            ("store".to_string(), OpCode::Store),
            ("push".to_string(), OpCode::Push),
            ("pop".to_string(), OpCode::Pop),
            ("mov".to_string(), OpCode::Binop(BinopType::Mov)),
            ("add".to_string(), OpCode::Binop(BinopType::Add)),
            ("sub".to_string(), OpCode::Binop(BinopType::Sub)),
            ("mul".to_string(), OpCode::Binop(BinopType::Mul)),
            ("div".to_string(), OpCode::Binop(BinopType::Div)),
            ("mod".to_string(), OpCode::Binop(BinopType::Mod)),
            ("and".to_string(), OpCode::Binop(BinopType::And)),
            ("or".to_string(), OpCode::Binop(BinopType::Or)),
            ("xor".to_string(), OpCode::Binop(BinopType::Xor)),
            ("shl".to_string(), OpCode::Binop(BinopType::Shl)),
            ("shr".to_string(), OpCode::Binop(BinopType::Shr)),
            ("cmp".to_string(), OpCode::Binop(BinopType::Cmp)),
            ("movi".to_string(), OpCode::BinopImmediate(BinopType::Mov)),
            ("addi".to_string(), OpCode::BinopImmediate(BinopType::Add)),
            ("subi".to_string(), OpCode::BinopImmediate(BinopType::Sub)),
            ("muli".to_string(), OpCode::BinopImmediate(BinopType::Mul)),
            ("divi".to_string(), OpCode::BinopImmediate(BinopType::Div)),
            ("modi".to_string(), OpCode::BinopImmediate(BinopType::Mod)),
            ("andi".to_string(), OpCode::BinopImmediate(BinopType::And)),
            ("ori".to_string(), OpCode::BinopImmediate(BinopType::Or)),
            ("xori".to_string(), OpCode::BinopImmediate(BinopType::Xor)),
            ("shli".to_string(), OpCode::BinopImmediate(BinopType::Shl)),
            ("shri".to_string(), OpCode::BinopImmediate(BinopType::Shr)),
            ("cmpi".to_string(), OpCode::BinopImmediate(BinopType::Cmp)),
            ("jmp".to_string(), OpCode::Jump(FlagConditionType::Always)),
            ("jz".to_string(), OpCode::Jump(FlagConditionType::Zero)),
            ("jnz".to_string(), OpCode::Jump(FlagConditionType::NotZero)),
            ("jg".to_string(), OpCode::Jump(FlagConditionType::Greater)),
            (
                "jge".to_string(),
                OpCode::Jump(FlagConditionType::GreaterEquals),
            ),
            ("jl".to_string(), OpCode::Jump(FlagConditionType::Less)),
            (
                "jle".to_string(),
                OpCode::Jump(FlagConditionType::LessEquals),
            ),
            ("set".to_string(), OpCode::Set(FlagConditionType::Always)),
            ("setz".to_string(), OpCode::Set(FlagConditionType::Zero)),
            ("setnz".to_string(), OpCode::Set(FlagConditionType::NotZero)),
            ("setg".to_string(), OpCode::Set(FlagConditionType::Greater)),
            (
                "setge".to_string(),
                OpCode::Set(FlagConditionType::GreaterEquals),
            ),
            ("setl".to_string(), OpCode::Set(FlagConditionType::Less)),
            (
                "setle".to_string(),
                OpCode::Set(FlagConditionType::LessEquals),
            ),
            ("not".to_string(), OpCode::Unop(UnopType::Not)),
            ("neg".to_string(), OpCode::Unop(UnopType::Neg)),
            ("inc".to_string(), OpCode::Unop(UnopType::Inc)),
            ("dec".to_string(), OpCode::Unop(UnopType::Dec)),
            ("call".to_string(), OpCode::Call),
            ("ret".to_string(), OpCode::Ret),
            ("read".to_string(), OpCode::ReadStdin),
            ("write".to_string(), OpCode::WriteStdout),
            ("exit".to_string(), OpCode::Exit),
            ("signext".to_string(), OpCode::SignExtend),
            ("lea".to_string(), OpCode::Lea),
            ("pushmem".to_string(), OpCode::PushMem),
            ("popmem".to_string(), OpCode::PopMem),
            ("peekmem".to_string(), OpCode::PeekMem),
            ("alloc".to_string(), OpCode::Alloc),
            ("free".to_string(), OpCode::Free),
            ("fileopen".to_string(), OpCode::FileOpen),
            ("fileclose".to_string(), OpCode::FileClose),
            ("fileread".to_string(), OpCode::FileRead),
            ("filewrite".to_string(), OpCode::FileWrite),
            ("memcopy".to_string(), OpCode::MemCopy),
        ])
    };
    pub static ref REGISTER_MAP: HashMap<String, RegisterCode> = {
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
    pub static ref SIZE_MAP: HashMap<String, u8> = {
        HashMap::from([
            ("#8".to_string(), 0b00),
            ("#16".to_string(), 0b01),
            ("#32".to_string(), 0b10),
            ("#64".to_string(), 0b11),
        ])
    };
}
