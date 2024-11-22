use crate::assembler::instruction_type::InstructionType;
use lazy_static::lazy_static;
use lychee_compiler::{BinopType, FlagConditionType, OpCode, RegisterCode, UnopType};
use std::collections::HashMap;
use std::iter::Iterator;

#[derive(Debug)]
pub enum Instruction {
    Label(String),
    Instr(InstructionType),
}

pub(crate) fn convert_line(line: &str) -> Instruction {
    let parts = line.split_whitespace().collect::<Vec<&str>>();

    if parts[0].ends_with(":") {
        let label_str = parts[0].split(":").collect::<Vec<&str>>()[0];
        return Instruction::Label(label_str.to_string());
    }

    let opcode = match OPCODE_MAP.get(parts[0]).cloned() {
        Some(opcode) => opcode,
        None => panic!("Invalid opcode: {}", parts[0]),
    };

    let instruction = match &opcode {
        OpCode::Nop | OpCode::Ret | OpCode::Exit => InstructionType::parse_simple(opcode),
        OpCode::Store | OpCode::Load => InstructionType::parse_register_size_address(opcode, parts),
        OpCode::Push | OpCode::Pop | OpCode::SignExtend => InstructionType::parse_size_register(opcode, parts),
        OpCode::Binop(_) => InstructionType::parse_two_registers(opcode, parts),
        OpCode::BinopImmediate(_) => InstructionType::parse_register_immediate(opcode, parts),
        OpCode::Call | OpCode::Jump(_) => InstructionType::parse_label(opcode, parts),
        OpCode::Unop(_) | OpCode::Set(_) => InstructionType::parse_register(opcode, parts),
        OpCode::ReadStdin | OpCode::WriteStdout | OpCode::Lea => {
            InstructionType::parse_register_address(opcode, parts)
        }
    };

    Instruction::Instr(instruction)
}

pub(crate) fn instructions_to_bytes(instructions: Vec<Instruction>) -> Vec<u8> {
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
    pub static ref OPCODE_MAP: HashMap<String, OpCode> = {
        HashMap::from([
            ("nop".to_string(), OpCode::Nop),
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
