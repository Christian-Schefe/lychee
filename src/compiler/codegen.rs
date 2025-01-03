mod expression_codegen;
mod program_codegen;

use crate::compiler::codegen::program_codegen::generate_program_code;
use crate::compiler::resolver::resolved_expression::ResolvedProgram;
use std::collections::HashMap;
use std::path::PathBuf;

pub struct CodegenContext {
    lines: Vec<String>,
    label_counter: usize,
    pub function_labels: HashMap<String, String>,
    pub constant_labels: Vec<String>,
    pub return_label: String,
    pub break_label: String,
    pub continue_label: String,
    pub current_stack_size: usize,
    pub last_loop_stack_size: usize,
}

impl CodegenContext {
    pub fn new() -> Self {
        CodegenContext {
            lines: Vec::new(),
            label_counter: 0,
            function_labels: HashMap::new(),
            return_label: String::new(),
            break_label: String::new(),
            continue_label: String::new(),
            constant_labels: Vec::new(),
            current_stack_size: 0,
            last_loop_stack_size: 0,
        }
    }
    pub fn function_reset(&mut self) {
        self.return_label = String::new();
        self.break_label = String::new();
        self.continue_label = String::new();
        self.current_stack_size = 0;
        self.last_loop_stack_size = 0;
    }
    pub fn build(&self) -> String {
        self.lines.join("\n")
    }
    pub fn new_label(&mut self, postfix: &str) -> String {
        let label = format!("_L{}_{}", self.label_counter, postfix);
        self.label_counter += 1;
        label
    }
    pub fn label(&mut self, label: &str) {
        self.lines.push(format!("{}:", label));
    }
    pub fn data(&mut self, data: &[u8]) {
        let data = data
            .iter()
            .map(|x| x.to_string())
            .collect::<Vec<String>>()
            .join(" ");
        self.lines.push(format!("bytes {}", data));
    }
    pub fn jmp(&mut self, label: &str) {
        self.lines.push(format!("jmp {}", label));
    }
    pub fn push(&mut self, bytes: usize, register: &str) {
        self.lines.push(format!("push #{} {register}", bytes * 8));
    }
    pub fn pop(&mut self, bytes: usize, register: &str) {
        self.lines.push(format!("pop #{} {register}", bytes * 8));
    }
    pub fn popmem(&mut self, register: &str, address: &str) {
        self.lines.push(format!("popmem {register} {address}"));
    }
    pub fn peekmem(&mut self, register: &str, address: &str) {
        self.lines.push(format!("peekmem {register} {address}"));
    }
    pub fn pushmem(&mut self, register: &str, address: &str) {
        self.lines.push(format!("pushmem {register} {address}"));
    }
    pub fn mov(&mut self, dest_register: &str, source_register: &str) {
        self.lines
            .push(format!("mov {dest_register} {source_register}"));
    }
    pub fn movi(&mut self, register: &str, value: isize) {
        self.lines.push(format!("movi {register} {value}"));
    }
    pub fn subi(&mut self, register: &str, value: isize) {
        self.lines.push(format!("subi {register} {value}"));
    }
    pub fn addi(&mut self, register: &str, value: isize) {
        self.lines.push(format!("addi {register} {value}"));
    }
    pub fn add(&mut self, dest_register: &str, source_register: &str) {
        self.lines
            .push(format!("add {dest_register} {source_register}"));
    }
    pub fn sub(&mut self, dest_register: &str, source_register: &str) {
        self.lines
            .push(format!("sub {dest_register} {source_register}"));
    }
    pub fn mul(&mut self, dest_register: &str, source_register: &str) {
        self.lines
            .push(format!("mul {dest_register} {source_register}"));
    }
    pub fn div(&mut self, dest_register: &str, source_register: &str) {
        self.lines
            .push(format!("div {dest_register} {source_register}"));
    }
    pub fn modulo(&mut self, dest_register: &str, source_register: &str) {
        self.lines
            .push(format!("mod {dest_register} {source_register}"));
    }
    pub fn and(&mut self, dest_register: &str, source_register: &str) {
        self.lines
            .push(format!("and {dest_register} {source_register}"));
    }
    pub fn or(&mut self, dest_register: &str, source_register: &str) {
        self.lines
            .push(format!("or {dest_register} {source_register}"));
    }
    pub fn xor(&mut self, dest_register: &str, source_register: &str) {
        self.lines
            .push(format!("xor {dest_register} {source_register}"));
    }
    pub fn shl(&mut self, dest_register: &str, source_register: &str) {
        self.lines
            .push(format!("shl {dest_register} {source_register}"));
    }
    pub fn shr(&mut self, dest_register: &str, source_register: &str) {
        self.lines
            .push(format!("shr {dest_register} {source_register}"));
    }
    pub fn cmp(&mut self, dest_register: &str, source_register: &str) {
        self.lines
            .push(format!("cmp {dest_register} {source_register}"));
    }
    pub fn inc(&mut self, register: &str) {
        self.lines.push(format!("inc {register}"));
    }
    pub fn dec(&mut self, register: &str) {
        self.lines.push(format!("dec {register}"));
    }
    pub fn muli(&mut self, register: &str, value: isize) {
        self.lines.push(format!("muli {register} {value}"));
    }
    pub fn exit(&mut self) {
        self.lines.push("exit".to_string());
    }
    pub fn ret(&mut self) {
        self.lines.push("ret".to_string());
    }
    pub fn cmpi(&mut self, register: &str, value: isize) {
        self.lines.push(format!("cmpi {register} {value}"));
    }
    pub fn jz(&mut self, label: &str) {
        self.lines.push(format!("jz {}", label));
    }
    pub fn jnz(&mut self, label: &str) {
        self.lines.push(format!("jnz {}", label));
    }
    pub fn store(&mut self, bytes: usize, register: &str, address: &str) {
        self.lines
            .push(format!("store #{} {register} {address}", bytes * 8));
    }
    pub fn load(&mut self, bytes: usize, register: &str, address: &str) {
        self.lines
            .push(format!("load #{} {register} {address}", bytes * 8));
    }
    pub fn lea(&mut self, register: &str, address: &str) {
        self.lines.push(format!("lea {register} {address}"));
    }
    pub fn neg(&mut self, register: &str) {
        self.lines.push(format!("neg {register}"));
    }
    pub fn not(&mut self, register: &str) {
        self.lines.push(format!("not {register}"));
    }
    pub fn setz(&mut self, register: &str) {
        self.lines.push(format!("setz {register}"));
    }
    pub fn setnz(&mut self, register: &str) {
        self.lines.push(format!("setnz {register}"));
    }
    pub fn setl(&mut self, register: &str) {
        self.lines.push(format!("setl {register}"));
    }
    pub fn setle(&mut self, register: &str) {
        self.lines.push(format!("setle {register}"));
    }
    pub fn setg(&mut self, register: &str) {
        self.lines.push(format!("setg {register}"));
    }
    pub fn setge(&mut self, register: &str) {
        self.lines.push(format!("setge {register}"));
    }
    pub fn signext(&mut self, to_bytes: usize, register: &str) {
        self.lines
            .push(format!("signext #{} {register}", to_bytes * 8));
    }
    pub fn call_function(&mut self, function: &str) {
        let label = self.get_label(function);
        self.lines.push(format!("call {}", label));
    }
    pub fn get_label(&mut self, function: &str) -> String {
        let label = self.function_labels.get(function).unwrap_or_else(|| {
            panic!(
                "Function label not found for function {}, {:?}",
                function, self.function_labels
            );
        });
        label.clone()
    }
    pub fn call_address(&mut self, address: &str) {
        self.lines.push(format!("call {}", address));
    }
    pub fn read(&mut self, register: &str, address: &str) {
        self.lines.push(format!("read {register} {address}"));
    }
    pub fn write(&mut self, register: &str, address: &str) {
        self.lines.push(format!("write {register} {address}"));
    }
    pub fn alloc(&mut self, size_register: &str, address_register: &str) {
        self.lines
            .push(format!("alloc {size_register} {address_register}"));
    }
    pub fn free(&mut self, address_register: &str) {
        self.lines.push(format!("free {address_register}"));
    }
    pub fn rand(&mut self, register: &str) {
        self.lines.push(format!("rand {register}"));
    }

    pub fn memcopy(&mut self, size_register: &str, dest_address: &str, source_address: &str) {
        self.lines.push(format!(
            "memcopy {size_register} {dest_address} {source_address}"
        ));
    }

    pub fn memset(&mut self, size_register: &str, value_register: &str, address: &str) {
        self.lines.push(format!(
            "memset {size_register} {value_register} {address}"
        ));
    }
    
    pub fn file_open(&mut self, pointer_register: &str, filename_address: &str) {
        self.lines
            .push(format!("fileopen {pointer_register} {filename_address}"));
    }
    pub fn file_close(&mut self, pointer_register: &str) {
        self.lines.push(format!("fileclose {pointer_register}"));
    }

    pub fn file_read(&mut self, pointer_register: &str, size_register: &str, buffer_address: &str) {
        self.lines.push(format!(
            "fileread {pointer_register} {size_register} {buffer_address}"
        ));
    }

    pub fn file_write(
        &mut self,
        pointer_register: &str,
        size_register: &str,
        buffer_address: &str,
    ) {
        self.lines.push(format!(
            "filewrite {pointer_register} {size_register} {buffer_address}"
        ));
    }
}

pub fn gen_code(program: ResolvedProgram, output: &PathBuf) {
    let mut context = CodegenContext::new();
    generate_program_code(&mut context, &program);
    let code = context.build();
    std::fs::write(output, code).unwrap();
}
