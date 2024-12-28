use crate::assembler::assemble;
use crate::compiler::compile;
use clap::Parser;
use std::path::PathBuf;

mod assembler;
mod compiler;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    input: PathBuf,
    #[arg(short, long)]
    output: Option<PathBuf>,
    #[arg(long)]
    bundle_vm: Option<PathBuf>,
    #[arg(short, long, default_value("false"))]
    debug_output: bool,
}

fn main() {
    let args = Args::parse();
    let output_dir = args
        .output
        .clone()
        .unwrap_or_else(|| args.input.join("target"));

    if !output_dir.try_exists().unwrap() {
        std::fs::create_dir_all(&output_dir).unwrap();
    }

    let input_dir = args.input.canonicalize().unwrap();
    let output_dir = output_dir.canonicalize().unwrap();

    let config = compiler::config::read_config(&input_dir).unwrap();
    let assembly_output = compile(config, &output_dir, args.debug_output).unwrap();
    let executable_output = assemble(&assembly_output).unwrap();

    if let Some(vm_path) = args.bundle_vm {
        bundle_vm(&executable_output, &vm_path);
    }
}

fn bundle_vm(program_path: &PathBuf, vm_path: &PathBuf) {
    let program = std::fs::read(program_path).unwrap();
    let program_size = program.len() as u64;
    let program_size_bytes = program_size.to_le_bytes();
    let mut vm_binary = std::fs::read(vm_path).unwrap();
    vm_binary.extend(program);
    vm_binary.extend(&program_size_bytes);
    std::fs::write(program_path.with_extension("exe"), vm_binary).unwrap();
}
