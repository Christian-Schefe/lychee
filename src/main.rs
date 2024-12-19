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
    #[arg(short, long)]
    assembly_output: Option<PathBuf>,
    #[arg(long, default_value("false"))]
    assemble_only: bool,
    #[arg(long)]
    bundle_vm: Option<PathBuf>,
}

fn main() {
    let args = Args::parse();
    let output = args
        .output
        .clone()
        .unwrap_or_else(|| args.input.with_extension("o"));

    if args.assemble_only {
        assemble(&args.input, &output);
    } else {
        let assembly_output = args
            .assembly_output
            .clone()
            .unwrap_or_else(|| output.with_extension("bud"));
        compile(&args.input, &assembly_output);
        assemble(&assembly_output, &output);
    }

    if let Some(vm_path) = args.bundle_vm {
        bundle_vm(&output, &vm_path);
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
