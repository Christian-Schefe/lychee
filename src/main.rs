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
}
