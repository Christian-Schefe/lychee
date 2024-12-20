use clap::Parser;
use std::path::PathBuf;
use vm::execute;

mod input; 

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    input: PathBuf,
    #[arg(short, long, default_value("false"))]
    debug_print: bool,
}

fn main() {
    let args = Args::parse();
    let program = input::read_obj_file(&args.input, args.debug_print);
    execute(program, args.debug_print);
}
