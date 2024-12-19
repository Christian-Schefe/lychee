mod core;

pub fn execute(program: Vec<u8>, debug_print: bool) {
    core::execute(program, debug_print);
}
