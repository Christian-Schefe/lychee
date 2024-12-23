use crate::compiler::analyzer::analyzed_expression::AnalyzedProgram;
use crate::compiler::unwrapper::program_unwrapper::UnwrapperContext;
use crate::compiler::unwrapper::unwrapped_type::UnwrappedProgram;
use std::collections::HashMap;

mod program_unwrapper;
pub mod unwrapped_type;

pub fn unwrap_program(program: &AnalyzedProgram) -> UnwrappedProgram {
    let mut context = UnwrapperContext {
        functions: HashMap::new(),
        structs: HashMap::new(),
    };

    program_unwrapper::unwrap_function(&mut context, program, &program.main_function);

    UnwrappedProgram {
        structs: context.structs,
        functions: context.functions,
        main_function_name: program.main_function.to_string(),
    }
}
