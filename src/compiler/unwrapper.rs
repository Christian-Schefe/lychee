use crate::compiler::analyzer::analyzed_expression::AnalyzedProgram;
use crate::compiler::unwrapper::program_unwrapper::UnwrapperContext;
use crate::compiler::unwrapper::unwrapped_type::UnwrappedProgram;
use std::collections::{HashMap, HashSet};

mod program_unwrapper;
pub mod unwrapped_type;

pub fn unwrap_program(program: &AnalyzedProgram) -> UnwrappedProgram {
    let builtin_functions = crate::compiler::builtin::BuiltinFunction::get_builtin_function_refs();
    let mut context = UnwrapperContext {
        functions: HashMap::new(),
        structs: HashMap::new(),
        builtin_functions: builtin_functions
            .into_iter()
            .map(|x| x.to_string())
            .collect::<HashSet<String>>(),
    };

    program_unwrapper::unwrap_function(&mut context, program, &program.main_function);

    UnwrappedProgram {
        structs: context.structs,
        functions: context.functions,
        main_function_name: program.main_function.to_string(),
    }
}
