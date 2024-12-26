use crate::compiler::analyzer::analyzed_expression::AnalyzedProgram;
use crate::compiler::analyzer::analyzed_type::{GenericIdKind, GenericParams};
use crate::compiler::unwrapper::program_unwrapper::{GenericInfo, UnwrapperContext};
use crate::compiler::unwrapper::unwrapped_type::UnwrappedProgram;
use std::collections::{HashMap, HashSet};

pub mod program_unwrapper;
pub mod unwrapped_type;

pub fn unwrap_program(program: &AnalyzedProgram) -> UnwrappedProgram {
    let builtin_functions = crate::compiler::builtin::BuiltinFunction::get_builtin_function_refs();
    let mut context = UnwrapperContext {
        functions: HashMap::new(),
        structs: HashMap::new(),
        builtin_functions: builtin_functions
            .into_iter()
            .map(|x| x.get_key())
            .collect::<HashSet<String>>(),
    };

    let mut generic_infos = GenericInfo {
        generic_args: Vec::new(),
        generic_params: GenericParams::empty(GenericIdKind::Function(
            program.main_function.id.clone(),
        )),
    };

    let unwrapped_main_ref = program_unwrapper::unwrap_function_ref(
        &mut context,
        program,
        &mut generic_infos,
        &program.main_function,
    );

    program_unwrapper::unwrap_function(
        &mut context,
        program,
        &unwrapped_main_ref,
    );

    UnwrappedProgram {
        structs: context.structs,
        functions: context.functions,
        main_function_name: unwrapped_main_ref.get_key(),
    }
}
