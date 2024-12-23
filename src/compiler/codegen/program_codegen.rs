use crate::compiler::builtin::BuiltinFunction;
use crate::compiler::codegen::expression_codegen::generate_expression_code;
use crate::compiler::codegen::CodegenContext;
use crate::compiler::resolver::resolved_expression::{
    FunctionReturnLocation, ResolvedFunction, ResolvedProgram,
};

pub fn generate_program_code(context: &mut CodegenContext, program: &ResolvedProgram) {
    program.functions.iter().for_each(|x| {
        let label = context.new_label(&x.name);
        context.function_labels.insert(x.name.clone(), label);
    });

    program.constants.iter().enumerate().for_each(|x| {
        let label = context.new_label(format!("constant_{}", x.0).as_str());
        context.constant_labels.push(label);
    });

    generate_program_prelude(context, &program.main_function_name);

    BuiltinFunction::generate_builtin_function_code(context);

    for function in &program.functions {
        context.function_reset();
        context.return_label = context.new_label(format!("{}_return", function.name).as_str());
        generate_function_code(context, function);
    }

    for (index, constant) in program.constants.iter().enumerate() {
        let label = context.constant_labels[index].clone();
        context.label(&label);
        context.data(constant);
    }
}

fn generate_program_prelude(context: &mut CodegenContext, main_function_name: &String) {
    context.call(main_function_name);
    context.exit();
}

fn generate_function_code(context: &mut CodegenContext, function: &ResolvedFunction) {
    let label = context.function_labels[&function.name].clone();
    context.label(&label);
    generate_function_prologue(context, function);

    generate_expression_code(context, &function.body);

    let return_label = context.return_label.clone();
    context.label(&return_label);

    if let FunctionReturnLocation::Stack { offset, size } = function.value_location {
        context.movi("r0", size as isize);
        context.popmem("r0", &format!("[bp;{}]", offset));
    }

    generate_function_epilogue(context);
}

fn generate_function_prologue(context: &mut CodegenContext, function: &ResolvedFunction) {
    context.push(8, "bp");
    context.mov("bp", "sp");
    context.subi("sp", function.local_var_stack_size as isize);
}

fn generate_function_epilogue(context: &mut CodegenContext) {
    context.mov("sp", "bp");
    context.pop(8, "bp");
    context.ret();
}
