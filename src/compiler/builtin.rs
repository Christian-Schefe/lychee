use std::collections::HashMap;
use crate::compiler::analyzer::program_analyzer::FunctionHeader;
use crate::compiler::analyzer::type_resolver::AnalyzedType;
use crate::compiler::codegen2::CodegenContext;

pub fn add_builtin_function_headers(function_headers: &mut HashMap<String, FunctionHeader>) {
    function_headers.insert("read_char".to_string(), FunctionHeader {
        return_type: AnalyzedType::Char,
        parameters: vec![],
    });
    function_headers.insert("write_char".to_string(), FunctionHeader {
        return_type: AnalyzedType::Unit,
        parameters: vec![AnalyzedType::Char],
    });
}

pub fn generate_builtin_function_code(context: &mut CodegenContext) {
    let read_char_label = context.new_label("read_char");
    context.function_labels.insert("read_char".to_string(), read_char_label.clone());

    context.label(&read_char_label);
    context.movi("r0", 1);
    context.subi("sp", 1);
    context.read("r0", "[sp]");
    context.pop(1, "r0");
    context.ret();

    let write_char_label = context.new_label("write_char");
    context.function_labels.insert("write_char".to_string(), write_char_label.clone());

    context.label(&write_char_label);
    context.movi("r0", 1);
    context.write("r0", "[sp;8]");
    context.ret();
}