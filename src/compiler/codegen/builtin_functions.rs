use std::collections::HashMap;
use crate::compiler::codegen::{Context, FunctionData, FunctionResultLocation, VarData};

pub fn add_builtin_fn_code(context: &mut Context) {
    write_fn(context);
    read_fn(context);
}

fn read_fn(context: &mut Context) {
    let label = context.get_new_label();

    context.function_data.insert("readchar".to_string(), FunctionData {
        label: label.clone(),
        args: HashMap::new(),
        arg_push_order: Vec::new(),
        return_location: FunctionResultLocation::Register(0),
    });
    context.push_label(label);
    context.push("movi r0 1");
    context.push("subi sp 1");
    context.push("read r0 [sp]");
    context.push("pop #8 r0");
    context.push("ret")
}

fn write_fn(context: &mut Context) {
    let label = context.get_new_label();
    context.function_data.insert("writechar".to_string(), FunctionData {
        label: label.clone(),
        args: HashMap::from([("char".to_string(), VarData { offset: 8, byte_size: 1 })]),
        arg_push_order: vec!["char".to_string()],
        return_location: FunctionResultLocation::Discard,
    });
    context.push_label(label);
    context.push("movi r0 1");
    context.push("write r0 [sp;8]");
    context.push("ret")
}
