use crate::compiler::codegen::Context;

pub fn add_builtin_fn_code(context: &mut Context) {
    write_fn(context);
    read_fn(context);
}

fn read_fn(context: &mut Context) {
    let label = context.get_new_label();
    context.function_labels.insert("readchar".to_string(), label.clone());
    context.function_arg_sizes.insert("readchar".to_string(), vec![]);
    context.push_label(label);
    context.push("set r0 1");
    context.push("sub sp r0");
    context.push("read r0 [sp]");
    context.push("pop #8 r0");
    context.push("ret")
}

fn write_fn(context: &mut Context) {
    let label = context.get_new_label();
    context.function_labels.insert("writechar".to_string(), label.clone());
    context.function_arg_sizes.insert("writechar".to_string(), vec![1]);
    context.push_label(label);
    context.push("set r0 1");
    context.push("write r0 [sp;8]");
    context.push("ret")
}