use crate::compiler::analyzer::program_analyzer::FunctionHeader;
use crate::compiler::analyzer::type_resolver::AnalyzedType;
use crate::compiler::codegen::CodegenContext;
use std::collections::HashMap;

pub struct BuiltinFunction {
    pub name: String,
    pub return_type: AnalyzedType,
    pub parameters: Vec<AnalyzedType>,
    pub code: Box<dyn Fn(&mut CodegenContext)>,
}

impl BuiltinFunction {
    fn new(
        name: String,
        return_type: AnalyzedType,
        parameters: Vec<AnalyzedType>,
        code: Box<dyn Fn(&mut CodegenContext)>,
    ) -> BuiltinFunction {
        BuiltinFunction {
            name,
            return_type,
            parameters,
            code,
        }
    }

    pub fn read_char() -> BuiltinFunction {
        BuiltinFunction::new(
            "read_char".to_string(),
            AnalyzedType::Char,
            vec![],
            Box::new(|context| {
                context.movi("r0", 1);
                context.subi("sp", 1);
                context.read("r0", "[sp]");
                context.pop(1, "r0");
                context.ret();
            }),
        )
    }

    pub fn write_char() -> BuiltinFunction {
        BuiltinFunction::new(
            "write_char".to_string(),
            AnalyzedType::Unit,
            vec![AnalyzedType::Char],
            Box::new(|context| {
                context.movi("r0", 1);
                context.write("r0", "[sp;8]");
                context.ret();
            }),
        )
    }

    pub fn malloc() -> BuiltinFunction {
        BuiltinFunction::new(
            "malloc".to_string(),
            AnalyzedType::Pointer(Box::new(AnalyzedType::Unit)),
            vec![AnalyzedType::Integer(4)],
            Box::new(|context| {
                context.load(4, "r1", "[sp;8]");
                context.alloc("r1", "r0");
                context.ret();
            }),
        )
    }

    pub fn free() -> BuiltinFunction {
        BuiltinFunction::new(
            "free".to_string(),
            AnalyzedType::Unit,
            vec![AnalyzedType::Pointer(Box::new(AnalyzedType::Unit))],
            Box::new(|context| {
                context.load(8, "r0", "[sp;8]");
                context.free("r0");
                context.ret();
            }),
        )
    }

    pub fn random() -> BuiltinFunction {
        BuiltinFunction::new(
            "random".to_string(),
            AnalyzedType::Integer(8),
            vec![],
            Box::new(|context| {
                context.rand("r0");
                context.ret();
            }),
        )
    }

    fn all_functions() -> Vec<BuiltinFunction> {
        vec![
            BuiltinFunction::read_char(),
            BuiltinFunction::write_char(),
            BuiltinFunction::malloc(),
            BuiltinFunction::free(),
            BuiltinFunction::random(),
        ]
    }

    pub fn add_builtin_function_headers(function_headers: &mut HashMap<String, FunctionHeader>) {
        for function in BuiltinFunction::all_functions() {
            function_headers.insert(
                function.name.clone(),
                FunctionHeader {
                    return_type: function.return_type,
                    parameters: function.parameters,
                },
            );
        }
    }

    pub fn generate_builtin_function_code(context: &mut CodegenContext) {
        for function in BuiltinFunction::all_functions() {
            let label = context.new_label(&function.name);
            context
                .function_labels
                .insert(function.name.clone(), label.clone());
            context.label(&label);
            (function.code)(context);
        }
    }
}
