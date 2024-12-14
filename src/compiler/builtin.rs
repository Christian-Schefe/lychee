use crate::compiler::codegen::CodegenContext;
use crate::compiler::merger::merged_expression::{FunctionId, ResolvedFunctionHeader, TypeId};
use crate::compiler::parser::item_id::ItemId;
use crate::compiler::parser::ModuleIdentifier;
use std::collections::HashMap;

pub struct BuiltinFunction {
    pub name: String,
    pub return_type: TypeId,
    pub parameters: Vec<(String, TypeId)>,
    pub code: Box<dyn Fn(&mut CodegenContext)>,
}

impl BuiltinFunction {
    fn new(
        name: String,
        return_type: TypeId,
        parameters: Vec<(String, TypeId)>,
        code: Box<dyn Fn(&mut CodegenContext)>,
    ) -> BuiltinFunction {
        BuiltinFunction {
            name,
            return_type,
            parameters,
            code,
        }
    }

    fn function_id(&self) -> ItemId {
        ItemId {
            module_id: ModuleIdentifier { path: vec![] },
            item_name: self.name.clone(),
        }
    }

    pub fn read_char() -> BuiltinFunction {
        BuiltinFunction::new(
            "read_char".to_string(),
            TypeId::Char,
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
            TypeId::Unit,
            vec![("ch".to_string(), TypeId::Char)],
            Box::new(|context| {
                context.movi("r0", 1);
                context.write("r0", "[sp;8]");
                context.ret();
            }),
        )
    }

    pub fn write() -> BuiltinFunction {
        BuiltinFunction::new(
            "write".to_string(),
            TypeId::Unit,
            vec![
                (
                    "string".to_string(),
                    TypeId::Pointer(Box::new(TypeId::Char)),
                ),
                ("length".to_string(), TypeId::Integer(4)),
            ],
            Box::new(|context| {
                context.load(4, "r0", "[sp;8]");
                context.load(8, "r1", "[sp;12]");
                context.write("r0", "[r1]");
                context.ret();
            }),
        )
    }

    pub fn read() -> BuiltinFunction {
        BuiltinFunction::new(
            "read".to_string(),
            TypeId::Unit,
            vec![
                (
                    "string".to_string(),
                    TypeId::Pointer(Box::new(TypeId::Char)),
                ),
                ("length".to_string(), TypeId::Integer(4)),
            ],
            Box::new(|context| {
                context.load(4, "r0", "[sp;8]");
                context.load(8, "r1", "[sp;12]");
                context.read("r0", "[r1]");
                context.ret();
            }),
        )
    }

    pub fn malloc() -> BuiltinFunction {
        BuiltinFunction::new(
            "malloc".to_string(),
            TypeId::Pointer(Box::new(TypeId::Unit)),
            vec![("size".to_string(), TypeId::Integer(4))],
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
            TypeId::Unit,
            vec![(
                "pointer".to_string(),
                TypeId::Pointer(Box::new(TypeId::Unit)),
            )],
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
            TypeId::Integer(8),
            vec![],
            Box::new(|context| {
                context.rand("r0");
                context.ret();
            }),
        )
    }

    pub fn exit() -> BuiltinFunction {
        BuiltinFunction::new(
            "exit".to_string(),
            TypeId::Unit,
            vec![("code".to_string(), TypeId::Integer(4))],
            Box::new(|context| {
                context.load(4, "r0", "[sp;8]");
                context.exit();
            }),
        )
    }

    fn all_functions() -> Vec<BuiltinFunction> {
        vec![
            BuiltinFunction::read_char(),
            BuiltinFunction::write_char(),
            BuiltinFunction::write(),
            BuiltinFunction::read(),
            BuiltinFunction::malloc(),
            BuiltinFunction::free(),
            BuiltinFunction::random(),
            BuiltinFunction::exit(),
        ]
    }

    pub fn add_builtin_function_ids(function_ids: &mut HashMap<String, FunctionId>) {
        for function in BuiltinFunction::all_functions() {
            let id = function.function_id();
            function_ids.insert(
                function.name.clone(),
                FunctionId {
                    item_id: id,
                    impl_type: None,
                },
            );
        }
    }

    pub fn add_builtin_function_headers(
        function_headers: &mut HashMap<ItemId, ResolvedFunctionHeader>,
    ) {
        for function in BuiltinFunction::all_functions() {
            let mut parameter_order = Vec::new();
            let mut parameter_types = HashMap::new();
            for (name, ty) in &function.parameters {
                parameter_order.push(name.clone());
                parameter_types.insert(name.clone(), ty.clone());
            }

            let id = function.function_id();
            function_headers.insert(
                id,
                ResolvedFunctionHeader {
                    return_type: function.return_type,
                    parameter_types,
                    parameter_order,
                },
            );
        }
    }

    pub fn generate_builtin_function_code(context: &mut CodegenContext) {
        for function in BuiltinFunction::all_functions() {
            let id = function.function_id();
            let identifier = id.to_string();
            let label = context.new_label(&identifier);
            context.function_labels.insert(identifier, label.clone());
            context.label(&label);
            (function.code)(context);
        }
    }
}
