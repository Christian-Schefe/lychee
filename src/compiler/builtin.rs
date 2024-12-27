use crate::compiler::analyzer::analyzed_type::{
    AnalyzedTypeId, GenericId, GenericIdKind, GenericParams,
};
use crate::compiler::codegen::CodegenContext;
use crate::compiler::merger::merged_expression::{
    FunctionId, ResolvedFunctionHeader, ResolvedStruct, StructId,
};
use crate::compiler::parser::item_id::ItemId;
use crate::compiler::parser::ModuleIdentifier;
use crate::compiler::unwrapper::unwrapped_type::{UnwrappedFunctionRef, UnwrappedTypeId};
use std::collections::{HashMap, HashSet};

pub struct BuiltinFunction {
    pub name: String,
    pub return_type: AnalyzedTypeId,
    pub parameters: Vec<(String, AnalyzedTypeId)>,
    pub code: Box<dyn Fn(&mut CodegenContext)>,
}

impl BuiltinFunction {
    fn new(
        name: String,
        return_type: AnalyzedTypeId,
        parameters: Vec<(String, AnalyzedTypeId)>,
        code: Box<dyn Fn(&mut CodegenContext)>,
    ) -> BuiltinFunction {
        BuiltinFunction {
            name,
            return_type,
            parameters,
            code,
        }
    }

    fn function_ref(&self) -> UnwrappedFunctionRef {
        UnwrappedFunctionRef {
            id: FunctionId {
                id: ItemId {
                    module_id: ModuleIdentifier {
                        path: vec![],
                        root_name: "".to_string(),
                    },
                    item_name: self.name.clone(),
                },
                param_count: self.parameters.len(),
                generic_count: 0,
                body_index: -1,
            },
            arg_types: self
                .parameters
                .iter()
                .map(|(_, ty)| UnwrappedTypeId::upgrade_no_generic(ty))
                .collect(),
            generic_args: Vec::new(),
        }
    }

    fn read_char() -> BuiltinFunction {
        BuiltinFunction::new(
            "read_char".to_string(),
            AnalyzedTypeId::Char,
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

    fn write_char() -> BuiltinFunction {
        BuiltinFunction::new(
            "write_char".to_string(),
            AnalyzedTypeId::Unit,
            vec![("ch".to_string(), AnalyzedTypeId::Char)],
            Box::new(|context| {
                context.movi("r0", 1);
                context.write("r0", "[sp;8]");
                context.ret();
            }),
        )
    }

    fn write() -> BuiltinFunction {
        BuiltinFunction::new(
            "write".to_string(),
            AnalyzedTypeId::Unit,
            vec![
                (
                    "string".to_string(),
                    AnalyzedTypeId::Pointer(Box::new(AnalyzedTypeId::Char)),
                ),
                ("length".to_string(), AnalyzedTypeId::Integer(4)),
            ],
            Box::new(|context| {
                context.load(4, "r0", "[sp;8]");
                context.load(8, "r1", "[sp;12]");
                context.write("r0", "[r1]");
                context.ret();
            }),
        )
    }

    fn read() -> BuiltinFunction {
        BuiltinFunction::new(
            "read".to_string(),
            AnalyzedTypeId::Unit,
            vec![
                (
                    "string".to_string(),
                    AnalyzedTypeId::Pointer(Box::new(AnalyzedTypeId::Char)),
                ),
                ("length".to_string(), AnalyzedTypeId::Integer(4)),
            ],
            Box::new(|context| {
                context.load(4, "r0", "[sp;8]");
                context.load(8, "r1", "[sp;12]");
                context.read("r0", "[r1]");
                context.ret();
            }),
        )
    }

    fn malloc() -> BuiltinFunction {
        BuiltinFunction::new(
            "malloc".to_string(),
            AnalyzedTypeId::Pointer(Box::new(AnalyzedTypeId::Unit)),
            vec![("size".to_string(), AnalyzedTypeId::Integer(4))],
            Box::new(|context| {
                context.load(4, "r1", "[sp;8]");
                context.alloc("r1", "r0");
                context.ret();
            }),
        )
    }

    fn free() -> BuiltinFunction {
        BuiltinFunction::new(
            "free".to_string(),
            AnalyzedTypeId::Unit,
            vec![(
                "pointer".to_string(),
                AnalyzedTypeId::Pointer(Box::new(AnalyzedTypeId::Unit)),
            )],
            Box::new(|context| {
                context.load(8, "r0", "[sp;8]");
                context.free("r0");
                context.ret();
            }),
        )
    }
    fn random() -> BuiltinFunction {
        BuiltinFunction::new(
            "random".to_string(),
            AnalyzedTypeId::Integer(8),
            vec![],
            Box::new(|context| {
                context.rand("r0");
                context.ret();
            }),
        )
    }

    fn exit() -> BuiltinFunction {
        BuiltinFunction::new(
            "exit".to_string(),
            AnalyzedTypeId::Unit,
            vec![("code".to_string(), AnalyzedTypeId::Integer(4))],
            Box::new(|context| {
                context.load(4, "r0", "[sp;8]");
                context.exit();
            }),
        )
    }

    fn memcopy() -> BuiltinFunction {
        BuiltinFunction::new(
            "memcopy".to_string(),
            AnalyzedTypeId::Unit,
            vec![
                (
                    "src".to_string(),
                    AnalyzedTypeId::Pointer(Box::new(AnalyzedTypeId::Unit)),
                ),
                (
                    "dest".to_string(),
                    AnalyzedTypeId::Pointer(Box::new(AnalyzedTypeId::Unit)),
                ),
                ("length".to_string(), AnalyzedTypeId::Integer(4)),
            ],
            Box::new(|context| {
                context.load(4, "r0", "[sp;8]");
                context.load(8, "r1", "[sp;12]");
                context.load(8, "r2", "[sp;20]");
                context.memcopy("r0", "[r1]", "[r2]");
                context.ret();
            }),
        )
    }

    fn fopen() -> BuiltinFunction {
        BuiltinFunction::new(
            "fopen".to_string(),
            AnalyzedTypeId::Integer(4),
            vec![(
                "filename".to_string(),
                AnalyzedTypeId::Pointer(Box::new(AnalyzedTypeId::Char)),
            )],
            Box::new(|context| {
                context.load(8, "r0", "[sp;8]");
                context.file_open("r0", "[r0]");
                context.ret();
            }),
        )
    }

    fn fclose() -> BuiltinFunction {
        BuiltinFunction::new(
            "fclose".to_string(),
            AnalyzedTypeId::Unit,
            vec![("file".to_string(), AnalyzedTypeId::Integer(4))],
            Box::new(|context| {
                context.load(4, "r0", "[sp;8]");
                context.file_close("r0");
                context.ret();
            }),
        )
    }

    fn fread() -> BuiltinFunction {
        BuiltinFunction::new(
            "fread".to_string(),
            AnalyzedTypeId::Unit,
            vec![
                (
                    "buffer".to_string(),
                    AnalyzedTypeId::Pointer(Box::new(AnalyzedTypeId::Char)),
                ),
                ("length".to_string(), AnalyzedTypeId::Integer(4)),
                ("file".to_string(), AnalyzedTypeId::Integer(4)),
            ],
            Box::new(|context| {
                context.load(4, "r0", "[sp;8]");
                context.load(4, "r1", "[sp;12]");
                context.load(8, "r2", "[sp;16]");
                context.file_read("r0", "r1", "[r2]");
                context.ret();
            }),
        )
    }

    fn fwrite() -> BuiltinFunction {
        BuiltinFunction::new(
            "fwrite".to_string(),
            AnalyzedTypeId::Unit,
            vec![
                (
                    "buffer".to_string(),
                    AnalyzedTypeId::Pointer(Box::new(AnalyzedTypeId::Char)),
                ),
                ("length".to_string(), AnalyzedTypeId::Integer(4)),
                ("file".to_string(), AnalyzedTypeId::Integer(4)),
            ],
            Box::new(|context| {
                context.load(4, "r0", "[sp;8]");
                context.load(4, "r1", "[sp;12]");
                context.load(8, "r2", "[sp;16]");
                context.file_write("r0", "r1", "[r2]");
                context.ret();
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
            BuiltinFunction::memcopy(),
            BuiltinFunction::fopen(),
            BuiltinFunction::fclose(),
            BuiltinFunction::fread(),
            BuiltinFunction::fwrite(),
        ]
    }

    pub fn get_builtin_function_ids() -> HashMap<String, FunctionId> {
        let mut function_ids = HashMap::new();
        for function in BuiltinFunction::all_functions() {
            let function_ref = function.function_ref();
            function_ids.insert(function.name.clone(), function_ref.id);
        }
        function_ids
    }

    pub fn get_builtin_function_refs() -> Vec<UnwrappedFunctionRef> {
        let mut function_refs = Vec::new();
        for function in BuiltinFunction::all_functions() {
            let id = function.function_ref();
            function_refs.push(id);
        }
        function_refs
    }

    pub fn add_builtin_function_headers(
        function_headers: &mut HashMap<FunctionId, ResolvedFunctionHeader>,
    ) {
        for function in BuiltinFunction::all_functions() {
            let mut parameter_order = Vec::new();
            let mut parameter_types = HashMap::new();
            for (name, ty) in &function.parameters {
                parameter_order.push(name.clone());
                parameter_types.insert(name.clone(), ty.clone());
            }

            let id = function.function_ref().id;
            function_headers.insert(
                id.clone(),
                ResolvedFunctionHeader {
                    id: id.clone(),
                    return_type: function.return_type,
                    parameter_types,
                    parameter_order,
                    generic_params: GenericParams::empty(GenericIdKind::Function(id)),
                },
            );
        }
    }

    pub fn generate_builtin_function_code(context: &mut CodegenContext) {
        for function in BuiltinFunction::all_functions() {
            let id = function.function_ref();
            let identifier = id.get_key();
            let label = context.new_label(&identifier);
            context.function_labels.insert(identifier, label.clone());
            context.label(&label);
            (function.code)(context);
        }
    }
}

pub struct BuiltinStruct {
    pub id: StructId,
    pub fields: Vec<(String, AnalyzedTypeId)>,
    pub generic_params: GenericParams,
}

impl BuiltinStruct {
    fn new(
        id: StructId,
        fields: Vec<(String, AnalyzedTypeId)>,
        generic_params: Vec<String>,
    ) -> BuiltinStruct {
        if id.generic_count != generic_params.len() {
            panic!(
                "Generic count mismatch: {} != {}",
                id.generic_count,
                generic_params.len()
            );
        }
        BuiltinStruct {
            id: id.clone(),
            fields,
            generic_params: GenericParams::from_order(GenericIdKind::Struct(id), generic_params),
        }
    }

    fn tuple(size: usize) -> BuiltinStruct {
        let id = StructId {
            id: ItemId {
                module_id: ModuleIdentifier {
                    path: vec![],
                    root_name: "".to_string(),
                },
                item_name: "$tuple".to_string(),
            },
            generic_count: size,
        };

        let mut fields = Vec::new();
        for i in 0..size {
            fields.push((
                format!("item{}", i + 1),
                AnalyzedTypeId::GenericType(GenericId {
                    kind: GenericIdKind::Struct(id.clone()),
                    index: i,
                }),
            ));
        }
        BuiltinStruct::new(id, fields, (0..size).map(|i| format!("T{}", i)).collect())
    }

    fn all_structs() -> Vec<BuiltinStruct> {
        let mut structs = Vec::new();
        for i in 1..16 {
            structs.push(BuiltinStruct::tuple(i));
        }
        structs
    }

    pub fn get_builtin_struct_ids() -> HashMap<String, HashSet<StructId>> {
        let mut struct_ids = HashMap::new();
        for struct_def in BuiltinStruct::all_structs() {
            let struct_id = struct_def.id.clone();
            let entry = struct_ids
                .entry(struct_id.id.item_name.clone())
                .or_insert(HashSet::new());
            if !entry.insert(struct_id.clone()) {
                panic!("Duplicate struct id: {}", struct_id.id);
            }
        }
        struct_ids
    }

    pub fn add_builtin_resolved_structs(structs: &mut HashMap<StructId, ResolvedStruct>) {
        for struct_def in BuiltinStruct::all_structs() {
            let struct_id = struct_def.id.clone();
            let resolved_struct = ResolvedStruct {
                id: struct_id.clone(),
                generic_params: struct_def.generic_params.clone(),
                field_order: struct_def
                    .fields
                    .iter()
                    .map(|(name, _)| name.clone())
                    .collect(),
                field_types: struct_def.fields.iter().cloned().collect(),
            };
            structs.insert(struct_id, resolved_struct);
        }
    }
}
