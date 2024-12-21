use crate::compiler::analyzer::analyzed_type::AnalyzedTypeId;
use crate::compiler::builtin::BuiltinFunction;
use crate::compiler::merger::merged_expression::FunctionId;
use crate::compiler::merger::resolved_types::ResolvedTypes;
use crate::compiler::merger::MergerResult;
use crate::compiler::parser::item_id::{ItemId, ParsedFunctionId};
use crate::compiler::parser::parsed_expression::ParsedProgram;
use crate::compiler::parser::ModuleIdentifier;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct CollectedFunctionData {
    pub functions: HashMap<ModuleIdentifier, HashMap<String, FunctionId>>,
    pub member_functions:
        HashMap<ModuleIdentifier, HashMap<GenericAnalyzedTypeId, HashMap<String, FunctionId>>>,
    pub function_imports: HashMap<ModuleIdentifier, HashMap<String, FunctionId>>,
    pub member_function_imports:
        HashMap<ModuleIdentifier, HashMap<GenericAnalyzedTypeId, HashMap<String, FunctionId>>>,
    pub builtin_functions: HashMap<String, FunctionId>,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum GenericAnalyzedTypeId {
    Unit,
    Bool,
    Char,
    Integer(usize),
    Pointer(Box<GenericAnalyzedTypeId>),
    StructType(ItemId),
    GenericType(String),
}

impl GenericAnalyzedTypeId {
    pub fn from_analyzed_type_id(id: &AnalyzedTypeId) -> Self {
        match id {
            AnalyzedTypeId::Unit => GenericAnalyzedTypeId::Unit,
            AnalyzedTypeId::Bool => GenericAnalyzedTypeId::Bool,
            AnalyzedTypeId::Char => GenericAnalyzedTypeId::Char,
            AnalyzedTypeId::Integer(size) => GenericAnalyzedTypeId::Integer(*size),
            AnalyzedTypeId::Pointer(inner) => {
                GenericAnalyzedTypeId::Pointer(Box::new(Self::from_analyzed_type_id(inner)))
            }
            AnalyzedTypeId::StructType(id, _) => GenericAnalyzedTypeId::StructType(id.clone()),
            AnalyzedTypeId::GenericType(name) => GenericAnalyzedTypeId::GenericType(name.clone()),
        }
    }
}

impl CollectedFunctionData {
    pub fn map_function_id(
        &self,
        parsed_function_id: &ParsedFunctionId,
        impl_type: Option<&AnalyzedTypeId>,
    ) -> Option<FunctionId> {
        if let Some(resolved_type) = impl_type {
            let generic_resolved_type = GenericAnalyzedTypeId::from_analyzed_type_id(resolved_type);
            let module_member_functions = self
                .member_functions
                .get(&parsed_function_id.item_id.module_id)?;
            let module_member_function_imports = self
                .member_function_imports
                .get(&parsed_function_id.item_id.module_id)?;
            if let Some(type_functions) = module_member_functions.get(&generic_resolved_type) {
                if let Some(function_id) = type_functions.get(&parsed_function_id.item_id.item_name)
                {
                    return Some(function_id.clone());
                }
            }
            if parsed_function_id.is_module_local {
                if let Some(type_functions) =
                    module_member_function_imports.get(&generic_resolved_type)
                {
                    if let Some(function_id) =
                        type_functions.get(&parsed_function_id.item_id.item_name)
                    {
                        return Some(function_id.clone());
                    }
                }
            }
        } else {
            let module_functions = self.functions.get(&parsed_function_id.item_id.module_id)?;
            let module_function_imports = self
                .function_imports
                .get(&parsed_function_id.item_id.module_id)?;
            if let Some(function_id) = module_functions.get(&parsed_function_id.item_id.item_name) {
                return Some(function_id.clone());
            }
            if parsed_function_id.is_module_local {
                if let Some(function_id) =
                    module_function_imports.get(&parsed_function_id.item_id.item_name)
                {
                    return Some(function_id.clone());
                }
                if let Some(builtin_fn_id) = self
                    .builtin_functions
                    .get(&parsed_function_id.item_id.item_name)
                {
                    return Some(builtin_fn_id.clone());
                }
            }
        }
        None
    }
}

pub fn collect_function_data(
    program: &ParsedProgram,
    resolved_types: &ResolvedTypes,
) -> MergerResult<CollectedFunctionData> {
    let mut builtin_functions = HashMap::new();
    BuiltinFunction::add_builtin_function_ids(&mut builtin_functions);
    let (functions, member_functions) = collect_functions(program, resolved_types)?;
    let (function_imports, member_function_imports) =
        collect_function_imports(program, &functions, &member_functions, resolved_types)?;
    Ok(CollectedFunctionData {
        functions,
        member_functions,
        function_imports,
        member_function_imports,
        builtin_functions,
    })
}

fn collect_functions(
    program: &ParsedProgram,
    resolved_types: &ResolvedTypes,
) -> MergerResult<(
    HashMap<ModuleIdentifier, HashMap<String, FunctionId>>,
    HashMap<ModuleIdentifier, HashMap<GenericAnalyzedTypeId, HashMap<String, FunctionId>>>,
)> {
    let mut functions = HashMap::new();
    let mut member_functions = HashMap::new();
    for (module_id, module) in &program.module_tree {
        let mut module_functions = HashMap::new();
        let mut module_member_functions = HashMap::new();
        for function_def in &module.functions {
            validate_function_name(&function_def.value.function_name)?;
            let id = FunctionId {
                item_id: ItemId {
                    module_id: module_id.clone(),
                    item_name: function_def.value.function_name.clone(),
                },
                impl_type: None,
            };
            if module_functions
                .insert(function_def.value.function_name.clone(), id)
                .is_some()
            {
                return Err(anyhow::anyhow!(
                    "Duplicate function definition '{}' at {}",
                    function_def.value.function_name,
                    function_def.location
                ));
            }
        }
        for type_impl in &module.type_implementations {
            let resolved_type = resolved_types
                .resolve_generic_type(
                    &type_impl.value.impl_type.value,
                    &type_impl.value.generic_params,
                )
                .ok_or_else(|| {
                    anyhow::anyhow!(
                        "Type {} not found at {}",
                        type_impl.value.impl_type.value,
                        type_impl.location
                    )
                })?;
            let generic_resolved_type =
                GenericAnalyzedTypeId::from_analyzed_type_id(&resolved_type);
            let type_member_functions = module_member_functions
                .entry(generic_resolved_type.clone())
                .or_insert_with(HashMap::new);
            for function_def in &type_impl.value.functions {
                validate_function_name(&function_def.value.function_name)?;
                let id = FunctionId {
                    item_id: ItemId {
                        module_id: module_id.clone(),
                        item_name: function_def.value.function_name.clone(),
                    },
                    impl_type: Some(resolved_type.clone()),
                };
                if type_member_functions
                    .insert(function_def.value.function_name.clone(), id)
                    .is_some()
                {
                    return Err(anyhow::anyhow!(
                        "Duplicate member function definition '{}' at {}",
                        function_def.value.function_name,
                        function_def.location
                    ));
                }
            }
        }
        member_functions.insert(module_id.clone(), module_member_functions);
        functions.insert(module_id.clone(), module_functions);
    }
    Ok((functions, member_functions))
}

fn collect_function_imports(
    program: &ParsedProgram,
    functions: &HashMap<ModuleIdentifier, HashMap<String, FunctionId>>,
    member_functions: &HashMap<
        ModuleIdentifier,
        HashMap<GenericAnalyzedTypeId, HashMap<String, FunctionId>>,
    >,
    resolved_types: &ResolvedTypes,
) -> MergerResult<(
    HashMap<ModuleIdentifier, HashMap<String, FunctionId>>,
    HashMap<ModuleIdentifier, HashMap<GenericAnalyzedTypeId, HashMap<String, FunctionId>>>,
)> {
    let mut function_imports = HashMap::new();
    let mut member_function_imports = HashMap::new();
    for (module_id, module) in &program.module_tree {
        let mut module_function_imports = HashMap::new();
        let mut module_member_function_imports = HashMap::new();
        for import in &module.imports {
            if import.value.impl_type.is_some() {
                continue;
            }
            let module_functions = functions.get(&import.value.module_id).unwrap();
            if let Some(obj) = &import.value.imported_object {
                if let Some(id) = module_functions.get(obj) {
                    if module_function_imports
                        .insert(obj.clone(), id.clone())
                        .is_some()
                    {
                        return Err(anyhow::anyhow!(
                            "Duplicate function import '{}' at {}",
                            obj,
                            import.location
                        ));
                    }
                }
            } else {
                for (name, id) in module_functions {
                    if module_function_imports
                        .insert(name.clone(), id.clone())
                        .is_some()
                    {
                        return Err(anyhow::anyhow!(
                            "Duplicate function import '{}' at {}",
                            name,
                            import.location
                        ));
                    }
                }
            }
        }
        function_imports.insert(module_id.clone(), module_function_imports);
        for import in &module.imports {
            if import.value.impl_type.is_none() {
                continue;
            }
            let impl_type = import.value.impl_type.as_ref().unwrap();
            let module_member_functions = member_functions.get(&import.value.module_id).unwrap();
            let resolved_type = resolved_types
                .resolve_type(&impl_type.value)
                .ok_or_else(|| {
                    anyhow::anyhow!(
                        "Type {:?} not found at {}",
                        impl_type.value,
                        import.location
                    )
                })?;
            let generic_resolved_type =
                GenericAnalyzedTypeId::from_analyzed_type_id(&resolved_type);
            let type_member_functions = module_member_function_imports
                .entry(generic_resolved_type.clone())
                .or_insert_with(HashMap::new);
            if let Some(module_type_functions) = module_member_functions.get(&generic_resolved_type)
            {
                if let Some(obj) = &import.value.imported_object {
                    if let Some(id) = module_type_functions.get(obj) {
                        if type_member_functions
                            .insert(obj.clone(), id.clone())
                            .is_some()
                        {
                            return Err(anyhow::anyhow!(
                                "Duplicate member function import '{}' at {}",
                                obj,
                                import.location
                            ));
                        }
                    }
                } else {
                    for (name, id) in module_type_functions {
                        if type_member_functions
                            .insert(name.clone(), id.clone())
                            .is_some()
                        {
                            return Err(anyhow::anyhow!(
                                "Duplicate member function import '{}' at {}",
                                name,
                                import.location
                            ));
                        }
                    }
                }
            }
        }
        member_function_imports.insert(module_id.clone(), module_member_function_imports);
    }
    Ok((function_imports, member_function_imports))
}

fn validate_function_name(struct_name: &str) -> MergerResult<()> {
    let builtin_types = ["unit", "bool", "char", "byte", "short", "int", "long"];
    if builtin_types.contains(&struct_name) {
        Err(anyhow::anyhow!(
            "Type '{}' is a builtin type and cannot be redefined",
            struct_name
        ))
    } else {
        Ok(())
    }
}
