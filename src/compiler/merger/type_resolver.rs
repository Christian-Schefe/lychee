use crate::compiler::analyzer::analyzed_type::{GenericIdKind, GenericParams};
use crate::compiler::builtin;
use crate::compiler::lexer::location::Src;
use crate::compiler::merger::merged_expression::{ResolvedEnum, ResolvedStruct, StructId};
use crate::compiler::merger::resolved_types::ResolvedTypes;
use crate::compiler::merger::type_collector::{collect_type_data, CollectedTypeData};
use crate::compiler::merger::MergerResult;
use crate::compiler::parser::item_id::ItemId;
use crate::compiler::parser::parsed_expression::{
    ParsedEnumDefinition, ParsedLiteral, ParsedModule, ParsedProgram, ParsedStructDefinition,
};
use crate::compiler::parser::ModuleIdentifier;
use std::collections::HashMap;

pub fn build_resolved_types(program: &ParsedProgram) -> MergerResult<ResolvedTypes> {
    let collected_type_data = collect_type_data(program)?;

    let module_defs = extract_module_struct_types(&program.module_tree)?;

    let mut resolved_structs = HashMap::new();
    let mut resolved_enums = HashMap::new();

    builtin::BuiltinStruct::add_builtin_resolved_structs(&mut resolved_structs);

    for (module_id, (module_struct_defs, module_enum_defs)) in &module_defs {
        for (_, struct_def) in module_struct_defs {
            let resolved_struct =
                resolve_struct_definition(struct_def, module_id, &collected_type_data)?;
            resolved_structs.insert(resolved_struct.id.clone(), resolved_struct);
        }
        for (_, enum_def) in module_enum_defs {
            let resolved_enum = resolve_enum_definition(enum_def, module_id)?;
            resolved_enums.insert(resolved_enum.id.clone(), resolved_enum);
        }
    }

    Ok(ResolvedTypes {
        structs: resolved_structs,
        enums: resolved_enums,
        collected_type_data,
    })
}

fn resolve_struct_definition(
    struct_def: &Src<ParsedStructDefinition>,
    module_path: &ModuleIdentifier,
    collected_type_data: &CollectedTypeData,
) -> MergerResult<ResolvedStruct> {
    let struct_id = StructId {
        id: ItemId {
            module_id: module_path.clone(),
            item_name: struct_def.value.struct_name.clone(),
        },
        generic_count: struct_def.value.generics.order.len(),
    };

    let mut field_types = HashMap::new();
    let mut field_order = Vec::new();

    let resolved_generic_params = GenericParams::from(
        GenericIdKind::Struct(struct_id.clone()),
        &struct_def.value.generics,
    );

    for (field_name, field_type) in &struct_def.value.fields {
        field_order.push(field_name.clone());
        let resolved_field_type = collected_type_data
            .map_generic_parsed_type(&field_type.value, &resolved_generic_params)
            .ok_or_else(|| {
                anyhow::anyhow!(
                    "Type {} not found at {}",
                    field_type.value,
                    field_type.location
                )
            })?;
        if field_types
            .insert(field_name.clone(), resolved_field_type)
            .is_some()
        {
            return Err(anyhow::anyhow!(
                "Duplicate field name '{}' in struct '{}' at {}",
                field_name,
                struct_def.value.struct_name,
                struct_def.location
            ));
        }
    }

    Ok(ResolvedStruct {
        id: struct_id,
        field_types,
        field_order,
        generic_params: resolved_generic_params,
    })
}

fn resolve_enum_definition(
    enum_def: &Src<ParsedEnumDefinition>,
    module_path: &ModuleIdentifier,
) -> MergerResult<ResolvedEnum> {
    let enum_id = ItemId {
        module_id: module_path.clone(),
        item_name: enum_def.value.enum_name.clone(),
    };

    let mut variants = HashMap::new();

    for (variant_name, variant_value) in &enum_def.value.variants {
        let value = match variant_value {
            Some(ParsedLiteral::Integer(i)) => *i,
            Some(_) => {
                return Err(anyhow::anyhow!(
                    "Non-Integer literals are not supported as enum variants at {}",
                    enum_def.location
                ));
            }
            None => {
                let last_value = variants.values().max().unwrap_or(&-1);
                last_value + 1
            }
        };
        if let Some(_) = variants.insert(variant_name.clone(), value) {
            return Err(anyhow::anyhow!(
                "Duplicate variant name '{}' in enum '{}' at {}",
                variant_name,
                enum_def.value.enum_name,
                enum_def.location
            ));
        }
    }

    for (variant, value) in &variants {
        if *value < i32::MIN as i64 || *value > i32::MAX as i64 {
            return Err(anyhow::anyhow!(
                "Enum variant {} value {} out of range at {}",
                variant,
                value,
                enum_def.location
            ));
        }
    }

    Ok(ResolvedEnum {
        id: enum_id,
        variants,
    })
}

fn extract_module_struct_types(
    module_tree: &HashMap<ModuleIdentifier, ParsedModule>,
) -> MergerResult<
    HashMap<
        ModuleIdentifier,
        (
            HashMap<String, Src<ParsedStructDefinition>>,
            HashMap<String, Src<ParsedEnumDefinition>>,
        ),
    >,
> {
    let mut module_defs = HashMap::new();
    for module in module_tree.values() {
        let mut module_structs = HashMap::new();
        for struct_def in &module.struct_definitions {
            if let Some(_) =
                module_structs.insert(struct_def.value.struct_name.clone(), struct_def.clone())
            {
                return Err(anyhow::anyhow!(
                    "Duplicate struct definition '{}' at {}",
                    struct_def.value.struct_name.clone(),
                    struct_def.location
                ));
            }
        }
        let mut module_enums = HashMap::new();
        for enum_def in &module.enums {
            if let Some(_) = module_enums.insert(enum_def.value.enum_name.clone(), enum_def.clone())
            {
                return Err(anyhow::anyhow!(
                    "Duplicate enum definition '{}' at {}",
                    enum_def.value.enum_name.clone(),
                    enum_def.location
                ));
            }
        }
        module_defs.insert(module.module_path.clone(), (module_structs, module_enums));
    }
    Ok(module_defs)
}
