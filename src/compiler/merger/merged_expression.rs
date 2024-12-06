use crate::compiler::lexer::location::Src;
use crate::compiler::merger::MergerResult;
use crate::compiler::parser::parsed_expression::{
    BinaryOp, ParsedType, ParsedTypeKind, UnaryMathOp,
};
use crate::compiler::parser::ModulePath;
use std::collections::HashMap;
use std::fmt::Display;

#[derive(Debug, Clone)]
pub struct MergedProgram {
    pub functions: HashMap<ModuleId, Src<MergedFunction>>,
    pub resolved_functions: ResolvedFunctions,
    pub resolved_types: ResolvedTypes,
}

#[derive(Debug, Clone)]
pub struct ResolvedTypes {
    pub structs: HashMap<TypeId, ResolvedStruct>,
    pub type_sizes: HashMap<TypeId, usize>,
    pub known_types: HashMap<ModuleId, TypeId>,
    pub builtin_types: HashMap<String, TypeId>,
}

impl ResolvedTypes {
    pub fn resolve_type(
        &self,
        current_module: &ModulePath,
        parsed_type: &ParsedType,
    ) -> MergerResult<TypeId> {
        match &parsed_type.value {
            ParsedTypeKind::Named(module_id) => {
                if module_id.module_path.len() == 0
                    && self.builtin_types.contains_key(&module_id.name)
                {
                    return self
                        .builtin_types
                        .get(&module_id.name)
                        .cloned()
                        .ok_or_else(|| {
                            anyhow::anyhow!(
                                "Builtin type '{}' not found in builtin types",
                                module_id.name
                            )
                        });
                }
                let resolved_module_id = ModuleId {
                    name: module_id.name.clone(),
                    module_path: current_module.resolve(&module_id.module_path),
                };
                self.known_types
                    .get(&resolved_module_id)
                    .cloned()
                    .ok_or_else(|| {
                        anyhow::anyhow!(
                            "Type '{}' not found in known types at {}",
                            resolved_module_id,
                            parsed_type.location
                        )
                    })
            }
            ParsedTypeKind::Pointer(inner) => {
                let inner_type = self.resolve_type(current_module, inner)?;
                Ok(TypeId::Pointer(Box::new(inner_type)))
            }
        }
    }

    pub fn get_type_size(&self, ty: &TypeId) -> usize {
        match ty {
            TypeId::Unit => 0,
            TypeId::Bool => 1,
            TypeId::Char => 1,
            TypeId::Integer(size) => *size,
            TypeId::Pointer(_) => 8,
            TypeId::StructType(_) => *self.type_sizes.get(ty).unwrap(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ResolvedFunctions {
    pub functions: HashMap<ModuleId, ResolvedFunctionHeader>,
    pub builtin_functions: HashMap<String, ModuleId>,
}

impl ResolvedFunctions {
    pub fn resolve_function(
        &self,
        current_module: &ModulePath,
        parsed_function: &ModuleId,
    ) -> Option<&ResolvedFunctionHeader> {
        if parsed_function.module_path.len() == 0
            && self.builtin_functions.contains_key(&parsed_function.name)
        {
            return Some(
                self.functions
                    .get(self.builtin_functions.get(&parsed_function.name).unwrap())
                    .unwrap(),
            );
        }
        let resolved_module_id = ModuleId {
            name: parsed_function.name.clone(),
            module_path: current_module.resolve(&parsed_function.module_path),
        };
        self.functions.get(&resolved_module_id)
    }
}

#[derive(Debug, Clone)]
pub struct ResolvedFunctionHeader {
    pub id: ModuleId,
    pub return_type: TypeId,
    pub parameter_types: HashMap<String, TypeId>,
    pub parameter_order: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct ResolvedStruct {
    pub field_types: HashMap<String, TypeId>,
    pub field_order: Vec<String>,
    pub field_offsets: HashMap<String, usize>,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct ModuleId {
    pub name: String,
    pub module_path: ModulePath,
}

impl Display for ModuleId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}::{}", self.module_path.get_identifier(), self.name)
    }
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum TypeId {
    Unit,
    Bool,
    Char,
    Integer(usize),
    Pointer(Box<TypeId>),
    StructType(ModuleId),
}

impl Display for TypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeId::Unit => write!(f, "unit"),
            TypeId::Bool => write!(f, "bool"),
            TypeId::Char => write!(f, "char"),
            TypeId::Integer(size) => match size {
                1 => write!(f, "byte"),
                2 => write!(f, "short"),
                4 => write!(f, "int"),
                8 => write!(f, "long"),
                _ => write!(f, "i{}", size * 8),
            },
            TypeId::Pointer(inner) => write!(f, "&{}", inner),
            TypeId::StructType(module_id) => write!(f, "{}", module_id.name),
        }
    }
}

#[derive(Debug, Clone)]
pub struct MergedFunction {
    pub body: MergedExpression,
}

pub type MergedExpression = Src<MergedExpressionKind>;

#[derive(Debug, Clone)]
pub enum MergedExpressionKind {
    Block {
        expressions: Vec<MergedExpression>,
        returns_value: bool,
    },
    Return(Option<Box<MergedExpression>>),
    Continue,
    Break(Option<Box<MergedExpression>>),
    If {
        condition: Box<MergedExpression>,
        then_block: Box<MergedExpression>,
        else_expr: Option<Box<MergedExpression>>,
    },
    While {
        condition: Box<MergedExpression>,
        loop_body: Box<MergedExpression>,
        else_expr: Option<Box<MergedExpression>>,
    },
    For {
        init: Box<MergedExpression>,
        condition: Box<MergedExpression>,
        step: Box<MergedExpression>,
        loop_body: Box<MergedExpression>,
        else_expr: Option<Box<MergedExpression>>,
    },
    Declaration {
        var_type: Option<TypeId>,
        var_name: String,
        value: Box<MergedExpression>,
    },
    Variable(String),
    Literal(MergedLiteral),
    Unary {
        op: MergedUnaryOp,
        expr: Box<MergedExpression>,
    },
    Binary {
        op: BinaryOp,
        left: Box<MergedExpression>,
        right: Box<MergedExpression>,
    },
    FunctionCall {
        function_id: ModuleId,
        args: Vec<MergedExpression>,
    },
}

#[derive(Debug, Clone)]
pub enum MergedLiteral {
    Unit,
    Bool(bool),
    Char(i8),
    Integer(i64),
    String(String),
    Struct(TypeId, HashMap<String, MergedExpression>),
}

#[derive(Debug, Clone)]
pub enum MergedUnaryOp {
    Math(UnaryMathOp),
    LogicalNot,
    Borrow,
    Dereference,
    Increment { is_prefix: bool },
    Decrement { is_prefix: bool },
    Cast(TypeId),
    Member(String),
    Index(Box<MergedExpression>),
}
