use crate::compiler::analyzer::analyzed_expression::{AnalyzedBinaryOp, BinaryAssignOp};
use crate::compiler::analyzer::type_resolver::AnalyzedType;
use crate::compiler::parser::parsed_expression::UnaryMathOp;
use crate::compiler::resolver::program_resolver::ResolverContext;

#[derive(Debug, Clone)]
pub struct ResolvedProgram {
    pub functions: Vec<ResolvedFunction>,
    pub constants: Vec<Vec<u8>>,
}

#[derive(Debug, Clone)]
pub struct ValueData {
    pub size: usize,
    pub location: ValueLocation,
}

#[derive(Debug, Clone)]
pub enum ValueLocation {
    Stack,
    Register,
    None,
}

impl ValueData {
    pub fn from_type(ty: &AnalyzedType, resolver_context: &ResolverContext) -> ValueData {
        match ty {
            AnalyzedType::Unit => ValueData {
                location: ValueLocation::None,
                size: 0,
            },
            AnalyzedType::Struct(struct_name) => {
                let size = resolver_context
                    .resolved_types
                    .struct_types
                    .get(struct_name)
                    .unwrap()
                    .size;
                ValueData {
                    location: ValueLocation::Stack,
                    size,
                }
            }
            AnalyzedType::Bool => ValueData {
                location: ValueLocation::Register,
                size: 1,
            },
            AnalyzedType::Char => ValueData {
                location: ValueLocation::Register,
                size: 1,
            },
            AnalyzedType::Integer(size) => ValueData {
                location: ValueLocation::Register,
                size: *size,
            },
            AnalyzedType::Pointer(_) => ValueData {
                location: ValueLocation::Register,
                size: 8,
            },
        }
    }
    pub fn discard_stack_size(&self, should_discard: bool) -> usize {
        if let ValueLocation::Stack = self.location {
            if should_discard {
                self.size
            } else {
                0
            }
        } else {
            0
        }
    }
}

#[derive(Debug, Clone)]
pub enum FunctionReturnLocation {
    Stack { offset: isize, size: usize },
    Register,
    None,
}

#[derive(Debug, Clone)]
pub struct ResolvedFunction {
    pub name: String,
    pub body: ResolvedExpression,
    pub value_location: FunctionReturnLocation,
    pub local_var_stack_size: usize,
}

#[derive(Debug, Clone)]
pub struct ResolvedExpression {
    pub kind: ResolvedExpressionKind,
    pub value_data: ValueData,
    pub stack_discard: usize,
}

#[derive(Debug, Clone)]
pub enum ResolvedExpressionKind {
    Block(Vec<ResolvedExpression>),
    Return(Option<Box<ResolvedExpression>>),
    Continue,
    Break {
        maybe_expr: Option<Box<ResolvedExpression>>,
    },
    If {
        condition: Box<ResolvedExpression>,
        then_block: Box<ResolvedExpression>,
        else_expr: Option<Box<ResolvedExpression>>,
    },
    While {
        condition: Box<ResolvedExpression>,
        loop_body: Box<ResolvedExpression>,
        else_expr: Option<Box<ResolvedExpression>>,
    },
    Declaration {
        var_offset: isize,
        value: Box<ResolvedExpression>,
    },
    ValueOfAssignable(ResolvedAssignableExpression),
    Literal(ResolvedLiteral),
    ConstantPointer(usize),
    Unary {
        op: ResolvedUnaryOp,
        expr: Box<ResolvedExpression>,
    },
    Binary {
        op: AnalyzedBinaryOp,
        left: Box<ResolvedExpression>,
        right: Box<ResolvedExpression>,
    },
    Assign {
        op: BinaryAssignOp,
        lhs: ResolvedAssignableExpression,
        rhs: Box<ResolvedExpression>,
    },
    Borrow {
        expr: ResolvedAssignableExpression,
    },
    FunctionCall {
        function_name: String,
        args: Vec<ResolvedExpression>,
        return_stack_space: usize,
    },
    FieldAccess {
        expr: Box<ResolvedExpression>,
        field_offset: usize,
        struct_size: usize,
    },
    Increment(ResolvedAssignableExpression, bool),
    Decrement(ResolvedAssignableExpression, bool),
}

#[derive(Debug, Clone)]
pub enum ResolvedLiteral {
    Unit,
    Bool(bool),
    Char(i8),
    Integer(i64),
    Struct(Vec<ResolvedExpression>),
}

#[derive(Debug, Clone)]
pub enum ResolvedAssignableExpression {
    LocalVariable(isize),
    Dereference(Box<ResolvedExpression>),
    FieldAccess(Box<ResolvedAssignableExpression>, usize),
    PointerFieldAccess(Box<ResolvedExpression>, usize),
    ArrayIndex(Box<ResolvedExpression>, Box<ResolvedExpression>, usize),
}

#[derive(Debug, Clone)]
pub enum ResolvedUnaryOp {
    Math(UnaryMathOp),
    LogicalNot,
    PointerCast,
    IntCast,
    BoolCast,
}
