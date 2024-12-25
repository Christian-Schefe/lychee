use crate::compiler::analyzer::analyzed_expression::{
    AnalyzedBinaryOp, AnalyzedLiteral, BinaryAssignOp,
};
use crate::compiler::parser::parsed_expression::UnaryMathOp;
use crate::compiler::resolver::program_resolver::ResolverContext;
use crate::compiler::unwrapper::unwrapped_type::UnwrappedTypeId;

#[derive(Debug, Clone)]
pub struct ResolvedProgram {
    pub functions: Vec<ResolvedFunction>,
    pub constants: Vec<Vec<u8>>,
    pub main_function_name: String,
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
    pub fn from_type(ty: &UnwrappedTypeId, context: &ResolverContext) -> ValueData {
        let size = context.get_type_size(&ty);
        match ty {
            UnwrappedTypeId::Unit => ValueData {
                location: ValueLocation::None,
                size,
            },
            UnwrappedTypeId::StructType(_) => ValueData {
                location: ValueLocation::Stack,
                size,
            },
            UnwrappedTypeId::Bool
            | UnwrappedTypeId::Char
            | UnwrappedTypeId::Integer(_)
            | UnwrappedTypeId::Pointer(_) => ValueData {
                location: ValueLocation::Register,
                size,
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
    Loop {
        init: Option<Box<ResolvedExpression>>,
        condition: Option<Box<ResolvedExpression>>,
        step: Option<Box<ResolvedExpression>>,
        loop_body: Box<ResolvedExpression>,
        else_expr: Option<Box<ResolvedExpression>>,
    },
    Declaration {
        var_offset: isize,
        value: Box<ResolvedExpression>,
    },
    ValueOfAssignable(ResolvedAssignableExpression),
    Literal(AnalyzedLiteral),
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
    StructInstance {
        fields: Vec<ResolvedExpression>,
    },
}

#[derive(Debug, Clone)]
pub enum ResolvedAssignableExpression {
    LocalVariable(isize),
    Dereference(Box<ResolvedExpression>),
    FieldAccess(Box<ResolvedAssignableExpression>, usize),
    PointerFieldAccess(Box<ResolvedExpression>, usize, usize),
    ArrayIndex(Box<ResolvedExpression>, Box<ResolvedExpression>, usize),
}

#[derive(Debug, Clone)]
pub enum ResolvedUnaryOp {
    Math(UnaryMathOp),
    LogicalNot,
    PointerCast,
    IntCast(usize),
    BoolCast,
}
