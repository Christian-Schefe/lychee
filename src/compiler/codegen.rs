mod builtin_functions;

use crate::compiler::codegen::builtin_functions::add_builtin_fn_code;
use crate::compiler::parser::analyzed_syntax_tree::{AnalyzedAddressableExpression, AnalyzedExpression, AnalyzedFunction, AnalyzedProgram, AnalyzedStatement, TypedAnalyzedAddressableExpression, TypedAnalyzedExpression};
use crate::compiler::parser::syntax_tree::{BinaryComparisonOp, BinaryLogicOp, BinaryMathOp, BinaryOp, Literal, UnaryOp};
use crate::compiler::parser::types::Type;
use std::collections::HashMap;
use std::path::PathBuf;

struct Context {
    output: Vec<String>,
    function_data: HashMap<String, FunctionData>,
    struct_data: HashMap<String, StructData>,
    label_counter: usize,
    current_data: CurrentData,
}

struct FunctionData {
    label: String,
    args: HashMap<String, VarData>,
    arg_order: Vec<String>,
    return_location: FunctionResultLocation,
}

struct StructData {
    fields: HashMap<String, VarData>,
    field_order: Vec<String>,
}

#[derive(Debug, Clone)]
struct VarData {
    offset: isize,
    byte_size: usize,
}

struct CurrentData {
    function_name: String,
    local_variables: HashMap<String, VarData>,
    stack_offset: isize,
    return_label: String,
}

impl Context {
    fn push(&mut self, line: &str) {
        self.output.push(line.to_string());
    }

    fn push_label(&mut self, label: String) {
        self.output.push(format!("{}:", label));
    }

    fn get_new_label(&mut self) -> String {
        let label = format!("_L{}", self.label_counter);
        self.label_counter += 1;
        label
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
enum ExprResultLocation {
    Register(usize),
    Stack,
    Discard,
}

#[derive(Debug, Clone, Eq, PartialEq)]
enum FunctionResultLocation {
    Register(usize),
    Stack(isize),
    Discard,
}

pub fn generate_code(program: AnalyzedProgram, output: &PathBuf) {
    let mut context = Context {
        output: Vec::new(),
        function_data: HashMap::new(),
        struct_data: HashMap::new(),
        current_data: CurrentData {
            function_name: String::new(),
            local_variables: HashMap::new(),
            stack_offset: 0,
            return_label: String::new(),
        },
        label_counter: 0,
    };

    for (struct_name, struct_def) in &program.struct_definitions {
        let mut fields = HashMap::new();
        let mut field_order = Vec::new();
        let mut offset = 0;
        for (field_name, ty) in &struct_def.fields {
            let field_size = ty.size();
            fields.insert(field_name.clone(), VarData {
                offset: offset as isize,
                byte_size: field_size,
            });
            field_order.push(field_name.clone());
            offset += field_size;
        }
        let struct_data = StructData { fields, field_order };
        context.struct_data.insert(struct_name.clone(), struct_data);
    }

    for (name, func) in &program.functions {
        let fun_label = context.get_new_label();
        let return_type = &func.return_type;
        let mut args = HashMap::new();
        let mut arg_order = Vec::new();

        let mut offset = 16;
        for (arg_name, ty) in func.args.iter().rev() {
            let arg_size = ty.size();
            args.insert(arg_name.clone(), VarData {
                offset: offset as isize,
                byte_size: arg_size,
            });
            offset += arg_size;
            arg_order.push(arg_name.clone());
        }

        let return_location = match return_type {
            Type::Struct { .. } => FunctionResultLocation::Stack(offset as isize),
            ty if ty.size() > 0 => FunctionResultLocation::Register(0),
            _ => FunctionResultLocation::Discard,
        };

        let function_data = FunctionData {
            label: fun_label,
            args,
            arg_order,
            return_location,
        };
        context.function_data.insert(name.clone(), function_data);
    }

    context.push(&format!(
        "call {}",
        context.function_data[&program.main_function].label
    ));

    match &context.function_data[&program.main_function].return_location
    {
        FunctionResultLocation::Discard => context.push("movi r0 0"),
        FunctionResultLocation::Register(reg) => if *reg != 0 {
            context.push(&format!("mov r0 r{}", reg))
        },
        FunctionResultLocation::Stack(_) => unreachable!("Main function cannot return a struct"),
    }
    context.push("exit");

    add_builtin_fn_code(&mut context);

    for (_, function) in program.functions {
        generate_function_code(&mut context, function)
    }

    let result = context.output.join("\n");
    std::fs::write(output, result).unwrap();
}

fn generate_function_code(context: &mut Context, function: AnalyzedFunction) {
    context.current_data.function_name = function.name.clone();
    context.current_data.local_variables.clear();
    context.current_data.stack_offset = 0;

    let return_label = context.get_new_label();
    context.current_data.return_label = return_label.clone();

    context.push_label(context.function_data[&function.name].label.clone());
    context.push("push #64 bp");
    context.push("mov bp sp");

    if function.local_var_stack_size > 0 {
        context.push(&format!("subi sp {}", function.local_var_stack_size));
    }

    for (arg_name, _) in function.args.iter().rev() {
        let arg_data = &context.function_data[&function.name].args[arg_name];
        context.current_data.local_variables.insert(arg_name.clone(), arg_data.clone());
    }

    let return_location = context.function_data[&function.name].return_location.clone();
    match return_location {
        FunctionResultLocation::Register(reg) => {
            generate_expression_code(context, function.expr, ExprResultLocation::Register(reg));
            context.push_label(return_label);
        }
        FunctionResultLocation::Stack(offset) => {
            generate_expression_code(context, function.expr, ExprResultLocation::Stack);
            context.push_label(return_label);
            context.push(&format!("movi r0 {}", function.return_type.size()));
            context.push(&format!("popmem r0 [bp;{}]", offset));
        }
        FunctionResultLocation::Discard => {
            generate_expression_code(context, function.expr, ExprResultLocation::Discard);
            context.push_label(return_label);
        }
    }

    context.push("mov sp bp");
    context.push("pop #64 bp");
    context.push("ret");
}

fn generate_statement_code(context: &mut Context, statement: AnalyzedStatement) {
    let fn_data = &context.function_data[&context.current_data.function_name];
    match statement {
        AnalyzedStatement::Return(expr) => {
            let return_label = context.current_data.return_label.clone();
            if let Some(return_expr) = expr {
                match fn_data.return_location {
                    FunctionResultLocation::Register(reg) => {
                        generate_expression_code(context, return_expr, ExprResultLocation::Register(reg));
                    }
                    FunctionResultLocation::Stack(_) => {
                        generate_expression_code(context, return_expr, ExprResultLocation::Stack);
                    }
                    FunctionResultLocation::Discard => {
                        generate_expression_code(context, return_expr, ExprResultLocation::Discard);
                    }
                }
            }
            context.push(&format!("jmp {}", return_label));
        }
        AnalyzedStatement::Declaration {
            var_type,
            name,
            value,
        } => {
            let byte_size = var_type.size();
            context.current_data.stack_offset -= byte_size as isize;
            let offset = context.current_data.stack_offset;
            context.current_data.local_variables.insert(name.clone(), VarData {
                offset,
                byte_size,
            });

            if let Type::Struct { .. } = var_type {
                generate_expression_code(context, value, ExprResultLocation::Stack);
                context.push(&format!("movi r0 {}", byte_size));
                context.push(&format!("popmem r0 [bp;{}]", offset));
            } else {
                if byte_size > 0 {
                    generate_expression_code(context, value, ExprResultLocation::Register(0));
                    context.push(&format!("store #{} r0 [bp;{}]", byte_size * 8, offset));
                } else {
                    generate_expression_code(context, value, ExprResultLocation::Discard);
                }
            }
        }
        AnalyzedStatement::Expr(expr) => {
            generate_expression_code(context, expr, ExprResultLocation::Discard);
        }
        AnalyzedStatement::If {
            condition,
            true_expr,
            false_statement,
        } => {
            let false_label = context.get_new_label();
            let end_label = context.get_new_label();
            generate_expression_code(context, condition, ExprResultLocation::Register(0));
            context.push("cmpi r0 0");
            context.push(&format!("jz {}", false_label));
            generate_expression_code(context, true_expr, ExprResultLocation::Discard);
            context.push(&format!("jmp {}", end_label));
            context.push_label(false_label);
            if let Some(false_statement) = false_statement {
                generate_statement_code(context, *false_statement);
            }
            context.push_label(end_label);
        }
        AnalyzedStatement::For {
            init,
            condition,
            update,
            body,
        } => {
            generate_statement_code(context, *init);
            let loop_label = context.get_new_label();
            let end_label = context.get_new_label();
            context.push_label(loop_label.clone());
            generate_expression_code(context, condition, ExprResultLocation::Register(0));
            context.push("cmpi r0 0");
            context.push(&format!("jz {}", end_label));
            generate_expression_code(context, body, ExprResultLocation::Discard);
            generate_expression_code(context, update, ExprResultLocation::Discard);
            context.push(&format!("jmp {}", loop_label));
            context.push_label(end_label);
        }
        AnalyzedStatement::While {
            condition,
            body,
            is_do_while,
        } => {
            let loop_label = context.get_new_label();
            context.push_label(loop_label.clone());
            if !is_do_while {
                let end_label = context.get_new_label();
                generate_expression_code(context, condition, ExprResultLocation::Register(0));
                context.push("cmpi r0 0");
                context.push(&format!("jz {}", end_label));
                generate_expression_code(context, body, ExprResultLocation::Discard);
                context.push(&format!("jmp {}", loop_label));
                context.push_label(end_label);
            } else {
                generate_expression_code(context, body, ExprResultLocation::Discard);
                generate_expression_code(context, condition, ExprResultLocation::Register(0));
                context.push("cmpi r0 0");
                context.push(&format!("jnz {}", loop_label));
            }
        }
    }
}

fn generate_expression_code(context: &mut Context, expr: TypedAnalyzedExpression, result_location: ExprResultLocation) {
    match expr.value {
        AnalyzedExpression::Literal(literal) => match result_location {
            ExprResultLocation::Register(reg) => match literal {
                Literal::Integer(integer) => {
                    context.push(&format!("movi r{} {}", reg, integer));
                }
                Literal::Bool(boolean) => {
                    context.push(&format!("movi r{} {}", reg, if boolean { 1 } else { 0 }));
                }
                Literal::Char(char) => {
                    context.push(&format!("movi r{} {}", reg, char));
                }
                Literal::Unit => {}
            }
            ExprResultLocation::Stack => match literal {
                Literal::Integer(integer) => {
                    context.push(&format!("movi r0 {}", integer));
                    let size = expr.ty.size() * 8;
                    context.push(&format!("push #{} r0", size));
                }
                Literal::Bool(boolean) => {
                    context.push(&format!("movi r0 {}", if boolean { 1 } else { 0 }));
                    let size = expr.ty.size() * 8;
                    context.push(&format!("push #{} r0", size));
                }
                Literal::Char(char) => {
                    context.push(&format!("movi r0 {}", char));
                    let size = expr.ty.size() * 8;
                    context.push(&format!("push #{} r0", size));
                }
                Literal::Unit => {}
            }
            ExprResultLocation::Discard => {}
        }
        AnalyzedExpression::Sizeof(ty) => {
            match result_location {
                ExprResultLocation::Register(reg) => {
                    context.push(&format!("movi r{} {}", reg, ty.size()));
                }
                ExprResultLocation::Stack => {
                    context.push(&format!("movi r0 {}", ty.size()));
                    context.push(&format!("push #{} r0", expr.ty.size() * 8));
                }
                ExprResultLocation::Discard => {}
            }
        }
        AnalyzedExpression::FunctionCall { function, mut args } => {
            let function_return_location = context.function_data[&function].return_location.clone();
            let return_bytes = expr.ty.size();
            let mut total_byte_size = 0;
            if let FunctionResultLocation::Stack(_) = function_return_location {
                context.push(&format!("subi sp {}", return_bytes));
                total_byte_size += return_bytes;
            }

            let arg_exprs = context.function_data[&function].arg_order.iter().map(|arg_name| {
                (arg_name.clone(), args.remove(arg_name).unwrap_or_else(|| {
                    panic!("Argument {} not found in function call", arg_name)
                }))
            }).collect::<Vec<(String, TypedAnalyzedExpression)>>();

            for (arg_name, arg_expr) in arg_exprs {
                let arg_bytes = context.function_data[&function].args[&arg_name].byte_size;
                let arg_location = if arg_bytes > 0 {
                    ExprResultLocation::Stack
                } else {
                    ExprResultLocation::Discard
                };
                generate_expression_code(context, arg_expr, arg_location);
                total_byte_size += arg_bytes;
            }
            context.push(&format!("call {}", context.function_data[&function].label));
            match (result_location, function_return_location) {
                (ExprResultLocation::Register(reg), FunctionResultLocation::Register(func_reg)) => {
                    context.push(&format!("addi sp {}", total_byte_size));
                    if reg != func_reg {
                        context.push(&format!("mov r{} r{}", reg, func_reg));
                    }
                }
                (ExprResultLocation::Stack, FunctionResultLocation::Stack(_)) => {
                    context.push(&format!("addi sp {}", total_byte_size - return_bytes));
                }
                (ExprResultLocation::Stack, FunctionResultLocation::Register(func_reg)) => {
                    let return_size = expr.ty.size() * 8;
                    context.push(&format!("addi sp {}", total_byte_size));
                    context.push(&format!("push #{} r{}", return_size, func_reg));
                }
                (ExprResultLocation::Discard, FunctionResultLocation::Discard) => {
                    context.push(&format!("addi sp {}", total_byte_size));
                }
                (a, b) => panic!("Invalid function call result location: {:?} {:?}", a, b),
            }
        }
        AnalyzedExpression::Block(statements, final_expr) => {
            for statement in statements {
                generate_statement_code(context, statement);
            }
            if let Some(final_expr) = final_expr {
                generate_expression_code(context, *final_expr, result_location);
            }
        }
        AnalyzedExpression::Unary { op, expr: inner_expr } => {
            match op {
                UnaryOp::Negate => {
                    generate_expression_code(context, *inner_expr, ExprResultLocation::Register(0));
                    context.push("neg r0");
                    let size = expr.ty.size() * 8;
                    move_register_to_location(context, 0, size, result_location);
                }
                UnaryOp::Positive => {
                    generate_expression_code(context, *inner_expr, result_location);
                }
                UnaryOp::Not => {
                    generate_expression_code(context, *inner_expr, ExprResultLocation::Register(0));
                    context.push("not r0");
                    let size = expr.ty.size() * 8;
                    move_register_to_location(context, 0, size, result_location);
                }
                UnaryOp::LogicalNot => {
                    generate_expression_code(context, *inner_expr, ExprResultLocation::Register(0));
                    context.push("cmpi r0 0");
                    context.push("setz r0");
                    let size = expr.ty.size() * 8;
                    move_register_to_location(context, 0, size, result_location);
                }
            }
        }
        AnalyzedExpression::Increment { is_increment, postfix, expr } => {
            load_var(context, *expr.clone(), ExprResultLocation::Register(0));
            let size = expr.ty.size() * 8;
            if is_increment {
                if postfix {
                    context.push("push #64 r0");
                    context.push("addi r0 1");
                } else {
                    context.push("addi r0 1");
                    context.push("push #64 r0");
                }
            } else {
                if postfix {
                    context.push("push #64 r0");
                    context.push("subi r0 1");
                } else {
                    context.push("subi r0 1");
                    context.push("push #64 r0");
                }
            }
            store_reg0_in_var(context, *expr);
            context.push("pop #64 r0");

            move_register_to_location(context, 0, size, result_location);
        }
        AnalyzedExpression::Ternary {
            condition,
            true_expr,
            false_expr,
        } => {
            let false_label = context.get_new_label();
            let end_label = context.get_new_label();
            generate_expression_code(context, *condition, ExprResultLocation::Register(0));
            context.push("cmpi r0 0");
            context.push(&format!("jz {}", false_label));
            generate_expression_code(context, *true_expr, result_location.clone());
            context.push(&format!("jmp {}", end_label));
            context.push_label(false_label);
            generate_expression_code(context, *false_expr, result_location);
            context.push_label(end_label);
        }
        AnalyzedExpression::Binary { op, left, right } => {
            let byte_size = expr.ty.size();
            match op {
                BinaryOp::Logical(logic_op) => generate_logic_binop(context, logic_op, byte_size, *left, *right, result_location),
                BinaryOp::Math(math_op) => generate_math_binop(context, math_op, byte_size, *left, *right, result_location),
                BinaryOp::Comparison(comp_op) => generate_comparison_binop(context, comp_op, byte_size, *left, *right, result_location),
                _ => unreachable!("Invalid binary operator"),
            }
        }
        AnalyzedExpression::BinaryAssign { op, left, right } => {
            let byte_size = expr.ty.size();
            match op {
                BinaryOp::Assign => generate_assignment_binop(context, byte_size, *left, *right, result_location),
                BinaryOp::MathAssign(math_op) => generate_math_assignment_binop(context, math_op, byte_size, *left, *right, result_location),
                BinaryOp::LogicAssign(logic_op) => generate_logic_assignment_binop(context, logic_op, byte_size, *left, *right, result_location),
                _ => unreachable!("Invalid binary assign operator"),
            }
        }
        AnalyzedExpression::Cast { var_type, expr } => {
            generate_expression_code(context, *expr, ExprResultLocation::Register(0));
            let bit_size = var_type.size() * 8;
            if var_type == Type::Bool {
                context.push("cmpi r0 0");
                context.push("setnz r0");
            } else if bit_size < 64 {
                context.push(&format!("signext #{} r0", bit_size));
            }
            match result_location {
                ExprResultLocation::Register(reg) => {
                    if reg != 0 {
                        context.push(&format!("mov r{} r0", reg));
                    }
                }
                ExprResultLocation::Stack => {
                    context.push(&format!("push #{} r0", bit_size));
                }
                ExprResultLocation::Discard => {}
            }
        }
        AnalyzedExpression::StructLiteral { name, mut fields } => {
            let struct_data = &context.struct_data[&name];
            let field_exprs = struct_data.field_order.iter().rev().map(|field_name| {
                let field_expr = fields.remove(field_name).unwrap_or_else(|| {
                    panic!("Field {} not found in struct literal", field_name)
                });
                field_expr
            }).collect::<Vec<TypedAnalyzedExpression>>();
            for field_expr in field_exprs {
                generate_expression_code(context, field_expr, ExprResultLocation::Stack);
            }
            match result_location {
                ExprResultLocation::Register(_) => {
                    panic!("Struct literals cannot be assigned to registers");
                }
                ExprResultLocation::Stack => {}
                ExprResultLocation::Discard => {}
            }
        }
        AnalyzedExpression::Borrow(expr) => load_var_address(context, expr, result_location),
        AnalyzedExpression::Addressable(adr) => load_var(context, adr, result_location),
    }
}

fn generate_logic_binop(
    context: &mut Context,
    op: BinaryLogicOp,
    byte_size: usize,
    left: TypedAnalyzedExpression,
    right: TypedAnalyzedExpression,
    result_location: ExprResultLocation,
) {
    let end_label = context.get_new_label();
    generate_expression_code(context, left, ExprResultLocation::Register(0));
    context.push("cmpi r0 0");
    match op {
        BinaryLogicOp::Or => context.push(&format!("jnz {}", end_label)),
        BinaryLogicOp::And => context.push(&format!("jz {}", end_label)),
    }
    generate_expression_code(context, right, ExprResultLocation::Register(0));
    context.push_label(end_label);

    move_register_to_location(context, 0, byte_size * 8, result_location);
}

fn generate_math_binop(
    context: &mut Context,
    op: BinaryMathOp,
    byte_size: usize,
    left: TypedAnalyzedExpression,
    right: TypedAnalyzedExpression,
    result_location: ExprResultLocation,
) {
    generate_expression_code(context, left, ExprResultLocation::Register(0));
    context.push("push #64 r0");
    generate_expression_code(context, right, ExprResultLocation::Register(0));
    context.push("mov r1 r0");
    context.push("pop #64 r0");
    add_math_op_instruction(context, op);
    if byte_size < 8 {
        context.push(&format!("signext #{} r0", byte_size * 8));
    }

    move_register_to_location(context, 0, byte_size * 8, result_location);
}

fn generate_comparison_binop(
    context: &mut Context,
    op: BinaryComparisonOp,
    byte_size: usize,
    left: TypedAnalyzedExpression,
    right: TypedAnalyzedExpression,
    result_location: ExprResultLocation,
) {
    generate_expression_code(context, left, ExprResultLocation::Register(0));
    context.push("push #64 r0");
    generate_expression_code(context, right, ExprResultLocation::Register(0));
    context.push("mov r1 r0");
    context.push("pop #64 r0");
    context.push("cmp r0 r1");
    match op {
        BinaryComparisonOp::Equals => context.push("setz r0"),
        BinaryComparisonOp::NotEquals => context.push("setnz r0"),
        BinaryComparisonOp::Less => context.push("setl r0"),
        BinaryComparisonOp::LessEquals => context.push("setle r0"),
        BinaryComparisonOp::Greater => context.push("setg r0"),
        BinaryComparisonOp::GreaterEquals => context.push("setge r0"),
    }
    move_register_to_location(context, 0, byte_size * 8, result_location);
}

fn generate_assignment_binop(
    context: &mut Context,
    byte_size: usize,
    left: TypedAnalyzedAddressableExpression,
    right: TypedAnalyzedExpression,
    result_location: ExprResultLocation,
) {
    let value_location = match right.ty {
        Type::Struct { .. } => ExprResultLocation::Stack,
        _ if byte_size > 0 => ExprResultLocation::Register(0),
        _ => ExprResultLocation::Discard,
    };

    match (value_location, result_location) {
        (ExprResultLocation::Register(val_reg), ExprResultLocation::Register(reg)) => {
            generate_expression_code(context, right, ExprResultLocation::Register(val_reg));

            context.push(&format!("push #{} r{}", byte_size, val_reg));
            store_reg0_in_var(context, left);
            context.push(&format!("pop #{} r{}", byte_size, reg));
        }
        (ExprResultLocation::Register(val_reg), ExprResultLocation::Stack) => {
            generate_expression_code(context, right, ExprResultLocation::Register(val_reg));

            context.push(&format!("push #{} r{}", byte_size, val_reg));
            store_reg0_in_var(context, left);
        }
        (ExprResultLocation::Stack, ExprResultLocation::Stack) => {
            generate_expression_code(context, right, ExprResultLocation::Stack);
            store_stack_in_var(context, left);
        }
        (ExprResultLocation::Discard, ExprResultLocation::Discard) => {
            generate_expression_code(context, right, ExprResultLocation::Discard);
        }
        (ExprResultLocation::Register(val_reg), ExprResultLocation::Discard) => {
            generate_expression_code(context, right, ExprResultLocation::Register(val_reg));
            store_reg0_in_var(context, left);
        }
        (ExprResultLocation::Stack, ExprResultLocation::Discard) => {
            generate_expression_code(context, right, ExprResultLocation::Stack);
            store_stack_in_var(context, left);
            context.push(&format!("addi sp {}", byte_size));
        }
        (a, b) => panic!("Invalid assignment result location: {:?} {:?} {:?}", a, b, right),
    }
}

fn generate_math_assignment_binop(
    context: &mut Context,
    op: BinaryMathOp,
    byte_size: usize,
    left: TypedAnalyzedAddressableExpression,
    right: TypedAnalyzedExpression,
    result_location: ExprResultLocation,
) {
    generate_expression_code(context, right, ExprResultLocation::Register(0));

    context.push("push #64 r0");
    load_var(context, left.clone(), ExprResultLocation::Register(0));
    context.push("pop #64 r1");
    add_math_op_instruction(context, op);
    if byte_size < 8 {
        context.push(&format!("signext #{} r0", byte_size * 8));
    }

    context.push("push #64 r0");
    store_reg0_in_var(context, left);
    context.push("pop #64 r0");

    move_register_to_location(context, 0, byte_size * 8, result_location);
}

fn generate_logic_assignment_binop(
    context: &mut Context,
    op: BinaryLogicOp,
    byte_size: usize,
    left: TypedAnalyzedAddressableExpression,
    right: TypedAnalyzedExpression,
    result_location: ExprResultLocation,
) {
    let end_label = context.get_new_label();
    load_var(context, left.clone(), ExprResultLocation::Register(0));
    context.push("cmpi r0 0");
    match op {
        BinaryLogicOp::Or => context.push(&format!("jnz {}", end_label)),
        BinaryLogicOp::And => context.push(&format!("jz {}", end_label)),
    }
    generate_expression_code(context, right, ExprResultLocation::Register(0));
    context.push_label(end_label);
    context.push("push #64 r0");
    store_reg0_in_var(context, left);
    context.push("pop #64 r0");

    move_register_to_location(context, 0, byte_size * 8, result_location);
}

fn add_math_op_instruction(context: &mut Context, op: BinaryMathOp) {
    match op {
        BinaryMathOp::Add => context.push("add r0 r1"),
        BinaryMathOp::Sub => context.push("sub r0 r1"),
        BinaryMathOp::Mul => context.push("mul r0 r1"),
        BinaryMathOp::Div => context.push("div r0 r1"),
        BinaryMathOp::Mod => context.push("mod r0 r1"),
        BinaryMathOp::And => context.push("and r0 r1"),
        BinaryMathOp::Or => context.push("or r0 r1"),
        BinaryMathOp::Xor => context.push("xor r0 r1"),
        BinaryMathOp::Shl => context.push("shl r0 r1"),
        BinaryMathOp::Shr => context.push("shr r0 r1"),
    }
}

fn load_var_address(context: &mut Context, expr: TypedAnalyzedAddressableExpression, result_location: ExprResultLocation) {
    match expr.value {
        AnalyzedAddressableExpression::Variable(name) => {
            let offset = context.current_data.local_variables[&name].offset;
            match result_location {
                ExprResultLocation::Register(reg) => {
                    context.push(&format!("lea r{} [bp;{}]", reg, offset));
                }
                ExprResultLocation::Stack => {
                    context.push(&format!("lea r0 [bp;{}]", offset));
                    context.push("push #64 r0");
                }
                ExprResultLocation::Discard => {}
            }
        }
        AnalyzedAddressableExpression::MemberAccess { expr, member, struct_name } => {
            let byte_size = expr.ty.size();
            load_var_address(context, *expr, ExprResultLocation::Register(0));
            let offset = context.struct_data[&struct_name].fields[&member].offset;
            context.push(&format!("addi r0 {}", offset));
            move_register_to_location(context, 0, byte_size * 8, result_location);
        }
        AnalyzedAddressableExpression::Dereference(inner_expr) => {
            generate_expression_code(context, *inner_expr, ExprResultLocation::Register(0));
            move_register_to_location(context, 0, 64, result_location);
        }
    }
}

fn load_var(context: &mut Context, expr: TypedAnalyzedAddressableExpression, result_location: ExprResultLocation) {
    let byte_size = expr.ty.size();
    match expr.value {
        AnalyzedAddressableExpression::Variable(name) => {
            let var_data = &context.current_data.local_variables.get(&name).unwrap_or_else(|| {
                panic!("Variable {} not found", name)
            });
            let offset = var_data.offset;

            match result_location {
                ExprResultLocation::Register(reg) => {
                    context.push(&format!("load #{} r{} [bp;{}]", byte_size * 8, reg, offset));
                }
                ExprResultLocation::Stack => {
                    context.push(&format!("movi r0 {}", byte_size));
                    context.push(&format!("pushmem r0 [bp;{}]", offset));
                }
                ExprResultLocation::Discard => {}
            }
        }
        AnalyzedAddressableExpression::MemberAccess { expr, member, struct_name } => {
            load_var_address(context, *expr, ExprResultLocation::Register(0));
            let var_data = &context.struct_data[&struct_name].fields[&member];
            let offset = var_data.offset;

            match result_location {
                ExprResultLocation::Register(reg) => {
                    context.push(&format!("load #{} r{} [r0;{}]", byte_size * 8, reg, offset));
                }
                ExprResultLocation::Stack => {
                    context.push(&format!("movi r1 {}", byte_size));
                    context.push(&format!("pushmem r1 [r0;{}]", offset));
                }
                ExprResultLocation::Discard => {}
            }
        }
        AnalyzedAddressableExpression::Dereference(inner_expr) => {
            generate_expression_code(context, *inner_expr, ExprResultLocation::Register(0));
            match result_location {
                ExprResultLocation::Register(reg) => {
                    context.push(&format!("load #{} r{} [r0]", byte_size * 8, reg));
                }
                ExprResultLocation::Stack => {
                    context.push(&format!("movi r1 {}", byte_size));
                    context.push("pushmem r1 [r0]");
                }
                ExprResultLocation::Discard => {}
            }
        }
    }
}

fn store_reg0_in_var(context: &mut Context, expr: TypedAnalyzedAddressableExpression) {
    let byte_size = expr.ty.size();
    match expr.value {
        AnalyzedAddressableExpression::Variable(name) => {
            let var_data = &context.current_data.local_variables[&name];
            let offset = var_data.offset;
            context.push(&format!("store #{} r0 [bp;{}]", byte_size * 8, offset));
        }
        AnalyzedAddressableExpression::MemberAccess { expr, member, struct_name } => {
            let var_data = &context.struct_data[&struct_name].fields[&member];
            let offset = var_data.offset;

            context.push("push #64 r0");
            load_var_address(context, *expr, ExprResultLocation::Register(0));
            context.push("pop #64 r1");
            context.push(&format!("store #{} r1 [r0;{}]", byte_size * 8, offset));
        }
        AnalyzedAddressableExpression::Dereference(inner_expr) => {
            context.push("push #64 r0");
            generate_expression_code(context, *inner_expr, ExprResultLocation::Register(0));
            context.push("pop #64 r1");
            context.push(&format!("store #{} r1 [r0]", byte_size * 8));
        }
    }
}

fn store_stack_in_var(context: &mut Context, expr: TypedAnalyzedAddressableExpression) {
    let byte_size = expr.ty.size();
    match expr.value {
        AnalyzedAddressableExpression::Variable(name) => {
            let var_data = &context.current_data.local_variables[&name];
            let offset = var_data.offset;
            context.push(&format!("movi r0 {}", byte_size));
            context.push(&format!("peekmem r0 [bp;{}]", offset));
        }
        AnalyzedAddressableExpression::MemberAccess { expr, member, struct_name } => {
            let var_data = &context.struct_data[&struct_name].fields[&member];
            let offset = var_data.offset;

            load_var_address(context, *expr, ExprResultLocation::Register(0));
            context.push(&format!("movi r1 {}", byte_size));
            context.push(&format!("peekmem r1 [r0;{}]", offset));
        }
        AnalyzedAddressableExpression::Dereference(inner_expr) => {
            generate_expression_code(context, *inner_expr, ExprResultLocation::Register(0));
            context.push(&format!("movi r1 {}", byte_size));
            context.push("peekmem r1 [r0]");
        }
    }
}

fn move_register_to_location(context: &mut Context, register: usize, size: usize, result_location: ExprResultLocation) {
    match result_location {
        ExprResultLocation::Register(reg) => {
            if reg != register {
                context.push(&format!("mov r{} r{}", reg, register));
            }
        }
        ExprResultLocation::Stack => {
            context.push(&format!("push #{} r0", size));
        }
        ExprResultLocation::Discard => {}
    }
}