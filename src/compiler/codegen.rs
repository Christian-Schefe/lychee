use std::collections::HashMap;
use std::path::PathBuf;
use crate::compiler::parser::analyzed_syntax_tree::{AnalyzedFunction, AnalyzedProgram};
use crate::compiler::parser::syntax_tree::{BinaryOp, Expression, Literal, SrcExpression, SrcStatement, Statement, Type, UnaryOp};

struct Context {
    output: Vec<String>,
    function_labels: HashMap<String, String>,
    function_arg_sizes: HashMap<String, Vec<usize>>,
    fn_context: FunctionContext,
    label_counter: usize,
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

struct FunctionContext {
    name: String,
    var_stack_offsets: HashMap<String, isize>,
    var_type_sizes: HashMap<String, usize>,
    current_stack_offset: isize,
    return_label: String,
}

pub fn generate_code(program: AnalyzedProgram, output: &PathBuf) {
    let mut context = Context {
        output: Vec::new(),
        function_labels: HashMap::new(),
        function_arg_sizes: HashMap::new(),
        fn_context: FunctionContext {
            name: String::new(),
            var_stack_offsets: HashMap::new(),
            var_type_sizes: HashMap::new(),
            current_stack_offset: 0,
            return_label: String::new(),
        },
        label_counter: 0,
    };
    for (name, func) in &program.functions {
        context.function_labels.insert(name.clone(), format!("_{}", name));
        let mut arg_sizes = Vec::new();
        for (_, ty) in &func.args {
            arg_sizes.push(ty.size());
        }
        context.function_arg_sizes.insert(name.clone(), arg_sizes);
    }

    context.push(&format!("call {}", context.function_labels[&program.main_function]));
    if program.functions[&program.main_function].return_type.is_none() {
        context.push("set r0 0");
    }
    context.push("exit");

    for (_, function) in program.functions {
        generate_function_code(&mut context, function)
    }

    let result = context.output.join("\n");
    std::fs::write(output, result).unwrap();
}

fn generate_function_code(context: &mut Context, function: AnalyzedFunction) {
    context.fn_context = FunctionContext {
        return_label: format!("_{}_return", function.name),
        name: function.name,
        var_stack_offsets: HashMap::new(),
        var_type_sizes: HashMap::new(),
        current_stack_offset: -8,
    };
    context.push_label(context.function_labels[&context.fn_context.name].clone());
    context.push("push #64 bp");
    context.push("mov bp sp");

    let mut argument_offset = 16;
    for (name, ty) in function.args.iter().rev() {
        let offset = argument_offset;
        context.fn_context.var_stack_offsets.insert(name.clone(), offset);
        context.fn_context.var_type_sizes.insert(name.clone(), ty.size());
        argument_offset += ty.size() as isize;
    }

    context.push(&format!("set r0 {}", function.local_var_stack_size + 8));
    context.push("sub sp r0");

    generate_expression_code(context, function.expr);

    context.push_label(context.fn_context.return_label.clone());
    context.push("mov sp bp");
    context.push("pop #64 bp");
    context.push("ret");
}

fn generate_statement_code(context: &mut Context, statement: SrcStatement) {
    match statement.statement {
        Statement::Return(expr) => {
            if let Some(return_expr) = expr {
                generate_expression_code(context, return_expr);
            }
            context.push(&format!("jmp {}", context.fn_context.return_label));
        }
        Statement::Declaration { var_type, name, value } => {
            let offset = context.fn_context.current_stack_offset;
            context.fn_context.current_stack_offset -= var_type.size() as isize;
            let size = var_type.size() * 8;
            context.fn_context.var_stack_offsets.insert(name.clone(), offset);
            context.fn_context.var_type_sizes.insert(name.clone(), var_type.size());

            if let Some(value) = value {
                generate_expression_code(context, value);
                context.push(&format!("store #{} r0 [bp;{}]", size, offset));
            }
        }
        Statement::Expr(expr) => {
            generate_expression_code(context, expr);
        }
        Statement::If { condition, true_expr, false_expr } => {
            let false_label = context.get_new_label();
            let end_label = context.get_new_label();
            generate_expression_code(context, condition);
            context.push("set r1 0");
            context.push("cmp r0 r1");
            context.push(&format!("jz {}", false_label));
            generate_expression_code(context, true_expr);
            context.push(&format!("jmp {}", end_label));
            context.push_label(false_label);
            if let Some(false_expr) = false_expr {
                generate_expression_code(context, false_expr);
            }
            context.push_label(end_label);
        }
    }
}

fn generate_expression_code(context: &mut Context, expr: SrcExpression) {
    match expr.expr {
        Expression::Literal(literal) => {
            match literal {
                Literal::Int(int) => {
                    context.push(&format!("set r0 {}", int));
                }
                Literal::Long(long) => {
                    context.push(&format!("set r0 {}", long));
                }
                Literal::Bool(boolean) => {
                    context.push(&format!("set r0 {}", if boolean { 1 } else { 0 }));
                }
                _ => unimplemented!()
            }
        }
        Expression::Variable(name) => {
            let offset = context.fn_context.var_stack_offsets[&name];
            let size = context.fn_context.var_type_sizes[&name] * 8;
            context.push(&format!("load #{} r0 [bp;{}]", size, offset));
        }
        Expression::FunctionCall { function, args } => {
            let mut total_size = 0;
            for (i, arg) in args.into_iter().enumerate() {
                let arg_bytes = context.function_arg_sizes[&function][i];
                generate_expression_code(context, arg);
                context.push(&format!("push #{} r0", arg_bytes * 8));
                total_size += arg_bytes;
            }
            context.push(&format!("call {}", context.function_labels[&function]));
            context.push(&format!("set r1 {}", total_size));
            context.push("add sp r1");
        }
        Expression::Block(statements, final_expr) => {
            for statement in statements {
                generate_statement_code(context, statement);
            }
            if let Some(final_expr) = final_expr {
                generate_expression_code(context, *final_expr);
            }
        }
        Expression::Unary { op, expr } => {
            generate_expression_code(context, *expr);
            match op {
                UnaryOp::Negate => {
                    context.push("neg r0");
                }
                UnaryOp::Positive => {}
                UnaryOp::Not => {
                    context.push("not r0");
                }
                UnaryOp::LogicalNot => {
                    context.push("set r1 0");
                    context.push("cmp r0 r1");
                    context.push("setz r0");
                }
            }
        }
        Expression::Ternary { condition, true_expr, false_expr } => {
            let false_label = context.get_new_label();
            let end_label = context.get_new_label();
            generate_expression_code(context, *condition);
            context.push("set r1 0");
            context.push("cmp r0 r1");
            context.push(&format!("jz {}", false_label));
            generate_expression_code(context, *true_expr);
            context.push(&format!("jmp {}", end_label));
            context.push_label(false_label);
            generate_expression_code(context, *false_expr);
            context.push_label(end_label);
        }
        Expression::Binary { op, left, right } => {
            match op {
                BinaryOp::LogicalOr | BinaryOp::LogicalAnd => generate_logic_binop(context, op, *left, *right),
                BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::And | BinaryOp::Or | BinaryOp::Xor | BinaryOp::Mod | BinaryOp::Shl | BinaryOp::Shr => generate_math_binop(context, op, *left, *right),
                BinaryOp::Less | BinaryOp::LessEquals | BinaryOp::Greater | BinaryOp::GreaterEquals | BinaryOp::Equals | BinaryOp::NotEquals => generate_relational_binop(context, op, *left, *right),
                BinaryOp::Assign | BinaryOp::AddAssign | BinaryOp::SubAssign | BinaryOp::MulAssign | BinaryOp::DivAssign | BinaryOp::AndAssign | BinaryOp::OrAssign | BinaryOp::XorAssign | BinaryOp::ModAssign | BinaryOp::ShlAssign | BinaryOp::ShrAssign => {
                    generate_assignment_binop(context, op, *left, *right);
                }
            }
        }
        Expression::Cast { var_type, expr } => {
            generate_expression_code(context, *expr);
            let bit_size = var_type.size() * 8;
            if var_type == Type::Bool {
                context.push("set r1 0");
                context.push("cmp r0 r1");
                context.push("setnz r0");
            } else if bit_size < 64 {
                let mask = (1u64 << bit_size) - 1;
                context.push(&format!("set r1 {}", mask));
                context.push("and r0 r1");
            }
        }
    }
}

fn generate_logic_binop(context: &mut Context, op: BinaryOp, left: SrcExpression, right: SrcExpression) {
    let false_label = context.get_new_label();
    let end_label = context.get_new_label();
    generate_expression_code(context, left);
    context.push("set r1 0");
    context.push("cmp r0 r1");
    match op {
        BinaryOp::LogicalOr => {
            context.push(&format!("jnz {}", end_label));
        }
        BinaryOp::LogicalAnd => {
            context.push(&format!("jz {}", false_label));
        }
        _ => unreachable!()
    }
    generate_expression_code(context, right);
    context.push(&format!("jmp {}", end_label));
    context.push_label(false_label);
    context.push("set r0 0");
    context.push_label(end_label);
}

fn generate_math_binop(context: &mut Context, op: BinaryOp, left: SrcExpression, right: SrcExpression) {
    generate_expression_code(context, left);
    context.push("push #64 r0");
    generate_expression_code(context, right);
    context.push("mov r1 r0");
    context.push("pop #64 r0");
    match op {
        BinaryOp::Add => context.push("add r0 r1"),
        BinaryOp::Sub => context.push("sub r0 r1"),
        BinaryOp::Mul => context.push("mul r0 r1"),
        BinaryOp::Div => context.push("div r0 r1"),
        BinaryOp::Mod => context.push("mod r0 r1"),
        BinaryOp::And => context.push("and r0 r1"),
        BinaryOp::Or => context.push("or r0 r1"),
        BinaryOp::Xor => context.push("xor r0 r1"),
        BinaryOp::Shl => context.push("shl r0 r1"),
        BinaryOp::Shr => context.push("shr r0 r1"),
        _ => unreachable!()
    }
}

fn generate_relational_binop(context: &mut Context, op: BinaryOp, left: SrcExpression, right: SrcExpression) {
    generate_expression_code(context, left);
    context.push("push #64 r0");
    generate_expression_code(context, right);
    context.push("mov r1 r0");
    context.push("pop #64 r0");
    context.push("cmp r0 r1");
    match op {
        BinaryOp::Less => context.push("setl r0"),
        BinaryOp::LessEquals => context.push("setle r0"),
        BinaryOp::Greater => context.push("setg r0"),
        BinaryOp::GreaterEquals => context.push("setge r0"),
        BinaryOp::Equals => context.push("setz r0"),
        BinaryOp::NotEquals => context.push("setnz r0"),
        _ => unreachable!()
    }
}

fn generate_assignment_binop(context: &mut Context, op: BinaryOp, left: SrcExpression, right: SrcExpression) {
    generate_expression_code(context, right);
    let name = match left.expr {
        Expression::Variable(name) => name,
        _ => unreachable!()
    };
    let offset = context.fn_context.var_stack_offsets[&name];
    let size = context.fn_context.var_type_sizes[&name] * 8;
    if op != BinaryOp::Assign {
        context.push("mov r1 r0");
        context.push(&format!("load #{} r0 [bp;{}]", size, offset));
        match op {
            BinaryOp::AddAssign => context.push("add r0 r1"),
            BinaryOp::SubAssign => context.push("sub r0 r1"),
            BinaryOp::MulAssign => context.push("mul r0 r1"),
            BinaryOp::DivAssign => context.push("div r0 r1"),
            BinaryOp::ModAssign => context.push("mod r0 r1"),
            BinaryOp::AndAssign => context.push("and r0 r1"),
            BinaryOp::OrAssign => context.push("or r0 r1"),
            BinaryOp::XorAssign => context.push("xor r0 r1"),
            BinaryOp::ShlAssign => context.push("shl r0 r1"),
            BinaryOp::ShrAssign => context.push("shr r0 r1"),
            _ => unreachable!()
        }
    }
    context.push(&format!("store #{} r0 [bp;{}]", size, offset));
}