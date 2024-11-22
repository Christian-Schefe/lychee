mod builtin_functions;

use crate::compiler::codegen::builtin_functions::add_builtin_fn_code;
use crate::compiler::parser::analyzed_syntax_tree::{AnalyzedExpression, AnalyzedFunction, AnalyzedProgram, AnalyzedStatement, TypedAnalyzedExpression};
use crate::compiler::parser::syntax_tree::{BinaryComparisonOp, BinaryLogicOp, BinaryMathOp, BinaryOp, Literal, UnaryOp};
use crate::compiler::parser::types::Type;
use std::collections::HashMap;
use std::path::PathBuf;

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
        context
            .function_labels
            .insert(name.clone(), format!("_func_{}", name));
        let mut arg_sizes = Vec::new();
        for (_, ty) in &func.args {
            arg_sizes.push(ty.size());
        }
        context.function_arg_sizes.insert(name.clone(), arg_sizes);
    }

    context.push(&format!(
        "call {}",
        context.function_labels[&program.main_function]
    ));

    if program.functions[&program.main_function].return_type == Type::Unit
    {
        context.push("movi r0 0");
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
    context.fn_context = FunctionContext {
        return_label: format!("_return_{}", function.name),
        name: function.name,
        var_stack_offsets: HashMap::new(),
        var_type_sizes: HashMap::new(),
        current_stack_offset: 0,
    };
    context.push_label(context.function_labels[&context.fn_context.name].clone());
    context.push("push #64 bp");
    context.push("mov bp sp");

    let mut argument_offset = 16;
    for (name, ty) in function.args.iter().rev() {
        let offset = argument_offset;
        context
            .fn_context
            .var_stack_offsets
            .insert(name.clone(), offset);
        context
            .fn_context
            .var_type_sizes
            .insert(name.clone(), ty.size());
        argument_offset += ty.size() as isize;
    }

    if function.local_var_stack_size > 0 {
        context.push(&format!("subi sp {}", function.local_var_stack_size));
    }

    generate_expression_code(context, function.expr);

    context.push_label(context.fn_context.return_label.clone());
    context.push("mov sp bp");
    context.push("pop #64 bp");
    context.push("ret");
}

fn generate_statement_code(context: &mut Context, statement: AnalyzedStatement) {
    match statement {
        AnalyzedStatement::Return(expr) => {
            if let Some(return_expr) = expr {
                generate_expression_code(context, return_expr);
            }
            context.push(&format!("jmp {}", context.fn_context.return_label));
        }
        AnalyzedStatement::Declaration {
            var_type,
            name,
            value,
        } => {
            let size = var_type.size();
            context.fn_context.current_stack_offset -= size as isize;
            let offset = context.fn_context.current_stack_offset;
            context.fn_context.var_stack_offsets.insert(name.clone(), offset);
            context.fn_context.var_type_sizes.insert(name.clone(), size);

            generate_expression_code(context, value);
            if size > 0 {
                context.push(&format!("store #{} r0 [bp;{}]", size * 8, offset));
            }
            println!("{}: {}, size {} bytes  {}", name, offset, size, context.fn_context.current_stack_offset);
        }
        AnalyzedStatement::Expr(expr) => {
            generate_expression_code(context, expr);
        }
        AnalyzedStatement::If {
            condition,
            true_expr,
            false_statement,
        } => {
            let false_label = context.get_new_label();
            let end_label = context.get_new_label();
            generate_expression_code(context, condition);
            context.push("cmpi r0 0");
            context.push(&format!("jz {}", false_label));
            generate_expression_code(context, true_expr);
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
            generate_expression_code(context, condition);
            context.push("cmpi r0 0");
            context.push(&format!("jz {}", end_label));
            generate_expression_code(context, body);
            generate_expression_code(context, update);
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
                generate_expression_code(context, condition);
                context.push("cmpi r0 0");
                context.push(&format!("jz {}", end_label));
                generate_expression_code(context, body);
                context.push(&format!("jmp {}", loop_label));
                context.push_label(end_label);
            } else {
                generate_expression_code(context, body);
                generate_expression_code(context, condition);
                context.push("cmpi r0 0");
                context.push(&format!("jnz {}", loop_label));
            }
        }
    }
}

fn generate_expression_code(context: &mut Context, expr: TypedAnalyzedExpression) {
    match expr.expr {
        AnalyzedExpression::Literal(literal) => match literal {
            Literal::Integer(integer) => {
                context.push(&format!("movi r0 {}", integer));
            }
            Literal::Bool(boolean) => {
                context.push(&format!("movi r0 {}", if boolean { 1 } else { 0 }));
            }
            Literal::Char(char) => {
                context.push(&format!("movi r0 {}", char));
            }
            Literal::Unit => {}
        },
        AnalyzedExpression::Variable(name) => {
            let offset = context.fn_context.var_stack_offsets[&name];
            let size = context.fn_context.var_type_sizes[&name] * 8;
            if size > 0 {
                context.push(&format!("load #{} r0 [bp;{}]", size, offset));
            }
        }
        AnalyzedExpression::FunctionCall { function, args } => {
            let mut total_size = 0;
            for (i, arg) in args.into_iter().enumerate() {
                let arg_bytes = context.function_arg_sizes[&function][i];
                generate_expression_code(context, arg);
                if arg_bytes > 0 {
                    context.push(&format!("push #{} r0", arg_bytes * 8));
                    total_size += arg_bytes;
                }
            }
            context.push(&format!("call {}", context.function_labels[&function]));
            context.push(&format!("addi sp {}", total_size));
        }
        AnalyzedExpression::Block(statements, final_expr) => {
            for statement in statements {
                generate_statement_code(context, statement);
            }
            if let Some(final_expr) = final_expr {
                generate_expression_code(context, *final_expr);
            }
        }
        AnalyzedExpression::Unary { op, expr: inner_expr } => {
            match op {
                UnaryOp::Negate => {
                    generate_expression_code(context, *inner_expr);
                    context.push("neg r0");
                }
                UnaryOp::Positive => {
                    generate_expression_code(context, *inner_expr);
                }
                UnaryOp::Not => {
                    generate_expression_code(context, *inner_expr);
                    context.push("not r0");
                }
                UnaryOp::LogicalNot => {
                    generate_expression_code(context, *inner_expr);
                    context.push("cmpi r0 0");
                    context.push("setz r0");
                }
                UnaryOp::Increment | UnaryOp::Decrement => {
                    generate_expression_code(context, *inner_expr.clone());
                    if op == UnaryOp::Increment {
                        context.push("addi r0 1");
                    } else {
                        context.push("subi r0 1");
                    }
                    store_var(context, *inner_expr);
                }
                UnaryOp::Borrow => {
                    load_var_address(context, inner_expr.expr);
                }
                UnaryOp::Deref => {
                    let size = expr.expr_type.size() * 8;
                    generate_expression_code(context, *inner_expr);
                    context.push(&format!("load #{} r0 [r0]", size));
                }
            }
        }
        AnalyzedExpression::Ternary {
            condition,
            true_expr,
            false_expr,
        } => {
            let false_label = context.get_new_label();
            let end_label = context.get_new_label();
            generate_expression_code(context, *condition);
            context.push("cmpi r0 0");
            context.push(&format!("jz {}", false_label));
            generate_expression_code(context, *true_expr);
            context.push(&format!("jmp {}", end_label));
            context.push_label(false_label);
            generate_expression_code(context, *false_expr);
            context.push_label(end_label);
        }
        AnalyzedExpression::Binary { op, left, right } => match op {
            BinaryOp::Logical(logic_op) => generate_logic_binop(context, logic_op, *left, *right),
            BinaryOp::Math(math_op) => generate_math_binop(context, math_op, *left, *right),
            BinaryOp::Comparison(comp_op) => generate_comparison_binop(context, comp_op, *left, *right),
            BinaryOp::Assign => generate_assignment_binop(context, None, *left, *right),
            BinaryOp::MathAssign(math_op) => generate_assignment_binop(context, Some(math_op), *left, *right),
            BinaryOp::LogicAssign(logic_op) => generate_logic_assignment_binop(context, logic_op, *left, *right),
        },
        AnalyzedExpression::Cast { var_type, expr } => {
            generate_expression_code(context, *expr);
            let bit_size = var_type.size() * 8;
            if var_type == Type::Bool {
                context.push("cmpi r0 0");
                context.push("setnz r0");
            } else if bit_size < 64 {
                context.push(&format!("signext #{} r0", bit_size));
            }
        }
    }
}

fn generate_logic_binop(
    context: &mut Context,
    op: BinaryLogicOp,
    left: TypedAnalyzedExpression,
    right: TypedAnalyzedExpression,
) {
    let end_label = context.get_new_label();
    generate_expression_code(context, left);
    context.push("cmpi r0 0");
    match op {
        BinaryLogicOp::Or => context.push(&format!("jnz {}", end_label)),
        BinaryLogicOp::And => context.push(&format!("jz {}", end_label)),
    }
    generate_expression_code(context, right);
    context.push_label(end_label);
}

fn generate_math_binop(
    context: &mut Context,
    op: BinaryMathOp,
    left: TypedAnalyzedExpression,
    right: TypedAnalyzedExpression,
) {
    generate_expression_code(context, left);
    context.push("push #64 r0");
    generate_expression_code(context, right);
    context.push("mov r1 r0");
    context.push("pop #64 r0");
    add_math_op_instruction(context, op);
}

fn generate_comparison_binop(
    context: &mut Context,
    op: BinaryComparisonOp,
    left: TypedAnalyzedExpression,
    right: TypedAnalyzedExpression,
) {
    generate_expression_code(context, left);
    context.push("push #64 r0");
    generate_expression_code(context, right);
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
}

fn generate_assignment_binop(
    context: &mut Context,
    op: Option<BinaryMathOp>,
    left: TypedAnalyzedExpression,
    right: TypedAnalyzedExpression,
) {
    generate_expression_code(context, right);
    if let Some(math_op) = op {
        context.push("push #64 r0");
        generate_expression_code(context, left.clone());
        context.push("pop #64 r1");
        add_math_op_instruction(context, math_op);
    }
    store_var(context, left);
}

fn generate_logic_assignment_binop(
    context: &mut Context,
    op: BinaryLogicOp,
    left: TypedAnalyzedExpression,
    right: TypedAnalyzedExpression,
) {
    let end_label = context.get_new_label();
    generate_expression_code(context, left.clone());
    context.push("cmpi r0 0");
    match op {
        BinaryLogicOp::Or => context.push(&format!("jnz {}", end_label)),
        BinaryLogicOp::And => context.push(&format!("jz {}", end_label)),
    }
    generate_expression_code(context, right);
    context.push_label(end_label);
    store_var(context, left);
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

fn load_var_address(context: &mut Context, expr: AnalyzedExpression) {
    match expr {
        AnalyzedExpression::Variable(name) => {
            let offset = context.fn_context.var_stack_offsets[&name];
            context.push(&format!("lea r0 [bp;{}]", offset));
        }
        _ => unreachable!(),
    }
}

fn store_var(context: &mut Context, expr: TypedAnalyzedExpression) {
    match expr.expr {
        AnalyzedExpression::Variable(name) => {
            let offset = context.fn_context.var_stack_offsets[&name];
            let size = context.fn_context.var_type_sizes[&name] * 8;
            context.push(&format!("store #{} r0 [bp;{}]", size, offset));
        }
        AnalyzedExpression::Unary { op: UnaryOp::Deref, expr: inner_expr } => {
            let size = expr.expr_type.size() * 8;
            context.push("push #64 r0");
            generate_expression_code(context, *inner_expr);
            context.push("pop #64 r1");
            context.push(&format!("store #{} r1 [r0]", size));
        }
        _ => unreachable!(),
    }
}