use crate::compiler::parser::expression_tree_printer::Printer;
use crate::compiler::resolver::resolved_expression::{
    ResolvedAssignableExpression, ResolvedExpression, ResolvedExpressionKind, ResolvedFunction,
    ResolvedLiteral, ResolvedProgram,
};

pub fn print_program(expr: &ResolvedProgram) {
    let mut printer = Printer::new();
    for function in &expr.functions {
        print_function(&mut printer, &function);
    }
    println!("{}", printer.build());
}

fn print_function(printer: &mut Printer, function: &ResolvedFunction) {
    printer.add_line(format!(
        "fn {} (stack: {}, return location: {:?})",
        function.name, function.local_var_stack_size, function.value_location
    ));
    printer.indent();
    print_expression(printer, &function.body);
    printer.dedent();
}

fn print_expression(printer: &mut Printer, expr: &ResolvedExpression) {
    printer.add_line(format!(
        "Expr: {:?}, stack discard: {}",
        expr.value_data, expr.stack_discard
    ));
    match &expr.kind {
        ResolvedExpressionKind::Literal(literal) => match literal {
            ResolvedLiteral::Struct(fields) => {
                printer.add_line("StructLiteral".to_string());
                printer.indent();
                for field_expr in fields {
                    printer.indent();
                    print_expression(printer, field_expr);
                    printer.dedent();
                }
                printer.dedent();
            }
            ResolvedLiteral::Unit => printer.add_line("Unit".to_string()),
            ResolvedLiteral::Bool(b) => printer.add_line(format!("Bool({})", b)),
            ResolvedLiteral::Char(c) => printer.add_line(format!("Char({})", c)),
            ResolvedLiteral::Integer(i) => printer.add_line(format!("Int({})", i)),
        },
        ResolvedExpressionKind::Block(expressions) => {
            printer.add_line("{".to_string());
            printer.indent();
            for expression in expressions {
                print_expression(printer, expression);
            }
            printer.dedent();
            printer.add_line("}".to_string());
        }
        ResolvedExpressionKind::Return(expr) => {
            printer.add_line("Return".to_string());
            if let Some(expr) = expr {
                printer.indent();
                print_expression(printer, expr);
                printer.dedent();
            }
        }
        ResolvedExpressionKind::Continue => {
            printer.add_line("Continue".to_string());
        }
        ResolvedExpressionKind::Break { maybe_expr } => {
            printer.add_line("Break".to_string());
            if let Some(expr) = maybe_expr {
                printer.indent();
                print_expression(printer, expr);
                printer.dedent();
            }
        }
        ResolvedExpressionKind::If {
            condition,
            then_block,
            else_expr,
        } => {
            printer.add_line("If".to_string());
            printer.indent();
            print_expression(printer, condition);
            print_expression(printer, then_block);
            if let Some(else_expr) = else_expr {
                print_expression(printer, else_expr);
            }
            printer.dedent();
        }
        ResolvedExpressionKind::Loop {
            init,
            condition,
            step,
            loop_body,
            else_expr,
        } => {
            printer.add_line("Loop".to_string());
            printer.indent();
            if let Some(init) = init {
                print_expression(printer, init);
            }
            if let Some(condition) = condition {
                print_expression(printer, condition);
            }
            if let Some(step) = step {
                print_expression(printer, step);
            }
            print_expression(printer, loop_body);
            if let Some(else_expr) = else_expr {
                print_expression(printer, else_expr);
            }
            printer.dedent();
        }
        ResolvedExpressionKind::Declaration { var_offset, value } => {
            printer.add_line(format!("Declaration (offset: {})", var_offset));
            printer.indent();
            print_expression(printer, value);
            printer.dedent();
        }
        ResolvedExpressionKind::ValueOfAssignable(offset) => {
            printer.add_line("ValueOfAssignable".to_string());
            printer.indent();
            print_assignable_expression(printer, offset);
            printer.dedent();
        }
        ResolvedExpressionKind::Unary { op, expr } => {
            printer.add_line(format!("Unary {:?}", op));
            printer.indent();
            print_expression(printer, expr);
            printer.dedent();
        }
        ResolvedExpressionKind::Binary { op, left, right } => {
            printer.add_line(format!("Binary {:?}", op));
            printer.indent();
            print_expression(printer, left);
            print_expression(printer, right);
            printer.dedent();
        }
        ResolvedExpressionKind::Assign { op, lhs, rhs } => {
            printer.add_line(format!("Assign {:?}", op));
            printer.indent();
            print_assignable_expression(printer, lhs);
            print_expression(printer, rhs);
            printer.dedent();
        }
        ResolvedExpressionKind::Borrow { expr } => {
            printer.add_line("Borrow".to_string());
            printer.indent();
            print_assignable_expression(printer, expr);
            printer.dedent();
        }
        ResolvedExpressionKind::FunctionCall {
            function_name,
            args,
            return_stack_space,
        } => {
            printer.add_line(format!(
                "FunctionCall {} (return on stack: {})",
                function_name, return_stack_space
            ));
            printer.indent();
            for arg in args {
                print_expression(printer, arg);
            }
            printer.dedent();
        }
        ResolvedExpressionKind::FieldAccess {
            expr,
            field_offset,
            struct_size,
        } => {
            printer.add_line(format!(
                "FieldAccess (offset: {}, struct size: {})",
                field_offset, struct_size
            ));
            printer.indent();
            print_expression(printer, expr);
            printer.dedent();
        }
        ResolvedExpressionKind::Increment(expr, is_prefix) => {
            printer.add_line(format!("Increment (prefix: {})", is_prefix));
            printer.indent();
            print_assignable_expression(printer, expr);
            printer.dedent();
        }
        ResolvedExpressionKind::Decrement(expr, is_prefix) => {
            printer.add_line(format!("Decrement (prefix: {})", is_prefix));
            printer.indent();
            print_assignable_expression(printer, expr);
            printer.dedent();
        }
        ResolvedExpressionKind::ConstantPointer(constant) => {
            printer.add_line(format!("Constant {}", constant));
        }
    }
}

fn print_assignable_expression(printer: &mut Printer, expr: &ResolvedAssignableExpression) {
    match expr {
        ResolvedAssignableExpression::LocalVariable(offset) => {
            printer.add_line(format!("LocalVariable (offset: {})", offset));
        }
        ResolvedAssignableExpression::Dereference(expr) => {
            printer.add_line("Dereference".to_string());
            printer.indent();
            print_expression(printer, expr);
            printer.dedent();
        }
        ResolvedAssignableExpression::FieldAccess(expr, offset) => {
            printer.add_line(format!("FieldAccess (offset: {})", offset));
            printer.indent();
            print_assignable_expression(printer, expr);
            printer.dedent();
        }
        ResolvedAssignableExpression::ArrayIndex(array, index, element_size) => {
            printer.add_line(format!("ArrayIndex (element_size: {})", element_size));
            printer.indent();
            print_expression(printer, array);
            print_expression(printer, index);
            printer.dedent();
        }
        ResolvedAssignableExpression::PointerFieldAccess(expr, offset, indirections) => {
            printer.add_line(format!(
                "PointerFieldAccess (offset: {}, indirections: {})",
                offset, indirections
            ));
            printer.indent();
            print_expression(printer, expr);
            printer.dedent();
        }
    }
}
