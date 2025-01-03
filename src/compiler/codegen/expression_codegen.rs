use crate::compiler::analyzer::analyzed_expression::{
    AnalyzedBinaryOp, AnalyzedLiteral, BinaryAssignOp,
};
use crate::compiler::codegen::CodegenContext;
use crate::compiler::parser::binary_op::{BinaryComparisonOp, BinaryLogicOp, BinaryMathOp};
use crate::compiler::parser::parsed_expression::UnaryMathOp;
use crate::compiler::resolver::resolved_expression::{
    ResolvedAssignableExpression, ResolvedExpression, ResolvedExpressionKind,
    ResolvedFunctionCallType, ResolvedUnaryOp, ValueData, ValueLocation,
};

pub fn generate_expression_code(context: &mut CodegenContext, expression: &ResolvedExpression) {
    match &expression.kind {
        ResolvedExpressionKind::Block(expressions) => {
            for expr in expressions {
                generate_expression_code(context, expr);
            }
        }
        ResolvedExpressionKind::Return(maybe_expr) => {
            if let Some(expr) = maybe_expr {
                generate_expression_code(context, expr);
            }

            let return_label = context.return_label.clone();
            context.jmp(&return_label);
        }
        ResolvedExpressionKind::Continue => {
            do_stack_discard(
                context,
                context.current_stack_size - context.last_loop_stack_size,
                false,
            );

            let continue_label = context.continue_label.clone();
            context.jmp(&continue_label);
        }
        ResolvedExpressionKind::Break { maybe_expr } => {
            do_stack_discard(
                context,
                context.current_stack_size - context.last_loop_stack_size,
                false,
            );

            if let Some(expr) = maybe_expr {
                generate_expression_code(context, expr);
            }

            let break_label = context.break_label.clone();
            context.jmp(&break_label);
        }
        ResolvedExpressionKind::If {
            condition,
            then_block,
            else_expr,
        } => {
            let else_label = context.new_label("else");
            let end_label = context.new_label("end");

            generate_expression_code(context, condition);
            context.cmpi("r0", 0);
            context.jz(&else_label);

            generate_expression_code(context, then_block);
            context.jmp(&end_label);

            context.label(&else_label);
            if let Some(else_expr) = else_expr {
                generate_expression_code(context, else_expr);
            }

            context.label(&end_label);
        }
        ResolvedExpressionKind::Loop {
            init,
            condition,
            step,
            loop_body,
            else_expr,
        } => {
            let loop_label = context.new_label("loop");
            let continue_label = context.new_label("continue");
            let else_label = context.new_label("else");
            let break_label = context.new_label("break");

            if let Some(init_expr) = init {
                generate_expression_code(context, init_expr);
            }

            context.label(&loop_label);

            if let Some(condition_expr) = condition {
                generate_expression_code(context, condition_expr);
                context.cmpi("r0", 0);
                context.jz(&else_label);
            }

            let old_break_label = context.break_label.clone();
            let old_continue_label = context.continue_label.clone();
            context.break_label = break_label.clone();
            context.continue_label = continue_label.clone();

            let old_loop_stack_size = context.last_loop_stack_size;
            context.last_loop_stack_size = context.current_stack_size;

            generate_expression_code(context, loop_body);

            context.label(&continue_label);
            if let Some(step_expr) = step {
                generate_expression_code(context, step_expr);
            }

            context.last_loop_stack_size = old_loop_stack_size;

            context.break_label = old_break_label;
            context.continue_label = old_continue_label;

            context.jmp(&loop_label);
            context.label(&else_label);

            if let Some(else_expr) = else_expr {
                generate_expression_code(context, else_expr);
            }

            context.label(&break_label);
        }
        ResolvedExpressionKind::Declaration { var_offset, value } => {
            generate_expression_code(context, value);
            store_from_value_data(
                context,
                &value.value_data,
                "r0",
                &format!("[bp;{}]", var_offset),
                true,
            );
        }
        ResolvedExpressionKind::ValueOfAssignable(assignable) => {
            generate_assignable_expression_value_code(context, assignable, &expression.value_data);
        }
        ResolvedExpressionKind::StructInstance { fields } => {
            for field in fields {
                generate_expression_code(context, field);
                if let ValueLocation::Register = field.value_data.location {
                    context.push(field.value_data.size, "r0");
                    context.current_stack_size += field.value_data.size;
                }
            }
        }
        ResolvedExpressionKind::Literal(lit) => match lit {
            AnalyzedLiteral::Unit => {}
            AnalyzedLiteral::Bool(b) => {
                context.movi("r0", if *b { 1 } else { 0 });
            }
            AnalyzedLiteral::Char(c) => {
                context.movi("r0", *c as isize);
            }
            AnalyzedLiteral::Integer(i) => {
                context.movi("r0", *i as isize);
            }
        },
        ResolvedExpressionKind::Unary { op, expr } => {
            generate_expression_code(context, expr);
            match op {
                ResolvedUnaryOp::Math(math_op) => match math_op {
                    UnaryMathOp::Positive => {}
                    UnaryMathOp::Negate => {
                        context.neg("r0");
                    }
                    UnaryMathOp::BitwiseNot => {
                        context.not("r0");
                    }
                },
                ResolvedUnaryOp::LogicalNot => {
                    context.cmpi("r0", 0);
                    context.setz("r0");
                }
                ResolvedUnaryOp::IntCast(smaller_size) => {
                    context.signext(*smaller_size, "r0");
                }
                ResolvedUnaryOp::BoolCast => {
                    context.cmpi("r0", 0);
                    context.setnz("r0");
                }
                ResolvedUnaryOp::PointerCast => {}
            }
        }
        ResolvedExpressionKind::Borrow { expr } => {
            generate_assignable_expression_pointer_code(context, expr);
        }
        ResolvedExpressionKind::FunctionCall {
            call_type,
            args,
            return_stack_space,
        } => {
            if *return_stack_space > 0 {
                context.subi("sp", *return_stack_space as isize);
                context.current_stack_size += *return_stack_space;
            }
            for arg in args {
                generate_expression_code(context, arg);
                if let ValueLocation::Register = arg.value_data.location {
                    context.push(arg.value_data.size, "r0");
                    context.current_stack_size += arg.value_data.size;
                }
            }
            match call_type {
                ResolvedFunctionCallType::Pointer(ptr) => {
                    generate_expression_code(context, ptr);
                    context.call_address("[r0]");
                }
                ResolvedFunctionCallType::Function(function_name) => {
                    context.call_function(function_name)
                }
            }
        }
        ResolvedExpressionKind::FieldAccess {
            field_offset,
            expr,
            struct_size,
        } => {
            generate_expression_code(context, expr);
            do_stack_discard(context, *struct_size, true);
            load_from_value_data(
                context,
                &expression.value_data,
                "r0",
                &format!("[sp;{}]", *field_offset as isize - *struct_size as isize),
            );
        }
        ResolvedExpressionKind::Increment(expr, is_prefix) => {
            generate_assignable_expression_pointer_code(context, expr);
            context.load(expression.value_data.size, "r1", "[r0]");
            if *is_prefix {
                context.inc("r1");
                context.push(expression.value_data.size, "r1");
            } else {
                context.push(expression.value_data.size, "r1");
                context.inc("r1");
            }
            context.store(expression.value_data.size, "r1", "[r0]");
            context.pop(expression.value_data.size, "r0");
        }
        ResolvedExpressionKind::Decrement(expr, is_prefix) => {
            generate_assignable_expression_pointer_code(context, expr);
            context.load(expression.value_data.size, "r1", "[r0]");
            if *is_prefix {
                context.dec("r1");
                context.push(expression.value_data.size, "r1");
            } else {
                context.push(expression.value_data.size, "r1");
                context.dec("r1");
            }
            context.store(expression.value_data.size, "r1", "[r0]");
            context.pop(expression.value_data.size, "r0");
        }
        ResolvedExpressionKind::Binary { op, left, right } => match op {
            AnalyzedBinaryOp::Math(math_op) => {
                generate_expression_code(context, left);
                context.push(left.value_data.size, "r0");
                context.current_stack_size += left.value_data.size;
                generate_expression_code(context, right);
                context.mov("r1", "r0");
                context.pop(left.value_data.size, "r0");
                context.current_stack_size -= left.value_data.size;
                do_math_op(context, math_op, "r0", "r1");
            }
            AnalyzedBinaryOp::Logical(logical_op) => {
                let short_circuit_label = context.new_label("short_circuit");
                generate_expression_code(context, left);
                context.cmpi("r0", 0);
                match logical_op {
                    BinaryLogicOp::And => context.jz(&short_circuit_label),
                    BinaryLogicOp::Or => context.jnz(&short_circuit_label),
                }
                generate_expression_code(context, right);
                context.label(&short_circuit_label);
            }
            AnalyzedBinaryOp::Comparison(comp_op) => {
                generate_expression_code(context, left);
                context.push(left.value_data.size, "r0");
                context.current_stack_size += left.value_data.size;
                generate_expression_code(context, right);
                context.mov("r1", "r0");
                context.pop(left.value_data.size, "r0");
                context.current_stack_size -= left.value_data.size;
                do_comp_op(context, comp_op, "r0", "r1");
            }
        },
        ResolvedExpressionKind::Assign { op, lhs, rhs } => match op {
            BinaryAssignOp::Assign => {
                generate_expression_code(context, rhs);
                if let ValueLocation::Stack = rhs.value_data.location {
                    generate_assignable_expression_pointer_code(context, lhs);
                    store_from_value_data(context, &rhs.value_data, "r1", "[r0]", false);
                } else {
                    context.push(8, "r0");
                    context.current_stack_size += 8;
                    generate_assignable_expression_pointer_code(context, lhs);
                    context.mov("r1", "r0");
                    context.pop(8, "r0");
                    context.current_stack_size -= 8;
                    context.store(expression.value_data.size, "r0", "[r1]");
                }
            }
            BinaryAssignOp::MathAssign(math_op) => {
                generate_expression_code(context, rhs);
                context.push(expression.value_data.size, "r0");
                context.current_stack_size += expression.value_data.size;
                generate_assignable_expression_pointer_code(context, lhs);
                context.mov("r1", "r0");
                context.load(expression.value_data.size, "r0", "[r1]");
                context.pop(expression.value_data.size, "r2");
                context.current_stack_size -= expression.value_data.size;
                do_math_op(context, math_op, "r0", "r2");
                context.store(expression.value_data.size, "r0", "[r1]");
            }
            BinaryAssignOp::LogicAssign(logic_op) => {
                let short_circuit_label = context.new_label("short_circuit");
                generate_assignable_expression_pointer_code(context, lhs);
                context.load(expression.value_data.size, "r1", "[r0]");
                context.cmpi("r1", 0);
                match logic_op {
                    BinaryLogicOp::And => context.jz(&short_circuit_label),
                    BinaryLogicOp::Or => context.jnz(&short_circuit_label),
                }
                context.push(8, "r0");
                context.current_stack_size += 8;
                generate_expression_code(context, rhs);
                context.pop(8, "r1");
                context.current_stack_size -= 8;
                context.store(expression.value_data.size, "r0", "[r1]");
                context.label(&short_circuit_label);
            }
        },
        ResolvedExpressionKind::ConstantPointer(constant) => {
            let label = context.constant_labels[*constant].clone();
            context.lea("r0", &label);
        }
        ResolvedExpressionKind::FunctionPointer(function) => {
            let label = context.function_labels[function].clone();
            context.lea("r0", &label);
        }
    };

    do_stack_discard(context, expression.stack_discard, true);
}

fn generate_assignable_expression_pointer_code(
    context: &mut CodegenContext,
    expression: &ResolvedAssignableExpression,
) {
    match expression {
        ResolvedAssignableExpression::LocalVariable(var_offset) => {
            context.lea("r0", &format!("[bp;{}]", var_offset));
        }
        ResolvedAssignableExpression::Dereference(expr) => {
            generate_expression_code(context, expr);
        }
        ResolvedAssignableExpression::FieldAccess(expr, field_offset) => {
            generate_assignable_expression_pointer_code(context, expr);
            context.addi("r0", *field_offset as isize);
        }
        ResolvedAssignableExpression::ArrayIndex(array, index, element_size) => {
            generate_expression_code(context, index);
            context.push(8, "r0");
            context.current_stack_size += 8;
            generate_expression_code(context, array);
            context.pop(8, "r1");
            context.current_stack_size -= 8;
            context.muli("r1", *element_size as isize);
            context.add("r0", "r1");
        }
        ResolvedAssignableExpression::PointerFieldAccess(expr, field_offset, indirections) => {
            generate_expression_code(context, expr);
            for _ in 0..(*indirections - 1) {
                context.load(8, "r0", "[r0]");
            }
            context.addi("r0", *field_offset as isize);
        }
    }
}

fn generate_assignable_expression_value_code(
    context: &mut CodegenContext,
    expression: &ResolvedAssignableExpression,
    data: &ValueData,
) {
    match expression {
        ResolvedAssignableExpression::LocalVariable(var_offset) => {
            load_from_value_data(context, data, "r0", &format!("[bp;{}]", var_offset));
        }
        ResolvedAssignableExpression::Dereference(expr) => {
            generate_expression_code(context, expr);
            load_from_value_data(context, data, "r1", "[r0]");
            if let ValueLocation::Register = data.location {
                context.mov("r0", "r1");
            }
        }
        ResolvedAssignableExpression::FieldAccess(expr, field_offset) => {
            generate_assignable_expression_pointer_code(context, expr);
            load_from_value_data(context, data, "r1", &format!("[r0;{}]", field_offset));
            if let ValueLocation::Register = data.location {
                context.mov("r0", "r1");
            }
        }
        ResolvedAssignableExpression::ArrayIndex(array, index, element_size) => {
            generate_expression_code(context, index);
            context.push(8, "r0");
            context.current_stack_size += 8;
            generate_expression_code(context, array);
            context.pop(8, "r1");
            context.current_stack_size -= 8;
            context.muli("r1", *element_size as isize);
            context.add("r1", "r0");
            load_from_value_data(context, data, "r0", "[r1]");
        }
        ResolvedAssignableExpression::PointerFieldAccess(expr, field_offset, indirections) => {
            generate_expression_code(context, expr);
            for _ in 0..(*indirections - 1) {
                context.load(8, "r0", "[r0]");
            }
            load_from_value_data(context, data, "r1", &format!("[r0;{}]", field_offset));
            if let ValueLocation::Register = data.location {
                context.mov("r0", "r1");
            }
        }
    }
}

fn do_stack_discard(context: &mut CodegenContext, amount: usize, tracked: bool) {
    if amount > 0 {
        context.addi("sp", amount as isize);
        if tracked {
            context.current_stack_size -= amount;
        }
    }
}

fn do_math_op(
    context: &mut CodegenContext,
    math_op: &BinaryMathOp,
    dest_register: &str,
    source_register: &str,
) {
    match math_op {
        BinaryMathOp::Add => {
            context.add(dest_register, source_register);
        }
        BinaryMathOp::Sub => {
            context.sub(dest_register, source_register);
        }
        BinaryMathOp::Mul => {
            context.mul(dest_register, source_register);
        }
        BinaryMathOp::Div => {
            context.div(dest_register, source_register);
        }
        BinaryMathOp::Mod => {
            context.modulo(dest_register, source_register);
        }
        BinaryMathOp::And => {
            context.and(dest_register, source_register);
        }
        BinaryMathOp::Or => {
            context.or(dest_register, source_register);
        }
        BinaryMathOp::Xor => {
            context.xor(dest_register, source_register);
        }
        BinaryMathOp::Shl => {
            context.shl(dest_register, source_register);
        }
        BinaryMathOp::Shr => {
            context.shr(dest_register, source_register);
        }
    }
}

fn do_comp_op(
    context: &mut CodegenContext,
    comp_op: &BinaryComparisonOp,
    dest_register: &str,
    source_register: &str,
) {
    context.cmp(dest_register, source_register);
    match comp_op {
        BinaryComparisonOp::Equals => {
            context.setz(dest_register);
        }
        BinaryComparisonOp::NotEquals => {
            context.setnz(dest_register);
        }
        BinaryComparisonOp::Less => {
            context.setl(dest_register);
        }
        BinaryComparisonOp::LessEquals => {
            context.setle(dest_register);
        }
        BinaryComparisonOp::Greater => {
            context.setg(dest_register);
        }
        BinaryComparisonOp::GreaterEquals => {
            context.setge(dest_register);
        }
    }
}

fn store_from_value_data(
    context: &mut CodegenContext,
    value_data: &ValueData,
    value_register: &str,
    address: &str,
    do_pop: bool,
) {
    match value_data.location {
        ValueLocation::Stack => {
            context.movi(value_register, value_data.size as isize);
            if do_pop {
                context.popmem(value_register, address);
                context.current_stack_size -= value_data.size;
            } else {
                context.peekmem(value_register, address);
            }
        }
        ValueLocation::Register => {
            context.store(value_data.size, value_register, address);
        }
        ValueLocation::None => {}
    }
}

fn load_from_value_data(
    context: &mut CodegenContext,
    value_data: &ValueData,
    value_register: &str,
    address: &str,
) {
    match value_data.location {
        ValueLocation::Stack => {
            context.movi(value_register, value_data.size as isize);
            context.pushmem(value_register, address);
            context.current_stack_size += value_data.size;
        }
        ValueLocation::Register => {
            context.load(value_data.size, value_register, address);
        }
        ValueLocation::None => {}
    }
}
