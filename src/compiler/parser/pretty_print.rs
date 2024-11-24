use crate::compiler::parser::syntax_tree::{Expression, Function, Program, Statement, StructDefinition};

pub trait PrettyPrint {
    fn pretty_print(&self, indent: usize) -> String;
}

impl PrettyPrint for Program {
    fn pretty_print(&self, indent: usize) -> String {
        let mut result = String::new();
        for function in &self.functions {
            result.push_str(&function.value.pretty_print(indent));
        }
        for struct_def in &self.struct_definitions {
            result.push_str(&struct_def.value.pretty_print(indent));
        }
        result
    }
}

impl PrettyPrint for Function {
    fn pretty_print(&self, indent: usize) -> String {
        let mut result = format!("fn {}(", self.name);
        for (i, (name, ty)) in self.args.iter().enumerate() {
            result.push_str(&format!("{:?}: {:?}", name, ty));
            if i < self.args.len() - 1 {
                result.push_str(", ");
            }
        }
        result.push_str(") -> ");
        result.push_str(&format!("{:?}", self.return_type));

        result.push_str("\n");
        result.push_str(&self.expr.value.pretty_print(indent));
        result.push_str("\n\n");
        result
    }
}

impl PrettyPrint for Statement {
    fn pretty_print(&self, indent: usize) -> String {
        let mut result = String::new();
        for _ in 0..indent {
            result.push_str("    ");
        }
        match self {
            Statement::Return(expr) => {
                result.push_str("return ");
                if let Some(expr) = expr {
                    result.push_str(&expr.value.pretty_print(indent));
                }
                result.push_str(";\n");
            }
            Statement::Declaration {
                var_type,
                name,
                value,
            } => {
                result.push_str(&format!("{:?} {}", var_type, name));
                result.push_str(" = ");
                result.push_str(&value.value.pretty_print(indent));
                result.push_str(";\n");
            }
            Statement::Expr(expr) => {
                result.push_str(&expr.value.pretty_print(indent));
                result.push_str(";\n");
            }
            Statement::If {
                condition,
                true_expr,
                false_statement,
            } => {
                result.push_str("if ");
                result.push_str(&condition.value.pretty_print(indent));
                result.push_str(" ");
                result.push_str(&true_expr.value.pretty_print(indent));
                if let Some(false_statement) = false_statement {
                    result.push_str(" else ");
                    result.push_str(&false_statement.value.pretty_print(indent));
                }
                result.push_str("\n");
            }
            Statement::For {
                init,
                condition,
                update,
                body,
            } => {
                result.push_str("for ");
                result.push_str(&init.value.pretty_print(indent));
                result.push_str(&condition.value.pretty_print(indent));
                result.push_str("; ");
                result.push_str(&update.value.pretty_print(indent));
                result.push_str(" ");
                result.push_str(&body.value.pretty_print(indent));
                result.push_str("\n");
            }
            Statement::While {
                condition,
                body,
                is_do_while,
            } => {
                if *is_do_while {
                    result.push_str("do ");
                    result.push_str(&body.value.pretty_print(indent));
                    result.push_str(" while ");
                    result.push_str(&condition.value.pretty_print(indent));
                } else {
                    result.push_str("while ");
                    result.push_str(&condition.value.pretty_print(indent));
                    result.push_str(" ");
                    result.push_str(&body.value.pretty_print(indent));
                }
                result.push_str("\n");
            }
        }
        result
    }
}

impl PrettyPrint for Expression {
    fn pretty_print(&self, indent: usize) -> String {
        match self {
            Expression::Block(statements, expr) => {
                let mut result = String::new();
                result.push_str("{\n");
                for statement in statements {
                    result.push_str(&statement.value.pretty_print(indent + 1));
                }
                if let Some(expr) = expr {
                    for _ in 0..=indent {
                        result.push_str("    ");
                    }
                    result.push_str(&expr.value.pretty_print(indent + 1));
                    result.push_str("\n");
                }
                for _ in 0..indent {
                    result.push_str("    ");
                }
                result.push_str("}");
                result
            }
            Expression::Binary { op, left, right } => {
                format!(
                    "({} {:?} {})",
                    left.value.pretty_print(indent),
                    op,
                    right.value.pretty_print(indent)
                )
            }
            Expression::Unary { op, expr } => {
                format!("{:?} {}", op, expr.value.pretty_print(indent))
            }
            Expression::Literal(lit) => format!("{:?}", lit),
            Expression::Variable(name) => name.clone(),
            Expression::Ternary {
                condition,
                true_expr,
                false_expr,
            } => {
                format!(
                    "{} ? {} : {}",
                    condition.value.pretty_print(indent),
                    true_expr.value.pretty_print(indent),
                    false_expr.value.pretty_print(indent)
                )
            }
            Expression::FunctionCall { function, args } => {
                let mut result = format!("{}(", function);
                for (i, arg) in args.iter().enumerate() {
                    result.push_str(&arg.value.pretty_print(indent));
                    if i < args.len() - 1 {
                        result.push_str(", ");
                    }
                }
                result.push_str(")");
                result
            }
            Expression::Cast { var_type, expr } => {
                format!("({:?}){}", var_type, expr.value.pretty_print(indent))
            }
            Expression::Sizeof(ty) => format!("sizeof({:?})", ty),
            Expression::StructLiteral { struct_type, fields } => {
                let mut result = format!("({struct_type:?}) {{\n");
                for (field_name, field_expr) in fields {
                    for _ in 0..=indent {
                        result.push_str("    ");
                    }
                    result.push_str(&format!("{:?}: {:?};\n", field_name, field_expr.value));
                }
                for _ in 0..indent {
                    result.push_str("    ");
                }
                result.push_str("}");
                result
            }
            Expression::MemberAccess { expr, member } => {
                format!("{}.{}", expr.value.pretty_print(indent), member)
            }
            Expression::Borrow(expr) => format!("&{}", expr.value.pretty_print(indent)),
            Expression::Dereference(expr) => format!("*{}", expr.value.pretty_print(indent)),
            Expression::Increment { is_increment: increment, expr, postfix } => {
                if *increment {
                    if *postfix {
                        format!("{}++", expr.value.pretty_print(indent))
                    } else {
                        format!("++{}", expr.value.pretty_print(indent))
                    }
                } else {
                    if *postfix {
                        format!("{}--", expr.value.pretty_print(indent))
                    } else {
                        format!("--{}", expr.value.pretty_print(indent))
                    }
                }
            }
        }
    }
}

impl PrettyPrint for StructDefinition {
    fn pretty_print(&self, indent: usize) -> String {
        let mut result = format!("struct {} {{\n", self.name);
        for (field_name, field_ty) in &self.fields {
            for _ in 0..=indent {
                result.push_str("    ");
            }
            result.push_str(&format!("{:?}: {:?};\n", field_name, field_ty));
        }
        for _ in 0..indent {
            result.push_str("    ");
        }
        result.push_str("}\n\n");
        result
    }
}
