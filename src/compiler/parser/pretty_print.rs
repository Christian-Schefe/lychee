use crate::compiler::parser::syntax_tree::{Expression, Function, Program, Statement};

pub trait PrettyPrint {
    fn pretty_print(&self, indent: usize) -> String;
}

impl PrettyPrint for Program {
    fn pretty_print(&self, indent: usize) -> String {
        let mut result = String::new();
        for function in &self.functions {
            result.push_str(&function.function.pretty_print(indent));
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
        if let Some(ty) = &self.return_type {
            result.push_str(&format!("{:?}", ty));
        } else {
            result.push_str("()");
        }
        result.push_str("\n");
        result.push_str(&self.expr.expr.pretty_print(indent));
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
                    result.push_str(&expr.expr.pretty_print(indent));
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
                result.push_str(&value.expr.pretty_print(indent));
                result.push_str(";\n");
            }
            Statement::Expr(expr) => {
                result.push_str(&expr.expr.pretty_print(indent));
                result.push_str(";\n");
            }
            Statement::If {
                condition,
                true_expr,
                false_expr,
            } => {
                result.push_str("if ");
                result.push_str(&condition.expr.pretty_print(indent));
                result.push_str(" ");
                result.push_str(&true_expr.expr.pretty_print(indent));
                if let Some(false_expr) = false_expr {
                    result.push_str(" else ");
                    result.push_str(&false_expr.expr.pretty_print(indent));
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
                    result.push_str(&statement.statement.pretty_print(indent + 1));
                }
                if let Some(expr) = expr {
                    for _ in 0..=indent {
                        result.push_str("    ");
                    }
                    result.push_str(&expr.expr.pretty_print(indent + 1));
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
                    left.expr.pretty_print(indent),
                    op,
                    right.expr.pretty_print(indent)
                )
            }
            Expression::Unary { op, expr } => {
                format!("{:?} {}", op, expr.expr.pretty_print(indent))
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
                    condition.expr.pretty_print(indent),
                    true_expr.expr.pretty_print(indent),
                    false_expr.expr.pretty_print(indent)
                )
            }
            Expression::FunctionCall { function, args } => {
                let mut result = format!("{}(", function);
                for (i, arg) in args.iter().enumerate() {
                    result.push_str(&arg.expr.pretty_print(indent));
                    if i < args.len() - 1 {
                        result.push_str(", ");
                    }
                }
                result.push_str(")");
                result
            }
            Expression::Cast { var_type, expr } => {
                format!("({:?}){}", var_type, expr.expr.pretty_print(indent))
            }
        }
    }
}
