package parser

class PrintVisitor : INodeVisitor {
    private var indent = 0
    private fun getIndentString() = "  ".repeat(indent)

    override fun visit(node: Program) {
        println("Program")
        node.function.accept(this)
    }

    override fun visit(node: Function) {
        println("${getIndentString()}Function(name=${node.name})")
        indent += 1
        node.statement.accept(this)
        indent -= 1
    }

    override fun visit(node: Statement) {
        println("${getIndentString()}Statement")
        indent += 1
        node.expression.accept(this)
        indent -= 1
    }

    override fun visit(expr: Expr) {
        println("${getIndentString()}Expr")
        indent += 1
        expr.left.accept(this)
        expr.right.forEach { (expr, op) ->
            println("${getIndentString()}${op}")
            expr.accept(this)
        }
        indent -= 1
    }

    override fun visit(expr: FactorExpr) {
        println("${getIndentString()}FactorExpr")
        indent += 1
        expr.factor.accept(this)
        indent -= 1
    }

    override fun visit(factor: ConstFactor) {
        println("${getIndentString()}ConstFactor(value=${factor.value})")
    }

    override fun visit(factor: UnaryOpFactor) {
        println("${getIndentString()}UnaryOpFactor(operator=${factor.operator})")
        indent += 1
        factor.factor.accept(this)
        indent -= 1
    }

    override fun visit(factor: ParenExprFactor) {
        println("${getIndentString()}ParenExprFactor")
        indent += 1
        factor.expression.accept(this)
        indent -= 1
    }
}