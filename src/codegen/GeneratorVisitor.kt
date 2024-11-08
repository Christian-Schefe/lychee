package codegen

import parser.*
import parser.Function

class GeneratorVisitor : INodeVisitor {
    private val lines = mutableListOf<String>()
    private var labelCounter = 0

    fun add(line: String) {
        lines.add(line)
    }

    fun takeLabel(): String {
        return ".L${labelCounter++}"
    }

    fun generateCode(node: Program): String {
        visit(node)
        return lines.joinToString("\n")
    }

    override fun visit(node: Program) {
        visit(node.function)
    }

    override fun visit(node: Function) {
        add(".globl ${node.name}")
        add("${node.name}:")
        visit(node.statement)
    }

    override fun visit(node: Statement) {
        node.expression.accept(this)
        add("ret")
    }

    override fun visit(expr: Expr) {
        when (expr.type) {
            ExprType.ADDITIVE -> generateMathExprCode(this, expr)
            ExprType.MULTIPLICATIVE -> generateMathExprCode(this, expr)
            ExprType.SHIFT -> generateMathExprCode(this, expr)
            ExprType.RELATIONAL -> generateComparisonExprCode(this, expr)
            ExprType.EQUALITY -> generateComparisonExprCode(this, expr)
            ExprType.BITWISE_AND -> generateMathExprCode(this, expr)
            ExprType.BITWISE_XOR -> generateMathExprCode(this, expr)
            ExprType.BITWISE_OR -> generateMathExprCode(this, expr)
            ExprType.LOGICAL_AND -> generateLogicalExprCode(this, expr)
            ExprType.LOGICAL_OR -> generateLogicalExprCode(this, expr)
        }
    }

    override fun visit(expr: FactorExpr) {
        expr.factor.accept(this)
    }

    override fun visit(factor: ConstFactor) {
        add("movq \$${factor.value}, %rax")
    }

    override fun visit(factor: UnaryOpFactor) {
        factor.factor.accept(this)
        when (factor.operator) {
            UnaryOperator.NEGATE -> add("neg %rax")
            UnaryOperator.BITWISE_NOT -> add("not %rax")
            UnaryOperator.LOGICAL_NOT -> {
                add("cmpq \$0, %rax")
                add("movq \$0, %rax")
                add("sete %al")
            }

            UnaryOperator.POSITIVE -> {}
        }
    }

    override fun visit(factor: ParenExprFactor) {
        factor.expression.accept(this)
    }
}