package codegen

import parser.*
import parser.Function

class GeneratorVisitor : INodeVisitor {
    val lines = mutableListOf<String>()

    fun generateCode(node: INode): String {
        node.accept(this)
        return lines.joinToString("\n")
    }

    override fun visit(node: Program) {
        node.function.accept(this)
    }

    override fun visit(node: Function) {
        lines.add(".globl ${node.name}")
        lines.add("${node.name}:")
        node.statement.accept(this)
    }

    override fun visit(node: Statement) {
        node.expression.accept(this)
        lines.add("ret")
    }

    override fun visit(node: Expression) {
        node.leftTerm.accept(this)
        if (node.rightTerms.isEmpty()) {
            return
        }
        node.rightTerms.forEach { (isAddition, term) ->
            lines.add("push %rax")
            term.accept(this)
            lines.add("pop %rcx")
            if (isAddition) {
                lines.add("addq %rcx, %rax")
            } else {
                lines.add("subq %rcx, %rax")
                lines.add("neg %rax")
            }
        }
    }

    override fun visit(node: Term) {
        node.leftFactor.accept(this)
        if (node.rightFactors.isEmpty()) {
            return
        }
        node.rightFactors.forEach { (isMultiplication, factor) ->
            lines.add("push %rax")
            factor.accept(this)
            lines.add("pop %rcx")
            if (isMultiplication) {
                lines.add("imulq %rcx, %rax")
            } else {
                lines.add("cltd")
                lines.add("idivq %rcx")
            }
        }
    }

    override fun visit(node: ConstFactor) {
        lines.add("movq \$${node.value}, %rax")
    }

    override fun visit(node: UnaryOpFactor) {
        node.factor.accept(this)
        when (node.operator) {
            UnaryOperator.NEGATE -> lines.add("neg %rax")
            UnaryOperator.BITWISE_NOT -> lines.add("not %rax")
            UnaryOperator.LOGICAL_NOT -> {
                lines.add("cmpq \$0, %rax")
                lines.add("movq \$0, %rax")
                lines.add("sete %al")
            }
        }
    }

    override fun visit(node: ParenExprFactor) {
        node.expression.accept(this)
    }
}