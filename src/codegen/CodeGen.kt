package codegen

import lexer.OperatorToken
import parser.*

fun genAssembly(ast: Program): String {
    val assembly = GeneratorVisitor().generateCode(ast)
    return assembly + "\n"
}


fun generateComparisonExprCode(visitor: GeneratorVisitor, expr: Expr) {
    expr.left.accept(visitor)
    expr.right.forEach { (factor, op) ->
        visitor.add("push %rax")
        factor.accept(visitor)
        visitor.add("pop %rcx")
        visitor.add("cmpq %rax, %rcx")
        visitor.add("movq \$0, %rax")
        when (op) {
            OperatorToken.LESS_THAN -> visitor.add("setl %al")
            OperatorToken.LESS_THAN_OR_EQUAL -> visitor.add("setle %al")
            OperatorToken.GREATER_THAN -> visitor.add("setg %al")
            OperatorToken.GREATER_THAN_OR_EQUAL -> visitor.add("setge %al")
            OperatorToken.EQUAL -> visitor.add("sete %al")
            OperatorToken.NOT_EQUAL -> visitor.add("setne %al")
            else -> throw IllegalArgumentException("Invalid operator")
        }
    }
}

fun generateLogicalExprCode(visitor: GeneratorVisitor, expr: Expr) {
    expr.left.accept(visitor)
    if (expr.right.isEmpty()) {
        return
    }
    val clausePrefix = visitor.takeLabel()
    val endLabel = clausePrefix + "_end"

    expr.right.forEachIndexed { index, (factor, op) ->
        val clauseLabel = "${clausePrefix}_clause$index"
        visitor.add("cmpq \$0, %rax")
        when (op) {
            OperatorToken.LOGICAL_AND -> {
                visitor.add("jne $clauseLabel")
                visitor.add("jmp $endLabel")
                visitor.add("$clauseLabel:")
            }

            OperatorToken.LOGICAL_OR -> {
                visitor.add("je $clauseLabel")
                visitor.add("movq \$1, %rax")
                visitor.add("jmp $endLabel")
                visitor.add("$clauseLabel:")
            }

            else -> throw IllegalArgumentException("Invalid operator")
        }
        factor.accept(visitor)
        visitor.add("cmpq \$0, %rax")
        visitor.add("movq \$0, %rax")
        visitor.add("setne %al")
    }
    visitor.add("$endLabel:")
}

fun generateMathExprCode(visitor: GeneratorVisitor, expr: Expr) {
    expr.left.accept(visitor)
    expr.right.forEach { (factor, op) ->
        visitor.add("push %rax")
        factor.accept(visitor)
        val shouldSwap = when (op) {
            OperatorToken.SUBTRACT, OperatorToken.DIVIDE, OperatorToken.MODULO -> true
            OperatorToken.LEFT_SHIFT, OperatorToken.RIGHT_SHIFT -> true
            else -> false
        }
        if (shouldSwap) {
            visitor.add("movq %rax, %rcx")
            visitor.add("pop %rax")
        } else {
            visitor.add("pop %rcx")
        }
        when (op) {
            OperatorToken.BITWISE_AND -> visitor.add("and %rcx, %rax")
            OperatorToken.BITWISE_OR -> visitor.add("or %rcx, %rax")
            OperatorToken.BITWISE_XOR -> visitor.add("xor %rcx, %rax")
            OperatorToken.ADD -> visitor.add("add %rcx, %rax")
            OperatorToken.MULTIPLY -> visitor.add("imul %rcx, %rax")
            OperatorToken.SUBTRACT -> visitor.add("subq %rcx, %rax")
            OperatorToken.LEFT_SHIFT -> visitor.add("shl %cl, %rax")
            OperatorToken.RIGHT_SHIFT -> visitor.add("sar %cl, %rax")

            OperatorToken.DIVIDE -> {
                visitor.add("cqto")
                visitor.add("idivq %rcx")
            }

            OperatorToken.MODULO -> {
                visitor.add("cqto")
                visitor.add("idivq %rcx")
                visitor.add("movq %rdx, %rax")
            }

            else -> throw IllegalArgumentException("Invalid operator $op")
        }
    }
}
