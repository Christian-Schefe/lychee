package codegen

import lexer.OperatorToken
import parser.*

fun genAssembly(ast: Program): String {
    val assembly = GeneratorVisitor().generateCode(ast)
    return assembly + "\n"
}


fun generateComparisonExprCode(visitor: GeneratorVisitor, expr: BinOpExpr) {
    expr.left.accept(visitor)
    expr.right.forEach { (factor, op) ->
        visitor.add(PushInstruction(Register.RAX))
        factor.accept(visitor)
        visitor.add(PopInstruction(Register.RCX))
        visitor.add(CmpInstruction(Register.RAX, Register.RCX))
        visitor.add(MoveInstruction(ConstantOperand(0), Register.RAX))
        val condition = when (op) {
            OperatorToken.LESS_THAN -> "l"
            OperatorToken.LESS_THAN_OR_EQUAL -> "le"
            OperatorToken.GREATER_THAN -> "g"
            OperatorToken.GREATER_THAN_OR_EQUAL -> "ge"
            OperatorToken.EQUAL -> "e"
            OperatorToken.NOT_EQUAL -> "ne"
            else -> throw IllegalArgumentException("Invalid operator")
        }
        visitor.add(SetInstruction(condition, Register.AL))
    }
}

fun generateLogicalExprCode(visitor: GeneratorVisitor, expr: BinOpExpr) {
    expr.left.accept(visitor)
    if (expr.right.isEmpty()) {
        return
    }
    val clausePrefix = visitor.takeLabel()
    val endLabel = clausePrefix + "_end"

    expr.right.forEachIndexed { index, (factor, op) ->
        val clauseLabel = "${clausePrefix}_clause$index"
        visitor.add(CmpInstruction(ConstantOperand(0), Register.RAX))
        when (op) {
            OperatorToken.LOGICAL_AND -> {
                visitor.add(JumpInstruction("ne", clauseLabel))
            }

            OperatorToken.LOGICAL_OR -> {
                visitor.add(JumpInstruction("e", clauseLabel))
                visitor.add(MoveInstruction(ConstantOperand(1), Register.RAX))
            }

            else -> throw IllegalArgumentException("Invalid operator")
        }
        visitor.add(JumpInstruction(null, endLabel))
        visitor.add(LabelInstruction(clauseLabel))
        factor.accept(visitor)
        visitor.add(CmpInstruction(ConstantOperand(0), Register.RAX))
        visitor.add(MoveInstruction(ConstantOperand(0), Register.RAX))
        visitor.add(SetInstruction("ne", Register.AL))
    }
    visitor.add(LabelInstruction(endLabel))
}

fun generateMathExprCode(visitor: GeneratorVisitor, expr: BinOpExpr) {
    expr.left.accept(visitor)
    expr.right.forEach { (factor, op) ->
        visitor.add(PushInstruction(Register.RAX))
        factor.accept(visitor)
        val shouldSwap = when (op) {
            OperatorToken.SUBTRACT, OperatorToken.DIVIDE, OperatorToken.MODULO -> true
            OperatorToken.LEFT_SHIFT, OperatorToken.RIGHT_SHIFT -> true
            else -> false
        }
        if (shouldSwap) {
            visitor.add(MoveInstruction(Register.RAX, Register.RCX))
            visitor.add(PopInstruction(Register.RAX))
        } else {
            visitor.add(PopInstruction(Register.RCX))
        }
        when (op) {
            OperatorToken.BITWISE_AND -> visitor.add(AndInstruction(Register.RCX, Register.RAX))
            OperatorToken.BITWISE_OR -> visitor.add(OrInstruction(Register.RCX, Register.RAX))
            OperatorToken.BITWISE_XOR -> visitor.add(XorInstruction(Register.RCX, Register.RAX))
            OperatorToken.ADD -> visitor.add(AddInstruction(Register.RCX, Register.RAX))
            OperatorToken.MULTIPLY -> visitor.add(MulInstruction(Register.RCX, Register.RAX))
            OperatorToken.SUBTRACT -> visitor.add(SubInstruction(Register.RCX, Register.RAX))
            OperatorToken.LEFT_SHIFT -> visitor.add(ShiftLeftInstruction(Register.CL, Register.RAX))
            OperatorToken.RIGHT_SHIFT -> visitor.add(ShiftRightInstruction(Register.CL, Register.RAX))

            OperatorToken.DIVIDE -> {
                visitor.add(CqtoInstruction())
                visitor.add(DivInstruction(Register.RCX))
            }

            OperatorToken.MODULO -> {
                visitor.add(CqtoInstruction())
                visitor.add(DivInstruction(Register.RCX))
                visitor.add(MoveInstruction(Register.RDX, Register.RAX))
            }

            else -> throw IllegalArgumentException("Invalid operator $op")
        }
    }
}
