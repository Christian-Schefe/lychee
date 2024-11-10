package codegen

import lexer.SymbolToken
import parser.*
/*
fun genAssembly(ast: Program): String {
    val assembly = GeneratorVisitor().generateCode(ast)
    return assembly + "\n"
}


fun generateComparisonExprCode(visitor: GeneratorVisitor, expr: BinOpExpr) {
    expr.left.accept(visitor)
    expr.right.forEach { (factor, op) ->
        visitor.add(PushInstruction(Register.A))
        factor.accept(visitor)
        visitor.add(PopInstruction(Register.C))
        visitor.add(CmpInstruction(Register.A, Register.C))
        visitor.add(MoveInstruction(ConstantOperand(0), Register.A))
        val condition = when (op) {
            SymbolToken.LESS_THAN -> "l"
            SymbolToken.LESS_THAN_OR_EQUAL -> "le"
            SymbolToken.GREATER_THAN -> "g"
            SymbolToken.GREATER_THAN_OR_EQUAL -> "ge"
            SymbolToken.EQUAL -> "e"
            SymbolToken.NOT_EQUAL -> "ne"
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
        visitor.add(CmpInstruction(ConstantOperand(0), Register.A))
        when (op) {
            SymbolToken.LOGICAL_AND -> {
                visitor.add(JumpInstruction("ne", clauseLabel))
            }

            SymbolToken.LOGICAL_OR -> {
                visitor.add(JumpInstruction("e", clauseLabel))
                visitor.add(MoveInstruction(ConstantOperand(1), Register.A))
            }

            else -> throw IllegalArgumentException("Invalid operator")
        }
        visitor.add(JumpInstruction(null, endLabel))
        visitor.add(LabelInstruction(clauseLabel))
        factor.accept(visitor)
        visitor.add(CmpInstruction(ConstantOperand(0), Register.A))
        visitor.add(MoveInstruction(ConstantOperand(0), Register.A))
        visitor.add(SetInstruction("ne", Register.AL))
    }
    visitor.add(LabelInstruction(endLabel))
}

fun generateMathExprCode(visitor: GeneratorVisitor, expr: BinOpExpr) {
    expr.left.accept(visitor)
    expr.right.forEach { (factor, op) ->
        visitor.add(PushInstruction(Register.A))
        factor.accept(visitor)
        val shouldSwap = when (op) {
            SymbolToken.MINUS, SymbolToken.DIVIDE, SymbolToken.MODULO -> true
            SymbolToken.LEFT_SHIFT, SymbolToken.RIGHT_SHIFT -> true
            else -> false
        }
        if (shouldSwap) {
            visitor.add(MoveInstruction(Register.A, Register.C))
            visitor.add(PopInstruction(Register.A))
        } else {
            visitor.add(PopInstruction(Register.C))
        }
        when (op) {
            SymbolToken.BITWISE_AND -> visitor.add(AndInstruction(Register.C, Register.A))
            SymbolToken.BITWISE_OR -> visitor.add(OrInstruction(Register.C, Register.A))
            SymbolToken.BITWISE_XOR -> visitor.add(XorInstruction(Register.C, Register.A))
            SymbolToken.PLUS -> visitor.add(AddInstruction(Register.C, Register.A))
            SymbolToken.ASTERISK -> visitor.add(MulInstruction(Register.C, Register.A))
            SymbolToken.MINUS -> visitor.add(SubInstruction(Register.C, Register.A))
            SymbolToken.LEFT_SHIFT -> visitor.add(ShiftLeftInstruction(Register.CL, Register.A))
            SymbolToken.RIGHT_SHIFT -> visitor.add(ShiftRightInstruction(Register.CL, Register.A))

            SymbolToken.DIVIDE -> {
                visitor.add(CqtoInstruction())
                visitor.add(DivInstruction(Register.C))
            }

            SymbolToken.MODULO -> {
                visitor.add(CqtoInstruction())
                visitor.add(DivInstruction(Register.C))
                visitor.add(MoveInstruction(Register.D, Register.A))
            }

            else -> throw IllegalArgumentException("Invalid operator $op")
        }
    }
}*/
