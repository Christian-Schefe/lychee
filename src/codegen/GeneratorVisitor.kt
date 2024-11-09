package codegen

import lexer.OperatorToken
import parser.*
import parser.Function

class GeneratorVisitor : INodeVisitor {
    private val lines = mutableListOf<IInstruction>()
    private var labelCounter = 0
    private val variables = mutableMapOf<String, Pair<Int, Int>>()
    private var stackOffset = 0
    private var blockIndent = 0
    private var currentBreakLabel: String? = null
    private var currentContinueLabel: String? = null

    fun add(line: IInstruction) {
        lines.add(line)
    }

    fun getVarOffset(name: String): Int {
        return variables[name]?.first ?: throw IllegalArgumentException("Variable $name not found")
    }

    private fun getVarBlockIndent(name: String): Int {
        return variables[name]?.second ?: throw IllegalArgumentException("Variable $name not found")
    }

    fun takeLabel(): String {
        return ".L${labelCounter++}"
    }

    fun generateCode(node: Program): String {
        visit(node)
        return lines.joinToString("\n")
    }

    override fun visit(node: Program) {
        add(EntryPointInstruction("main"))
        node.functions.forEach { it.accept(this) }
    }

    override fun visit(node: Function) {
        add(LabelInstruction(node.name))
        add(PushInstruction(Register.RBP))
        add(MoveInstruction(Register.RSP, Register.RBP))
        node.blockItems.forEach { it.accept(this) }
        if (node.blockItems.none { it is ReturnStatement }) {
            add(MoveInstruction(ConstantOperand(0), Register.RAX))
        }
        add(MoveInstruction(Register.RBP, Register.RSP))
        add(PopInstruction(Register.RBP))
        add(RetInstruction())
    }

    override fun visit(statement: ReturnStatement) {
        statement.expression.accept(this)
    }

    override fun visit(statement: ExprStatement) {
        statement.expression.accept(this)
    }

    override fun visit(statement: DeclareBlockItem) {
        if (variables.containsKey(statement.name) && getVarBlockIndent(statement.name) == blockIndent) {
            throw IllegalArgumentException("Variable ${statement.name} already declared")
        }
        stackOffset += 8
        val offset = stackOffset
        variables[statement.name] = offset to blockIndent
        val source = if (statement.initializer.isPresent) {
            statement.initializer.get().accept(this)
            Register.RAX
        } else {
            ConstantOperand(0)
        }
        add(SubInstruction(ConstantOperand(8), Register.RSP))
        add(MoveInstruction(source, StackLocation(offset)))
    }

    override fun visit(statement: IfStatement) {
        val endLabel = takeLabel()
        statement.expr.accept(this)
        add(CmpInstruction(ConstantOperand(0), Register.RAX))
        if (statement.elseBranch.isPresent) {
            val falseLabel = takeLabel()
            add(JumpInstruction("e", falseLabel))
            statement.ifBranch.accept(this)
            add(JumpInstruction(null, endLabel))
            add(LabelInstruction(falseLabel))
            statement.elseBranch.ifPresent { it.accept(this) }
        } else {
            add(JumpInstruction("e", endLabel))
            statement.ifBranch.accept(this)
        }
        add(LabelInstruction(endLabel))
    }

    override fun visit(statement: CompoundStatement) {
        doBlock {
            statement.blockItems.forEach { it.accept(this) }
        }
    }

    fun doBlock(action: () -> Unit) {
        val oldMap = variables.toMap()
        val oldOffset = stackOffset
        blockIndent++
        action()
        add(AddInstruction(ConstantOperand(stackOffset - oldOffset), Register.RSP))
        variables.clear()
        variables.putAll(oldMap)
        stackOffset = oldOffset
        blockIndent--
    }

    override fun visit(statement: ForLoopStatement) {
        val oldBreakLabel = currentBreakLabel
        val oldContinueLabel = currentContinueLabel
        val endLabel = takeLabel()
        val continueLabel = takeLabel()
        val loopLabel = takeLabel()
        currentContinueLabel = continueLabel
        currentBreakLabel = endLabel
        doBlock {
            statement.init.accept(this)
            add(LabelInstruction(loopLabel))
            statement.condition.accept(this)
            add(CmpInstruction(ConstantOperand(0), Register.RAX))
            add(JumpInstruction("e", endLabel))
            statement.body.accept(this)
            add(LabelInstruction(continueLabel))
            statement.update.ifPresent { it.accept(this) }
            add(JumpInstruction(null, loopLabel))
            add(LabelInstruction(endLabel))
        }
        currentBreakLabel = oldBreakLabel
        currentContinueLabel = oldContinueLabel
    }

    override fun visit(statement: WhileLoopStatement) {
        val oldBreakLabel = currentBreakLabel
        val oldContinueLabel = currentContinueLabel
        val endLabel = takeLabel()
        val loopLabel = takeLabel()
        currentContinueLabel = loopLabel
        currentBreakLabel = endLabel
        add(LabelInstruction(loopLabel))
        statement.condition.accept(this)
        add(CmpInstruction(ConstantOperand(0), Register.RAX))
        add(JumpInstruction("e", endLabel))
        statement.body.accept(this)
        add(JumpInstruction(null, loopLabel))
        add(LabelInstruction(endLabel))
        currentBreakLabel = oldBreakLabel
        currentContinueLabel = oldContinueLabel
    }

    override fun visit(statement: DoWhileLoopStatement) {
        val oldBreakLabel = currentBreakLabel
        val oldContinueLabel = currentContinueLabel
        val endLabel = takeLabel()
        val continueLabel = takeLabel()
        val loopLabel = takeLabel()
        currentContinueLabel = continueLabel
        currentBreakLabel = endLabel
        add(LabelInstruction(loopLabel))
        statement.body.accept(this)
        add(LabelInstruction(continueLabel))
        statement.condition.accept(this)
        add(CmpInstruction(ConstantOperand(0), Register.RAX))
        add(JumpInstruction("ne", loopLabel))
        add(LabelInstruction(endLabel))
        currentBreakLabel = oldBreakLabel
        currentContinueLabel = oldContinueLabel
    }

    override fun visit(statement: LoopControlStatement) {
        when (statement.type) {
            LoopControlType.BREAK -> {
                if (currentBreakLabel == null) {
                    throw IllegalArgumentException("Break statement outside of loop")
                }
                add(JumpInstruction(null, currentBreakLabel!!))
            }

            LoopControlType.CONTINUE -> {
                if (currentContinueLabel == null) {
                    throw IllegalArgumentException("Continue statement outside of loop")
                }
                add(JumpInstruction(null, currentContinueLabel!!))
            }
        }
    }

    override fun visit(statement: EmptyStatement) {
        // Do nothing
    }

    override fun visit(expr: BinOpExpr) {
        when (expr.type) {
            BinOpType.ADDITIVE -> generateMathExprCode(this, expr)
            BinOpType.MULTIPLICATIVE -> generateMathExprCode(this, expr)
            BinOpType.SHIFT -> generateMathExprCode(this, expr)
            BinOpType.RELATIONAL -> generateComparisonExprCode(this, expr)
            BinOpType.EQUALITY -> generateComparisonExprCode(this, expr)
            BinOpType.BITWISE_AND -> generateMathExprCode(this, expr)
            BinOpType.BITWISE_XOR -> generateMathExprCode(this, expr)
            BinOpType.BITWISE_OR -> generateMathExprCode(this, expr)
            BinOpType.LOGICAL_AND -> generateLogicalExprCode(this, expr)
            BinOpType.LOGICAL_OR -> generateLogicalExprCode(this, expr)
            BinOpType.COMMA -> {
                expr.left.accept(this)
                expr.right.forEach { (expr, _) -> expr.accept(this) }
            }
        }
    }

    override fun visit(expr: ConstExpr) {
        add(MoveInstruction(ConstantOperand(expr.value), Register.RAX))
    }

    override fun visit(expr: AssignExpr) {
        val offset = getVarOffset(expr.name)
        expr.expr.accept(this)
        var moveBack = true
        when (expr.op) {
            OperatorToken.ASSIGN -> add(MoveInstruction(Register.RAX, StackLocation(offset)))
            OperatorToken.ADD_ASSIGN -> add(AddInstruction(Register.RAX, StackLocation(offset)))
            OperatorToken.SUBTRACT_ASSIGN -> add(SubInstruction(Register.RAX, StackLocation(offset)))
            OperatorToken.MULTIPLY_ASSIGN -> {
                add(MoveInstruction(StackLocation(offset), Register.RCX))
                add(MulInstruction(Register.RCX, Register.RAX))
                add(MoveInstruction(Register.RAX, StackLocation(offset)))
                moveBack = false
            }

            OperatorToken.BITWISE_AND_ASSIGN -> add(AndInstruction(Register.RAX, StackLocation(offset)))
            OperatorToken.BITWISE_OR_ASSIGN -> add(OrInstruction(Register.RAX, StackLocation(offset)))
            OperatorToken.BITWISE_XOR_ASSIGN -> add(XorInstruction(Register.RAX, StackLocation(offset)))

            OperatorToken.DIVIDE_ASSIGN -> {
                add(MoveInstruction(Register.RAX, Register.RCX))
                add(MoveInstruction(StackLocation(offset), Register.RAX))
                add(CqtoInstruction())
                add(DivInstruction(Register.RCX))
                add(MoveInstruction(Register.RAX, StackLocation(offset)))
                moveBack = false
            }

            OperatorToken.MODULO_ASSIGN -> {
                add(MoveInstruction(Register.RAX, Register.RCX))
                add(MoveInstruction(StackLocation(offset), Register.RAX))
                add(CqtoInstruction())
                add(DivInstruction(Register.RCX))
                add(MoveInstruction(Register.RDX, StackLocation(offset)))
            }

            OperatorToken.LEFT_SHIFT_ASSIGN -> {
                add(MoveInstruction(Register.RAX, Register.RCX))
                add(MoveInstruction(StackLocation(offset), Register.RAX))
                add(ShiftLeftInstruction(Register.CL, Register.RAX))
                add(MoveInstruction(Register.RAX, StackLocation(offset)))
                moveBack = false
            }

            OperatorToken.RIGHT_SHIFT_ASSIGN -> {
                add(MoveInstruction(Register.RAX, Register.RCX))
                add(MoveInstruction(StackLocation(offset), Register.RAX))
                add(ShiftRightInstruction(Register.CL, Register.RAX))
                add(MoveInstruction(Register.RAX, StackLocation(offset)))
                moveBack = false
            }

            else -> throw IllegalArgumentException("Invalid operator ${expr.op}")
        }
        if (moveBack) {
            add(MoveInstruction(StackLocation(offset), Register.RAX))
        }
    }

    override fun visit(expr: VarExpr) {
        val offset = getVarOffset(expr.name)
        add(MoveInstruction(StackLocation(offset), Register.RAX))
    }

    override fun visit(expr: IncrementExpr) {
        val offset = getVarOffset(expr.name)
        if (expr.isPostfix) {
            add(MoveInstruction(StackLocation(offset), Register.RAX))
            add(if (expr.isDecrement) DecInstruction(StackLocation(offset)) else IncInstruction(StackLocation(offset)))
        } else {
            add(if (expr.isDecrement) DecInstruction(StackLocation(offset)) else IncInstruction(StackLocation(offset)))
            add(MoveInstruction(StackLocation(offset), Register.RAX))
        }
    }

    override fun visit(expr: ConditionalExpr) {
        val falseLabel = takeLabel()
        val endLabel = takeLabel()
        expr.condition.accept(this)
        add(CmpInstruction(ConstantOperand(0), Register.RAX))
        add(JumpInstruction("e", falseLabel))
        expr.trueBranch.accept(this)
        add(JumpInstruction(null, endLabel))
        add(LabelInstruction(falseLabel))
        expr.falseBranch.accept(this)
        add(LabelInstruction(endLabel))
    }

    override fun visit(expr: FunctionCallExpr) {
        TODO("Not yet implemented")
    }

    override fun visit(expr: UnOpExpr) {
        expr.right.accept(this)
        when (expr.operator) {
            UnaryOperator.NEGATE -> add(NegInstruction(Register.RAX))
            UnaryOperator.BITWISE_NOT -> add(NotInstruction(Register.RAX))
            UnaryOperator.LOGICAL_NOT -> {
                add(CmpInstruction(ConstantOperand(0), Register.RAX))
                add(MoveInstruction(ConstantOperand(0), Register.RAX))
                add(SetInstruction("e", Register.AL))
            }

            UnaryOperator.POSITIVE -> {}
        }
    }
}