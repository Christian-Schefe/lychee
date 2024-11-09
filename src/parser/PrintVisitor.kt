package parser

class PrintVisitor : INodeVisitor {
    private var indent = 0
    private fun getIndentString() = "  ".repeat(indent)

    override fun visit(node: Program) {
        println("Program")
        node.functions.forEach { it.accept(this) }
    }

    override fun visit(node: Function) {
        println("${getIndentString()}Function(name=${node.name})")
        indent += 1
        node.blockItems.forEach { it.accept(this) }
        indent -= 1
    }

    override fun visit(statement: ReturnStatement) {
        println("${getIndentString()}ReturnStatement")
        indent += 1
        statement.expression.accept(this)
        indent -= 1
    }

    override fun visit(statement: ExprStatement) {
        println("${getIndentString()}ExprStatement")
        indent += 1
        statement.expression.accept(this)
        indent -= 1
    }

    override fun visit(statement: DeclareBlockItem) {
        println("${getIndentString()}DeclareStatement(name=${statement.name})")
        indent += 1
        statement.initializer.ifPresent { it.accept(this) }
        indent -= 1
    }

    override fun visit(statement: IfStatement) {
        println("${getIndentString()}IfStatement")
        indent += 1
        statement.expr.accept(this)
        println("${getIndentString()}then")
        statement.ifBranch.accept(this)
        statement.elseBranch.ifPresent {
            println("${getIndentString()}else")
            it.accept(this)
        }
        indent -= 1
    }

    override fun visit(statement: CompoundStatement) {
        println("${getIndentString()}CompoundStatement")
        indent += 1
        statement.blockItems.forEach { it.accept(this) }
        indent -= 1
    }

    override fun visit(statement: ForLoopStatement) {
        println("${getIndentString()}ForLoopStatement")
        indent += 1
        statement.init.accept(this)
        statement.condition.accept(this)
        statement.update.ifPresent { it.accept(this) }
        statement.body.accept(this)
        indent -= 1
    }

    override fun visit(statement: WhileLoopStatement) {
        println("${getIndentString()}WhileLoopStatement")
        indent += 1
        statement.condition.accept(this)
        statement.body.accept(this)
        indent -= 1
    }

    override fun visit(statement: DoWhileLoopStatement) {
        println("${getIndentString()}DoWhileLoopStatement")
        indent += 1
        statement.body.accept(this)
        statement.condition.accept(this)
        indent -= 1
    }

    override fun visit(statement: LoopControlStatement) {
        println("${getIndentString()}LoopControlStatement(type=${statement.type})")
    }

    override fun visit(statement: EmptyStatement) {
        println("${getIndentString()}EmptyStatement")
    }

    override fun visit(expr: BinOpExpr) {
        println("${getIndentString()}BinOpExpr")
        indent += 1
        expr.left.accept(this)
        expr.right.forEach { (expr, op) ->
            println("${getIndentString()}${op}")
            expr.accept(this)
        }
        indent -= 1
    }

    override fun visit(expr: ConstExpr) {
        println("${getIndentString()}ConstExpr(value=${expr.value})")
    }

    override fun visit(expr: AssignExpr) {
        println("${getIndentString()}AssignExpr(name=${expr.name}, op: ${expr.op})")
        indent += 1
        expr.expr.accept(this)
        indent -= 1
    }

    override fun visit(expr: VarExpr) {
        println("${getIndentString()}VarExpr(name=${expr.name})")
    }

    override fun visit(expr: IncrementExpr) {
        println("${getIndentString()}IncrementExpr(name=${expr.name}, isPostfix=${expr.isPostfix}, isDecrement=${expr.isDecrement})")
    }

    override fun visit(expr: ConditionalExpr) {
        println("${getIndentString()}ConditionalExpr")
        indent += 1
        expr.condition.accept(this)
        println("${getIndentString()}?")
        expr.trueBranch.accept(this)
        println("${getIndentString()}:")
        expr.falseBranch.accept(this)
        indent -= 1
    }

    override fun visit(expr: FunctionCallExpr) {
        println("${getIndentString()}FunctionCallExpr(name=${expr.name})")
        indent += 1
        expr.args.forEach { it.accept(this) }
        indent -= 1
    }

    override fun visit(expr: UnOpExpr) {
        println("${getIndentString()}UnaryOpExpr(operator=${expr.operator})")
        indent += 1
        expr.right.accept(this)
        indent -= 1
    }
}